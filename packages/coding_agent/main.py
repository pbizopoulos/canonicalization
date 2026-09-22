#!/usr/bin/env python3
# Copyright (c) 2026 VALAB/ITI
"""An interactive or single-prompt client for a local llama.cpp coding model."""

import argparse
import contextlib
import curses
import glob
import json
import os
import readline
import shlex
import signal
import subprocess
import sys
import tempfile
import urllib.error
import urllib.request
from pathlib import Path
from typing import TYPE_CHECKING, Any, cast

if TYPE_CHECKING:
    from collections.abc import Callable
BASE_URL = "http://127.0.0.1:8080"
OUTPUT_LIMIT = 16_000
BASH_TIMEOUT = 60
NIX_TIMEOUT = 600
HTTP_TIMEOUT = 300
MAX_REQUESTS = 20
README = Path(__file__).with_name("prm") / "README"
if not README.is_file():
    README = Path(__file__).resolve().parents[2] / "README"


def tool(name: str, description: str, **properties: str) -> dict[str, Any]:  # noqa: D103
    return {
        "type": "function",
        "function": {
            "name": name,
            "description": description,
            "parameters": {
                "type": "object",
                "properties": {
                    key: {"type": "string", "description": value}
                    for key, value in properties.items()
                },
                "required": list(properties),
                "additionalProperties": False,
            },
        },
    }


TOOLS = [
    tool("read", "Read a UTF-8 file.", path="File path"),
    tool(
        "write",
        "Write a UTF-8 file, replacing it; create parent directories.",
        path="File path",
        content="New contents",
    ),
    tool(
        "edit",
        "Replace exactly one literal occurrence; reject empty or ambiguous matches.",
        path="File path",
        old_text="Text to replace",
        new_text="Replacement",
    ),
    tool(
        "bash",
        "Run Bash in the startup directory with a 60-second timeout.",
        command="Bash command",
    ),
    tool(
        "nix",
        "Run Nix with flakes enabled in the startup directory; 600-second timeout. "
        "Supports build, run, develop, fmt, and flake subcommands. No shell expansion.",
        arguments="Arguments without nix, e.g. build .#package or flake check .",
    ),
]


def bounded(text: str) -> str:  # noqa: D103
    marker = "\n[output truncated]"
    return (
        text
        if len(text) <= OUTPUT_LIMIT
        else text[: OUTPUT_LIMIT - len(marker)] + marker
    )


class AgentError(Exception):  # noqa: D101
    pass


class Agent:  # noqa: D101
    def __init__(self, cwd: str | Path | None = None, base_url: str = BASE_URL) -> None:  # noqa: D107
        self.cwd = Path(cwd or Path.cwd()).resolve()
        self.base_url = base_url.rstrip("/")
        self.model: str | None = None
        self.output: Callable[[str], None] | None = None
        self.messages = [
            {
                "role": "system",
                "content": README.read_text(encoding="utf-8"),
            },
        ]

    def emit(self, text: str) -> None:  # noqa: D102
        if self.output is not None:
            self.output(text)

    def request(self, endpoint: str, body: dict[str, Any] | None = None) -> Any:  # noqa: ANN401, D102
        request = urllib.request.Request(  # noqa: S310
            self.base_url + endpoint,
            data=None if body is None else json.dumps(body).encode("utf-8"),
            headers={"Content-Type": "application/json"},
        )
        try:
            with urllib.request.urlopen(request, timeout=HTTP_TIMEOUT) as response:  # noqa: S310
                return json.load(response)
        except (urllib.error.URLError, OSError) as exc:
            msg = f"Connection/HTTP error: {exc}"
            raise AgentError(msg) from exc
        except (ValueError, UnicodeError) as exc:
            msg = f"Protocol error: invalid JSON: {exc}"
            raise AgentError(msg) from exc

    def discover(self) -> None:  # noqa: D102
        data = self.request("/v1/models")
        try:
            model = data["data"][0]["id"]
            if not isinstance(model, str) or not model:
                msg = "empty or invalid model ID"
                raise ValueError(msg)  # noqa: TRY301
        except (KeyError, IndexError, TypeError, ValueError) as exc:
            msg = "Protocol error: /v1/models has no valid model ID"
            raise AgentError(msg) from exc
        self.model = model

    def bash(self, command: str) -> str:  # noqa: D102
        return self.run(["bash", "-c", command], BASH_TIMEOUT)

    def run(self, command: list[str], timeout: int) -> str:  # noqa: D102
        with tempfile.TemporaryFile() as output:
            process = subprocess.Popen(  # noqa: S603
                command,
                cwd=self.cwd,
                stdin=subprocess.DEVNULL,
                stdout=output,
                stderr=subprocess.STDOUT,
                start_new_session=True,
            )
            timed_out = False
            cancelled = False
            try:
                process.wait(timeout=timeout)
            except subprocess.TimeoutExpired:
                timed_out = True
            except KeyboardInterrupt:
                cancelled = True
            finally:
                with contextlib.suppress(ProcessLookupError):
                    os.killpg(process.pid, signal.SIGKILL)
                process.wait()
            output.seek(0)
            captured = output.read(OUTPUT_LIMIT * 4 + 1).decode(
                "utf-8",
                errors="replace",
            )
            status = f"exit status: {process.returncode}\n"
            if timed_out:
                status = f"timed out after {timeout} seconds; " + status
            if self.output is not None:
                output.seek(0)
                self.output(status + output.read().decode("utf-8", errors="replace"))
            if cancelled:
                raise KeyboardInterrupt
            return bounded(status + captured)

    def execute(self, name: str, arguments: str) -> str:  # noqa: D102
        try:
            args = json.loads(arguments)
            definition = next(
                t["function"] for t in TOOLS if t["function"]["name"] == name
            )
            if not isinstance(args, dict) or set(args) != set(
                definition["parameters"]["required"],
            ):
                msg = "incorrect tool arguments"
                raise ValueError(msg)  # noqa: TRY301
            if not all(isinstance(value, str) for value in args.values()):
                msg = "tool arguments must be strings"
                raise ValueError(msg)  # noqa: TRY301
            if name == "bash":
                return self.bash(args["command"])
            if name == "nix":
                return self.run(
                    [
                        "nix",
                        "--extra-experimental-features",
                        "nix-command flakes",
                        *shlex.split(args["arguments"]),
                    ],
                    NIX_TIMEOUT,
                )
            path = self.cwd / args["path"]
            if name == "read":
                content = path.read_text(encoding="utf-8")
                self.emit(content)
                return bounded(content)
            if name == "write":
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text(args["content"], encoding="utf-8")
            else:
                old = args["old_text"]
                content = path.read_text(encoding="utf-8")
                first = content.find(old)
                if not old or first < 0 or content.find(old, first + 1) >= 0:
                    msg = "old_text must match exactly once and must not be empty"
                    raise ValueError(msg)  # noqa: TRY301
                path.write_text(
                    content.replace(old, args["new_text"], 1),
                    encoding="utf-8",
                )
            return "OK"  # noqa: TRY300
        except (OSError, ValueError, TypeError, StopIteration) as exc:
            return bounded(f"Tool error ({name}): {exc}")

    def completion(self) -> dict[str, Any]:  # noqa: D102
        data = self.request(
            "/v1/chat/completions",
            {
                "model": self.model,
                "messages": self.messages,
                "tools": TOOLS,
                "stream": False,
            },
        )
        try:
            message = data["choices"][0]["message"]
            if message["role"] != "assistant":
                msg = "expected assistant role"
                raise ValueError(msg)  # noqa: TRY301
            content = message.get("content")
            calls = message.get("tool_calls", [])
            if content is not None and not isinstance(content, str):
                msg = "invalid content"
                raise ValueError(msg)  # noqa: TRY301
            if not isinstance(calls, list) or (not calls and content is None):
                msg = "missing content or tool calls"
                raise ValueError(msg)  # noqa: TRY301
            ids = set()
            for call in calls:
                if (
                    call["type"] != "function"
                    or not isinstance(call["id"], str)
                    or not call["id"]
                    or call["id"] in ids
                    or not isinstance(call["function"]["name"], str)
                    or not isinstance(call["function"]["arguments"], str)
                ):
                    msg = "invalid tool call"
                    raise ValueError(msg)  # noqa: TRY301
                ids.add(call["id"])
            return {  # noqa: TRY300
                "role": "assistant",
                "content": content,
                **({"tool_calls": calls} if calls else {}),
            }
        except (KeyError, IndexError, TypeError, ValueError) as exc:
            msg = f"Protocol error: invalid assistant response: {exc}"
            raise AgentError(msg) from exc

    def turn(self, prompt: str) -> str:  # noqa: D102
        if self.model is None:
            self.discover()
        start = len(self.messages)
        self.messages.append({"role": "user", "content": prompt})
        try:
            for _ in range(MAX_REQUESTS):
                message = self.completion()
                self.messages.append(message)
                if message.get("content"):
                    self.emit("assistant> " + message["content"])
                if not message.get("tool_calls"):
                    return cast("str", message["content"])
                for call in message["tool_calls"]:
                    self.emit(
                        f"tool> {call['function']['name']} "
                        f"{call['function']['arguments']}",
                    )
                    result = self.execute(
                        call["function"]["name"],
                        call["function"]["arguments"],
                    )
                    if call["function"]["name"] not in {
                        "bash",
                        "nix",
                        "read",
                    } or result.startswith("Tool error"):
                        self.emit(result)
                    self.messages.append(
                        {"role": "tool", "tool_call_id": call["id"], "content": result},
                    )
            msg = f"Stopped after {MAX_REQUESTS} model requests"
            raise AgentError(msg)  # noqa: TRY301
        except (AgentError, KeyboardInterrupt):
            del self.messages[start:]
            raise


def configure_filename_completion() -> None:  # noqa: D103
    readline.set_completer(None)
    readline.set_completer_delims(" \t\n\"'`@$><=;|&{(")
    readline.parse_and_bind("tab: complete")


class Viewer:  # noqa: D101
    def __init__(self, screen: Any, agent: Agent) -> None:  # noqa: ANN401, D107
        self.screen = screen
        self.agent = agent
        self.lines: list[str] = []
        self.top = 0
        self.mode = "view"
        self.draft = ""
        self.cursor = 0
        self.history: list[str] = []
        self.history_index = 0
        self.saved_draft = ""
        self.entry = ""
        self.query = ""
        self.direction = 1
        self.match: int | None = None
        self.notice = ""
        self.follow = True

    def append(self, text: str) -> None:  # noqa: D102
        self.lines.extend(
            "".join(char if char.isprintable() else repr(char)[1:-1] for char in line)
            for line in text.expandtabs(8).splitlines()
        )
        self.draw()

    def rows(self) -> list[str]:  # noqa: D102
        width = max(1, self.screen.getmaxyx()[1] - 1)
        return [
            line[start : start + width]
            for line in self.lines
            for start in range(0, max(1, len(line)), width)
        ]

    def draw(self) -> None:  # noqa: D102
        height, width = self.screen.getmaxyx()
        page = max(1, height - 2)
        rows = self.rows()
        end = max(0, len(rows) - page)
        self.top = end if self.follow else min(self.top, end)
        self.screen.erase()
        for y, line in enumerate(rows[self.top : self.top + page]):
            with contextlib.suppress(curses.error):
                self.screen.addnstr(
                    y,
                    0,
                    line,
                    max(0, width - 1),
                    curses.A_REVERSE
                    if self.query and self.query in line
                    else curses.A_NORMAL,
                )
        status = (
            f"{self.mode.upper()}  {self.top + 1}/{max(1, len(rows))}  "
            ": chat  Esc view  / ? search  n N repeat  j k scroll  g G ends  q quit"
        )
        prompt = (
            "> " + self.draft
            if self.mode == "chat"
            else self.mode + self.entry
            if self.mode in {"/", "?"}
            else self.notice
        )
        position = self.cursor + (2 if self.mode == "chat" else 1)
        offset = max(0, position - max(1, width - 2)) if self.mode != "view" else 0
        with contextlib.suppress(curses.error):
            self.screen.addnstr(
                max(0, height - 2),
                0,
                status,
                max(0, width - 1),
                curses.A_REVERSE,
            )
            self.screen.addnstr(
                max(0, height - 1),
                0,
                prompt[offset:],
                max(0, width - 1),
            )
            if self.mode != "view":
                self.screen.move(
                    max(0, height - 1),
                    min(max(0, width - 1), position - offset),
                )
            curses.curs_set(int(self.mode != "view"))
        self.screen.refresh()

    def search(self, direction: int) -> None:  # noqa: D102
        rows = self.rows()
        if not self.query or not rows:
            return
        origin = self.top if self.match is None else self.match
        for step in range(1, len(rows) + 1):
            index = (origin + direction * step) % len(rows)
            if self.query in rows[index]:
                self.top = index
                self.match = index
                self.follow = False
                self.notice = f"Search: {self.query}"
                return
        self.notice = f"Pattern not found: {self.query}"

    def submit(self) -> None:  # noqa: D102
        prompt = self.draft
        if not prompt.strip():
            return
        self.draft = ""
        self.cursor = 0
        self.history.append(prompt)
        self.history_index = len(self.history)
        self.follow = True
        self.append("> " + prompt)
        self.notice = "Working..."
        self.draw()
        try:
            self.agent.turn(prompt)
        except (AgentError, KeyboardInterrupt) as exc:
            self.append(
                f"{str(exc) or 'Cancelled'}. Completed tool effects remain; "
                "incomplete turn history discarded.",
            )
        finally:
            self.notice = ""

    def edit(self, key: str | int) -> None:  # noqa: C901, D102, PLR0912
        value = self.draft if self.mode == "chat" else self.entry
        if key == curses.KEY_LEFT:
            self.cursor = max(0, self.cursor - 1)
        elif key == curses.KEY_RIGHT:
            self.cursor = min(len(value), self.cursor + 1)
        elif key in {curses.KEY_HOME, "\x01"}:
            self.cursor = 0
        elif key in {curses.KEY_END, "\x05"}:
            self.cursor = len(value)
        elif key in {curses.KEY_UP, curses.KEY_DOWN} and self.mode == "chat":
            if self.history_index == len(self.history):
                self.saved_draft = value
            self.history_index = min(
                len(self.history),
                max(0, self.history_index + (-1 if key == curses.KEY_UP else 1)),
            )
            value = (
                self.saved_draft
                if self.history_index == len(self.history)
                else self.history[self.history_index]
            )
            self.cursor = len(value)
        elif key in {"\x7f", "\b", curses.KEY_BACKSPACE}:
            if self.cursor:
                value = value[: self.cursor - 1] + value[self.cursor :]
                self.cursor -= 1
        elif key == curses.KEY_DC:
            value = value[: self.cursor] + value[self.cursor + 1 :]
        elif key == "\x15":
            value = value[self.cursor :]
            self.cursor = 0
        elif key == "\t" and self.mode == "chat":
            start = self.cursor
            while start and not value[start - 1].isspace():
                start -= 1
            prefix = value[start : self.cursor]
            matches = sorted(glob.glob(glob.escape(os.path.expanduser(prefix)) + "*"))  # noqa: PTH111, PTH207
            if matches:
                replacement = os.path.commonprefix(matches)
                if len(matches) == 1 and Path(replacement).is_dir():
                    replacement += "/"
                value = value[:start] + replacement + value[self.cursor :]
                self.cursor = start + len(replacement)
        elif isinstance(key, str) and key.isprintable():
            value = value[: self.cursor] + key + value[self.cursor :]
            self.cursor += len(key)
        if self.mode == "chat":
            self.draft = value
        else:
            self.entry = value

    def key(self, key: str | int) -> bool:  # noqa: C901, D102, PLR0912
        if key == "\x1b":
            self.mode = "view"
            return True
        if self.mode != "view":
            if key in {"\n", "\r", curses.KEY_ENTER}:
                if self.mode == "chat":
                    self.submit()
                else:
                    self.direction = 1 if self.mode == "/" else -1
                    self.query = self.entry or self.query
                    self.match = None
                    self.mode = "view"
                    self.search(self.direction)
            else:
                self.edit(key)
            return True
        page = max(1, self.screen.getmaxyx()[0] - 2)
        if key in {"q", "\x04"}:
            return False
        if key == ":":
            self.mode = "chat"
            self.cursor = len(self.draft)
        elif key in {"/", "?"}:
            self.mode = str(key)
            self.entry = ""
            self.cursor = 0
        elif key in {"n", "N"}:
            self.search(self.direction * (1 if key == "n" else -1))
        elif key == "G":
            self.follow = True
        elif key in {"g", curses.KEY_HOME}:
            self.top = 0
            self.follow = False
        else:
            movement = {
                "j": 1,
                curses.KEY_DOWN: 1,
                "k": -1,
                curses.KEY_UP: -1,
                " ": page,
                "f": page,
                curses.KEY_NPAGE: page,
                "b": -page,
                curses.KEY_PPAGE: -page,
            }.get(key, 0)
            if movement:
                self.top = max(0, self.top + movement)
                self.follow = False
        return True

    def run(self) -> None:  # noqa: D102
        self.screen.keypad(True)  # noqa: FBT003
        previous = self.agent.output
        self.agent.output = self.append
        try:
            while True:
                self.draw()
                try:
                    key = self.screen.get_wch()
                except KeyboardInterrupt:
                    self.mode = "view"
                    continue
                if not self.key(key):
                    return
        finally:
            self.agent.output = previous


def main(argv: list[str] | None = None) -> None:  # noqa: C901, D103
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "-p",
        "--prompt",
        help="Run a single prompt non-interactively and exit",
    )
    args = parser.parse_args(argv)
    if args.prompt is not None and not args.prompt.strip():
        parser.error("--prompt must not be empty or whitespace")
    agent = Agent()
    if args.prompt is not None:
        try:
            print(agent.turn(args.prompt))  # noqa: T201
        except AgentError as exc:
            print(f"Error: {exc}", file=sys.stderr)  # noqa: T201
            raise SystemExit(1) from exc
        except KeyboardInterrupt as exc:
            print("\nCancelled. Completed tool effects remain.", file=sys.stderr)  # noqa: T201
            raise SystemExit(130) from exc
        return
    if sys.stdin.isatty() and sys.stdout.isatty():
        curses.set_escdelay(25)
        curses.wrapper(lambda screen: Viewer(screen, agent).run())
        return
    configure_filename_completion()
    while True:
        try:
            prompt = input("> ")
            if prompt.strip():
                print(agent.turn(prompt))  # noqa: T201
        except EOFError:  # noqa: PERF203
            print()  # noqa: T201
            return
        except KeyboardInterrupt:
            print(  # noqa: T201
                "\nCancelled. Completed tool effects remain; "
                "incomplete turn history discarded.",
            )
        except AgentError as exc:
            print(  # noqa: T201
                f"Error: {exc}. Completed tool effects remain; "
                "incomplete turn history discarded.",
            )


if __name__ == "__main__":
    main()
