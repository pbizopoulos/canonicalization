#!/usr/bin/env python3
# Copyright (c) 2026- Paschalis Bizopoulos
"""An interactive or single-prompt client for a local llama.cpp coding model."""

import argparse
import contextlib
import ctypes
import json
import os
import readline
import select
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
    def __init__(self, agent: Agent) -> None:  # noqa: D107
        self.agent = agent
        self.lines: list[str] = []

    def append(self, text: str, *, display: bool = True) -> None:  # noqa: D102
        self.lines.append(text)
        if display:
            print(text, flush=True)  # noqa: T201

    def view(self) -> None:  # noqa: D102
        with tempfile.NamedTemporaryFile(mode="w+", encoding="utf-8") as transcript:
            transcript.write("\n".join(self.lines) + "\n")
            transcript.flush()
            try:
                subprocess.run(  # noqa: S603
                    ["less", "--", transcript.name],  # noqa: S607
                    check=False,
                )
            except OSError as exc:
                print(f"Viewer error: {exc}", flush=True)  # noqa: T201

    def read_chat(self) -> str:  # noqa: D102
        library = ctypes.CDLL(readline.__file__)
        getter_type = ctypes.CFUNCTYPE(ctypes.c_int, ctypes.c_void_p)
        slot = ctypes.c_void_p.in_dll(library, "rl_getc_function")
        previous = slot.value
        if previous is None:
            msg = "GNU readline has no character input function"
            raise RuntimeError(msg)
        original = getter_type(previous)
        library.rl_get_keymap.restype = ctypes.c_void_p
        library.rl_get_keymap_by_name.argtypes = [ctypes.c_char_p]
        library.rl_get_keymap_by_name.restype = ctypes.c_void_p
        vi_insert = library.rl_get_keymap_by_name(b"vi-insert")
        failure: BaseException | None = None

        def get_character(stream: int) -> int:
            nonlocal failure
            try:
                char = original(stream)
                if char != ord("\x1b") or library.rl_get_keymap() == vi_insert:
                    return int(char)
                if select.select([sys.stdin], [], [], 0.15)[0]:
                    return int(char)
                library.rl_deprep_terminal()
                try:
                    self.view()
                finally:
                    library.rl_prep_terminal(1)
                    library.rl_on_new_line()
                    library.rl_forced_update_display()
            except BaseException as exc:  # noqa: BLE001
                failure = exc
                ctypes.c_int.in_dll(library, "rl_done").value = 1
                return ord("\n")
            return 0

        callback = getter_type(get_character)
        slot.value = ctypes.cast(callback, ctypes.c_void_p).value
        history_length = readline.get_current_history_length()
        try:
            prompt = input("> ")
            if failure is not None:
                if readline.get_current_history_length() > history_length:
                    readline.remove_history_item(
                        readline.get_current_history_length() - 1,
                    )
                raise failure
            return prompt
        finally:
            slot.value = previous

    def run(self) -> None:  # noqa: D102
        configure_filename_completion()
        previous = self.agent.output
        self.agent.output = self.append
        try:
            while True:
                try:
                    prompt = self.read_chat()
                    if prompt.strip():
                        self.append("> " + prompt, display=False)
                        self.agent.turn(prompt)
                except EOFError:  # noqa: PERF203
                    print()  # noqa: T201
                    return
                except (AgentError, KeyboardInterrupt) as exc:
                    self.append(
                        f"\n{str(exc) or 'Cancelled'}. Completed tool effects remain; "
                        "incomplete turn history discarded.",
                    )
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
        Viewer(agent).run()
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
