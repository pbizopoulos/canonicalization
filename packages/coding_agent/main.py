#!/usr/bin/env python3
# Copyright (c) 2026- Paschalis Bizopoulos
"""An interactive or single-prompt client for a local llama.cpp coding model."""

import argparse
import contextlib
import ctypes
import curses
import json
import os
import re
import readline
import select
import shlex
import signal
import subprocess
import sys
import tempfile
import unicodedata
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
        self.mode = "chat"
        self.draft = ""
        self.cursor = 0
        self.entry = ""
        self.query = ""
        self.direction = 1
        self.match: int | None = None
        self.notice = ""
        self.follow = False
        self.pattern: re.Pattern[str] | None = None
        self.highlight = True
        self.wrap_search = False
        self.literal_search = False
        self.keep_search = False
        self.search_history: list[str] = []
        self.search_history_index = 0
        self.number = ""
        self.pending = ""
        self.search_count = 1
        self.search_modifiers: dict[str, bool] = {}
        self.half_window = 0
        self.window = 0
        self.horizontal = 0
        self.horizontal_step = 0
        self.chop = False
        self.ignore_case = ""
        self.marks: dict[str, int] = {}
        self.previous_top = 0
        self.help_offset: int | None = None
        self.invert_search = False
        self.displayed_line = 0
        self.displayed_offset = 0

    def append(self, text: str, *, display: bool = True) -> None:  # noqa: D102
        self.lines.extend(
            "".join(char if char.isprintable() else repr(char)[1:-1] for char in line)
            for line in text.expandtabs(8).splitlines()
        )
        if self.mode == "chat":
            if display:
                print(text, flush=True)  # noqa: T201
        else:
            self.draw()

    def page_size(self) -> int:  # noqa: D102
        return max(1, int(self.screen.getmaxyx()[0]) - 1)

    @staticmethod
    def cell_width(text: str) -> int:  # noqa: D102
        return sum(
            0
            if unicodedata.combining(char)
            else 2
            if unicodedata.east_asian_width(char) in {"W", "F"}
            else 1
            for char in text
        )

    def layout(self) -> list[tuple[int, int, str]]:  # noqa: D102
        width = max(1, self.screen.getmaxyx()[1])
        rows = []
        for number, line in enumerate(self.lines):
            start = 0
            column = 0
            for index, char in enumerate(line):
                size = self.cell_width(char)
                if self.horizontal and column < self.horizontal:
                    column += size
                    start = index + 1
                    continue
                if column + size > self.horizontal + width:
                    rows.append((number, start, line[start:index]))
                    if self.chop or self.horizontal:
                        break
                    start, column = index, 0
                column += size
            else:
                rows.append((number, start, line[start:]))
        return rows

    def rows(self) -> list[str]:  # noqa: D102
        return [text for _, _, text in self.layout()]

    def draw(self) -> None:  # noqa: C901, D102
        if self.mode == "chat":
            return
        if self.help_offset is not None:
            self.draw_help()
            return
        height, width = self.screen.getmaxyx()
        page = self.page_size()
        rows = self.layout()
        if self.follow:
            self.top = max(0, len(rows) - page)
        self.top = min(self.top, max(0, len(rows) - 1))
        if rows:
            self.displayed_line, self.displayed_offset, _ = rows[self.top]
        self.screen.erase()
        for y in range(page):
            if self.top + y >= len(rows):
                with contextlib.suppress(curses.error):
                    self.screen.addstr(y, 0, "~")
                continue
            number, start, text = rows[self.top + y]
            with contextlib.suppress(curses.error):
                self.screen.addstr(y, 0, text, curses.A_NORMAL)
            if self.pattern is not None and self.highlight:
                for match in self.pattern.finditer(self.lines[number]):
                    left = max(start, match.start())
                    right = min(start + len(text), match.end())
                    if left < right:
                        with contextlib.suppress(curses.error):
                            self.screen.addstr(
                                y,
                                self.cell_width(self.lines[number][start:left]),
                                self.lines[number][left:right],
                                curses.A_REVERSE,
                            )
        status = self.notice or (
            "Waiting for data... (Ctrl+C to interrupt)"
            if self.follow
            else "(END)"
            if self.top + page >= len(rows)
            else ":"
        )
        search_prefix = self.mode + "".join(
            label + " "
            for attribute, label in (
                ("wrap_search", "WRAP"),
                ("literal_search", "LITERAL"),
                ("keep_search", "KEEP"),
                ("invert_search", "NOT"),
            )
            if self.search_modifiers.get(attribute)
        )
        prompt = (
            "> " + self.draft
            if self.mode == "chat"
            else search_prefix + self.entry
            if self.mode in {"/", "?"}
            else self.number
            or ("ESC" if self.pending == "\x1b" else self.pending)
            or status
        )
        position = self.cursor + (2 if self.mode == "chat" else len(search_prefix))
        offset = max(0, position - max(1, width - 2)) if self.mode != "view" else 0
        with contextlib.suppress(curses.error):
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

    def draw_help(self) -> None:  # noqa: D102
        lines = [
            "VIEWER COMMANDS (q returns to transcript)",
            "Commands accept a numeric prefix N where appropriate.",
            "j e Enter Down Ctrl+N Ctrl+E  Forward N lines",
            "k y Up Ctrl+P Ctrl+Y         Backward N lines",
            "Space f PgDn Ctrl+F Ctrl+V   Forward N lines (default a page)",
            "b PgUp Ctrl+B Esc-v          Backward N lines (default a page)",
            "d Ctrl+D / u Ctrl+U          Forward / backward half a page",
            "z / w                       Like f / b; N sets page size",
            "g < Home / G > End           First / last line, or line N",
            "N% or Np                    Jump to N percent",
            "F                           Follow output; Ctrl+C stops",
            "Left / Right                Scroll horizontally",
            "/pattern / ?pattern         Forward / backward regex search",
            "n / N                       Repeat search / reverse direction",
            "Search prefixes: Ctrl+W wrap; Ctrl+R literal; Ctrl+K highlight only",
            "Search prefix: Ctrl+N or !   Find nonmatching lines",
            "Esc-u / Esc-U               Toggle highlights / clear search",
            "-i / -I                     Smart / unconditional ignore case",
            "-S                          Toggle long-line chopping",
            "ma / 'a / ''                Set mark a / jump to a / previous jump",
            "r Ctrl+L Ctrl+R              Redraw",
            "= Ctrl+G                    Transcript position",
            "q Q ZZ                      Close viewer and return to chat",
            ":                           Enter chat (application extension)",
            "Esc (twice in vi insert)    View transcript; keep unfinished draft",
            "Ctrl+D (empty chat prompt)   Exit coding_agent",
        ]
        height, width = self.screen.getmaxyx()
        self.screen.erase()
        for row, text in enumerate(lines[self.help_offset or 0 :][: self.page_size()]):
            with contextlib.suppress(curses.error):
                self.screen.addnstr(row, 0, text, max(0, width - 1))
        with contextlib.suppress(curses.error):
            self.screen.addnstr(
                height - 1,
                0,
                "HELP -- Space: next, b: back, q: close",
                max(0, width - 1),
            )
            curses.curs_set(0)
        self.screen.refresh()

    def compile_search(self) -> bool:  # noqa: D102
        flags = (
            re.IGNORECASE
            if (
                self.ignore_case == "I"
                or (
                    self.ignore_case == "i" and not any(c.isupper() for c in self.query)
                )
            )
            else 0
        )
        try:
            self.pattern = re.compile(
                re.escape(self.query) if self.literal_search else self.query,
                flags,
            )
        except re.error as exc:
            self.notice = f"Invalid pattern: {exc}"
            return False
        return True

    def search(self, direction: int, count: int = 1, *, initial: bool = False) -> None:  # noqa: D102
        if not self.query:
            self.notice = "No previous regular expression"
            return
        if not self.compile_search():
            return
        self.highlight = True
        rows = self.layout()
        if not rows or (self.keep_search and initial):
            return
        anchor = min(self.top, len(rows) - 1)
        if initial and direction < 0:
            anchor = min(anchor + self.page_size() - 1, len(rows) - 1)
        origin = rows[anchor][0]
        start = origin if initial else origin + direction
        candidates = list(
            range(start, len(self.lines) if direction > 0 else -1, direction),
        )
        if self.wrap_search:
            candidates += list(
                range(0 if direction > 0 else len(self.lines) - 1, start, direction),
            )
        for number in candidates:
            match = self.pattern.search(self.lines[number]) if self.pattern else None
            if (match is not None) == self.invert_search:
                continue
            count -= 1
            if count:
                continue
            self.previous_top = self.top
            self.top = next(
                index
                for index, (line, offset, text) in enumerate(rows)
                if line == number
                and (
                    self.chop
                    or self.horizontal
                    or (
                        offset
                        <= min(match.start(), max(0, len(self.lines[number]) - 1))
                        < offset + max(1, len(text))
                        if match
                        else offset == 0
                    )
                )
            )
            self.match = number
            self.follow = False
            self.notice = ""
            return
        self.notice = "Pattern not found"

    def submit(self) -> None:  # noqa: D102
        prompt = self.draft
        if not prompt.strip():
            return
        self.draft = ""
        self.cursor = 0
        self.follow = True
        self.append("> " + prompt, display=False)
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
        value = self.entry
        if key == curses.KEY_LEFT:
            self.cursor = max(0, self.cursor - 1)
        elif key == curses.KEY_RIGHT:
            self.cursor = min(len(value), self.cursor + 1)
        elif key in {curses.KEY_HOME, "\x01"}:
            self.cursor = 0
        elif key in {curses.KEY_END, "\x05"}:
            self.cursor = len(value)
        elif key in {"\x7f", "\b", curses.KEY_BACKSPACE}:
            if self.cursor:
                value = value[: self.cursor - 1] + value[self.cursor :]
                self.cursor -= 1
        elif key in {curses.KEY_DC, "\x04"}:
            value = value[: self.cursor] + value[self.cursor + 1 :]
        elif key == "\x15":
            value = value[self.cursor :]
            self.cursor = 0
        elif key == "\x0b":
            value = value[: self.cursor]
        elif key == "\x17":
            start = self.cursor
            while start and value[start - 1].isspace():
                start -= 1
            while start and not value[start - 1].isspace():
                start -= 1
            value = value[:start] + value[self.cursor :]
            self.cursor = start
        elif isinstance(key, str) and key.isprintable():
            value = value[: self.cursor] + key + value[self.cursor :]
            self.cursor += len(key)
        self.entry = value

    def move(self, top: int) -> None:  # noqa: D102
        self.follow = False
        end = max(0, len(self.rows()) - self.page_size())
        limit = max(self.top, end) if top > self.top else max(0, len(self.rows()) - 1)
        self.top = max(0, min(top, limit))
        self.match = None

    def prefix_key(self, key: str | int, count: int) -> bool:  # noqa: C901, D102, PLR0912
        prefix, self.pending = self.pending, ""
        if prefix == "\x1b":
            if key == "u":
                self.highlight = not self.highlight
            elif key == "U":
                self.query = ""
                self.pattern = None
                self.match = None
            elif key == "v":
                self.move(self.top - (count or self.window or self.page_size()))
            elif key in {"<", ">"}:
                self.key("g" if key == "<" else "G")
            else:
                return self.key(key)
        elif prefix == "-":
            if key in {"i", "I"}:
                self.ignore_case = "" if self.ignore_case == key else str(key)
                if self.query:
                    self.compile_search()
                self.notice = (
                    "Case-sensitive search"
                    if not self.ignore_case
                    else "Ignore case in searches"
                )
            elif key == "S":
                self.chop = not self.chop
            else:
                self.notice = "Supported options: -i -I -S"
        elif prefix == "m" and isinstance(key, str) and key.isalpha():
            rows = self.layout()
            self.marks[key] = rows[min(self.top, len(rows) - 1)][0] if rows else 0
        elif prefix == "'":
            if key == "'":
                self.top, self.previous_top = self.previous_top, self.top
                self.follow = False
            elif key in self.marks:
                self.previous_top = self.top
                self.top = next(
                    (
                        i
                        for i, row in enumerate(self.layout())
                        if row[0] == self.marks[str(key)]
                    ),
                    0,
                )
                self.follow = False
            else:
                self.notice = "Mark not set"
        elif prefix == "Z" and key == "Z":
            self.enter_chat()
        return True

    def enter_chat(self) -> None:  # noqa: D102
        self.mode = "chat"
        self.cursor = len(self.draft)
        self.pending = self.number = self.notice = ""

    def key(self, key: str | int) -> bool:  # noqa: C901, D102, PLR0911, PLR0912, PLR0915
        if self.help_offset is not None:
            if key in {"q", "Q", "\x1b"}:
                self.help_offset = None
            elif key in {" ", "f", curses.KEY_NPAGE}:
                self.help_offset = min(25, self.help_offset + self.page_size())
            elif key in {"b", curses.KEY_PPAGE}:
                self.help_offset = max(0, self.help_offset - self.page_size())
            return True
        if key == "\x03":
            self.mode = "view"
            self.follow = False
            self.pending = self.number = ""
            return True
        if key == curses.KEY_RESIZE:
            self.top = next(
                (
                    index
                    for index, (line, offset, text) in enumerate(self.layout())
                    if line == self.displayed_line
                    and (
                        self.chop
                        or self.horizontal
                        or offset <= self.displayed_offset < offset + max(1, len(text))
                    )
                ),
                self.top,
            )
            return True
        if key == "\x07" and self.mode != "view":
            self.mode = "view"
            return True
        if key == "\x1b":
            self.pending = "\x1b" if self.mode == "view" else ""
            if self.mode != "view":
                self.number = ""
            self.mode = "view"
            return True
        if self.mode == "chat":
            return True
        if self.mode != "view":
            if key in {"\n", "\r", curses.KEY_ENTER}:
                self.direction = 1 if self.mode == "/" else -1
                for attribute in (
                    "wrap_search",
                    "literal_search",
                    "keep_search",
                    "invert_search",
                ):
                    if self.entry or attribute in self.search_modifiers:
                        setattr(
                            self,
                            attribute,
                            self.search_modifiers.get(attribute, False),
                        )
                self.query = self.entry or self.query
                if self.entry:
                    self.search_history.append(self.entry)
                self.mode = "view"
                self.search(self.direction, self.search_count, initial=True)
            elif (
                self.mode in {"/", "?"}
                and key in {"\x17", "\x12", "\x0b", "\x0e", "!"}
                and not self.entry
            ):
                attribute = {
                    "\x17": "wrap_search",
                    "\x12": "literal_search",
                    "\x0b": "keep_search",
                    "\x0e": "invert_search",
                    "!": "invert_search",
                }[str(key)]
                self.search_modifiers[attribute] = not self.search_modifiers.get(
                    attribute,
                    False,
                )
            elif self.mode in {"/", "?"} and key in {curses.KEY_UP, curses.KEY_DOWN}:
                self.search_history_index = min(
                    len(self.search_history),
                    max(
                        0,
                        self.search_history_index + (-1 if key == curses.KEY_UP else 1),
                    ),
                )
                self.entry = (
                    self.search_history[self.search_history_index]
                    if self.search_history_index < len(self.search_history)
                    else ""
                )
                self.cursor = len(self.entry)
            elif (
                self.mode in {"/", "?"}
                and not self.entry
                and key in {"\b", "\x7f", curses.KEY_BACKSPACE}
            ):
                self.mode = "view"
            else:
                self.edit(key)
            return True
        if (
            isinstance(key, str)
            and key.isascii()
            and key.isdigit()
            and not self.pending
        ):
            self.number = (self.number + key)[:9]
            return True
        count = int(self.number) if self.number else 0
        self.number = ""
        self.notice = ""
        if self.pending:
            return self.prefix_key(key, count)
        if key in {"-", "m", "'", "Z"}:
            self.pending = str(key)
        elif key in {":", "q", "Q"}:
            self.enter_chat()
        elif key in {"/", "?"}:
            self.mode = str(key)
            self.entry = ""
            self.cursor = 0
            self.search_count = count or 1
            self.search_history_index = len(self.search_history)
            self.search_modifiers = {}
        elif key in {"n", "N"}:
            self.search(self.direction * (1 if key == "n" else -1), count or 1)
        elif key in {"g", "<", curses.KEY_HOME, "G", ">", curses.KEY_END, "F"}:
            self.previous_top = self.top
            if count:
                self.top = next(
                    (i for i, row in enumerate(self.layout()) if row[0] >= count - 1),
                    max(0, len(self.rows()) - 1),
                )
                self.follow = False
            else:
                self.top = (
                    0
                    if key in {"g", "<", curses.KEY_HOME}
                    else max(0, len(self.rows()) - self.page_size())
                )
                self.follow = key == "F"
        elif key in {"p", "%"}:
            self.previous_top = self.top
            self.move((len(self.rows()) - 1) * min(count, 100) // 100)
        elif key in {curses.KEY_RIGHT, curses.KEY_LEFT}:
            rows = self.layout()
            line = rows[min(self.top, len(rows) - 1)][0] if rows else 0
            self.horizontal_step = (
                count or self.horizontal_step or max(1, self.screen.getmaxyx()[1] // 2)
            )
            self.horizontal = max(
                0,
                self.horizontal
                + (1 if key == curses.KEY_RIGHT else -1) * self.horizontal_step,
            )
            self.top = next(
                (i for i, row in enumerate(self.layout()) if row[0] == line),
                0,
            )
            self.follow = False
        elif key in {"d", "\x04", "u", "\x15"}:
            self.half_window = (
                count or self.half_window or max(1, self.page_size() // 2)
            )
            self.move(self.top + (1 if key in {"d", "\x04"} else -1) * self.half_window)
        elif key in {
            " ",
            "f",
            "\x06",
            "\x16",
            curses.KEY_NPAGE,
            "b",
            "\x02",
            curses.KEY_PPAGE,
            "z",
            "w",
        }:
            if key in {"z", "w"} and count:
                self.window = count
            direction = -1 if key in {"b", "\x02", curses.KEY_PPAGE, "w"} else 1
            self.move(self.top + direction * (count or self.window or self.page_size()))
        elif key in {
            "j",
            "e",
            "\n",
            "\r",
            "\x0e",
            "\x05",
            curses.KEY_ENTER,
            curses.KEY_DOWN,
            "k",
            "y",
            "\x19",
            "\x10",
            "\x0b",
            curses.KEY_UP,
        }:
            direction = (
                -1 if key in {"k", "y", "\x19", "\x10", "\x0b", curses.KEY_UP} else 1
            )
            self.move(self.top + direction * (count or 1))
        elif key in {"=", "\x07"}:
            self.notice = (
                f"Transcript: {len(self.lines)} lines; "
                f"screen row {self.top + 1}/{len(self.rows())}"
            )
        elif key in {"h", "H"}:
            self.help_offset = 0
        elif key not in {"r", "R", "\x12", "\x0c", curses.KEY_RESIZE}:
            self.notice = "Unknown command (press h for help)"
        return True

    def view(self) -> None:  # noqa: D102
        self.mode = "view"
        curses.reset_prog_mode()
        self.screen.touchwin()
        try:
            while self.mode != "chat":
                self.draw()
                try:
                    self.key(self.screen.get_wch())
                except KeyboardInterrupt:
                    self.follow = False
                    self.pending = self.number = ""
        finally:
            curses.endwin()
            self.mode = "chat"

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
                while True:
                    char = original(stream)
                    if char != ord("\x1b") or library.rl_get_keymap() == vi_insert:
                        return int(char)
                    if select.select([sys.stdin], [], [], 0.15)[0]:
                        return int(char)
                    library.rl_deprep_terminal()
                    try:
                        self.draft = readline.get_line_buffer()
                        self.view()
                    finally:
                        library.rl_prep_terminal(1)
                        library.rl_redisplay()
            except BaseException as exc:  # noqa: BLE001
                failure = exc
                ctypes.c_int.in_dll(library, "rl_done").value = 1
                return ord("\n")

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
        self.screen.keypad(True)  # noqa: FBT003
        configure_filename_completion()
        curses.endwin()
        previous = self.agent.output
        self.agent.output = self.append
        try:
            while True:
                try:
                    self.draft = self.read_chat()
                    self.submit()
                except EOFError:  # noqa: PERF203
                    print()  # noqa: T201
                    return
                except KeyboardInterrupt:
                    self.draft = ""
                    print("\nCancelled.", flush=True)  # noqa: T201
        finally:
            self.agent.output = previous
            curses.reset_prog_mode()
            self.screen.refresh()


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
