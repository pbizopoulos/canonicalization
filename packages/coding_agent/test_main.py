# Copyright (c) 2026 VALAB/ITI
"""Verify tool execution, model protocol, and the interactive entry point."""

import contextlib
import ctypes
import curses
import fcntl
import io
import json
import os
import pty
import readline
import select
import shutil
import signal
import struct
import subprocess
import sys
import tempfile
import termios
import threading
import time
import unicodedata
import unittest
from collections.abc import Iterator
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from typing import Any
from unittest.mock import MagicMock, patch

import pytest

from packages.coding_agent import main as app


@pytest.fixture(autouse=True)
def isolated_history(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    """Never read or modify the developer's real history."""
    monkeypatch.setenv("XDG_STATE_HOME", str(tmp_path / "state"))


class TestHistory(unittest.TestCase):
    """Exercise persistent histories through restarts and failure paths."""

    def setUp(self) -> None:  # noqa: D102
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.cwd = Path(self.directory.name)

    def test_xdg_paths_and_directory_identity(self) -> None:  # noqa: D102
        alias = self.cwd / "alias"
        alias.symlink_to(self.cwd, target_is_directory=True)
        if app.History(self.cwd).path != app.History(alias).path:
            msg = "Expected app.History(self.cwd).path == app.History(alias).path"
            raise AssertionError(msg)
        if app.History(self.cwd).path == app.History(self.cwd / "child").path:
            msg = "Child directories must have separate histories"
            raise AssertionError(msg)
        for value in ("", "relative", None):
            with patch.dict(os.environ, {"HOME": str(self.cwd)}):
                if value is None:
                    os.environ.pop("XDG_STATE_HOME", None)
                else:
                    os.environ["XDG_STATE_HOME"] = value
                if not (
                    app.History(self.cwd).path.is_relative_to(self.cwd / ".local/state")
                ):
                    msg = (
                        "Invalid or absent XDG_STATE_HOME must use the default location"
                    )
                    raise AssertionError(msg)
        with patch.dict(os.environ, {"XDG_STATE_HOME": str(self.cwd / "custom")}):
            if not (app.History(self.cwd).path.is_relative_to(self.cwd / "custom")):
                msg = "Absolute XDG_STATE_HOME must override the default location"
                raise AssertionError(msg)

    def test_restart_restores_context_and_full_searchable_transcript(self) -> None:  # noqa: C901, D102
        with app.History(self.cwd) as history:
            history.load()
            agent = app.Agent(self.cwd, history=history)
            agent.model = "old-model"
            viewer = app.Viewer(agent)
            agent.event = viewer.event
            output = "x" * (app.OUTPUT_LIMIT + 50) + "needle"
            with patch.object(
                agent,
                "completion",
                side_effect=[
                    answer(calls=[call("read", "read", path="large")])["choices"][0][
                        "message"
                    ],
                    answer("done")["choices"][0]["message"],
                ],
            ):
                (self.cwd / "large").write_text(output)
                agent.turn("read it")
            if len(viewer.entries) != 3:  # noqa: PLR2004
                msg = "Expected len(viewer.entries) == 3"
                raise AssertionError(msg)
            if history.path.stat().st_mode & 0o777 != 0o600:  # noqa: PLR2004
                msg = "Expected history.path.stat().st_mode & 511 == 384"
                raise AssertionError(msg)
            if history.directory.stat().st_mode & 0o777 != 0o700:  # noqa: PLR2004
                msg = "Expected history.directory.stat().st_mode & 511 == 448"
                raise AssertionError(msg)
        with app.History(self.cwd) as history:
            history.load()
            agent = app.Agent(self.cwd, history=history)
            if agent.model is not None:
                msg = "Expected agent.model is None"
                raise AssertionError(msg)
            if history.prompts != ["read it"]:
                msg = "Expected history.prompts == ['read it']"
                raise AssertionError(msg)
            if agent.messages[-1]["content"] != "done":
                msg = "Expected agent.messages[-1]['content'] == 'done'"
                raise AssertionError(msg)
            if agent.messages[0]["content"] != app.README.read_text():
                msg = "Expected agent.messages[0]['content'] == app.README.read_text()"
                raise AssertionError(msg)
            if not (len(agent.messages[-2]["content"]) < len(output)):
                msg = "Expected len(agent.messages[-2]['content']) < len(output)"
                raise AssertionError(msg)
            viewer = app.Viewer(agent)
            if not (all(not entry.expanded for entry in viewer.entries)):
                msg = "Expected all((not entry.expanded for entry in viewer.entries))"
                raise AssertionError(msg)
            viewer.pattern = "needle"
            viewer.search(1)
            if not (viewer.match is not None):
                msg = "Expected viewer.match is not None"
                raise AssertionError(msg)
            if output not in viewer.entries[viewer.selected].body:
                msg = "Expected output in viewer.entries[viewer.selected].body"
                raise AssertionError(msg)

    def test_failed_and_cancelled_turns_keep_only_completed_context(self) -> None:  # noqa: D102
        for failure in (app.AgentError("offline"), KeyboardInterrupt()):
            with self.subTest(failure=type(failure)), app.History(self.cwd) as history:
                history.clear()
                agent = app.Agent(self.cwd, history=history)
                agent.model = "local"
                with patch.object(
                    agent,
                    "completion",
                    return_value=answer("done")["choices"][0]["message"],
                ):
                    agent.turn("first")
                with (
                    patch.object(
                        agent,
                        "completion",
                        side_effect=[
                            answer(
                                calls=[call("shell", "bash", command="printf kept")],
                            )["choices"][0]["message"],
                            failure,
                        ],
                    ),
                    pytest.raises(type(failure)),
                ):
                    agent.turn("second")
                history.load()
                if history.prompts != ["first", "second"]:
                    msg = "Expected history.prompts == ['first', 'second']"
                    raise AssertionError(msg)
                if len(history.messages) != 2:  # noqa: PLR2004
                    msg = "Expected len(history.messages) == 2"
                    raise AssertionError(msg)
                if not (any("kept" in entry.body for entry in history.entries)):
                    msg = "Failed turns must retain completed tool output"
                    raise AssertionError(msg)

    def test_abrupt_exit_keeps_prompt_and_marks_tool_interrupted(self) -> None:  # noqa: D102
        code = """
import os
from pathlib import Path
from packages.coding_agent.main import History
with History(Path.cwd()) as history:
    history.prompts.append('pending')
    history.event('chat', 'user> pending', None)
    history.event('tool', 'tool> bash', None)
    os._exit(0)
"""
        result = subprocess.run([sys.executable, "-c", code], check=False)  # noqa: S603
        if result.returncode != 0:
            msg = "Expected result.returncode == 0"
            raise AssertionError(msg)
        with app.History(Path.cwd()) as history:
            history.load()
            if history.prompts != ["pending"]:
                msg = "Expected history.prompts == ['pending']"
                raise AssertionError(msg)
            if history.messages != []:
                msg = "Expected history.messages == []"
                raise AssertionError(msg)
            if history.entries[-1].success:
                msg = "Expected not history.entries[-1].success"
                raise AssertionError(msg)
            if "Interrupted" not in history.entries[-1].body:
                msg = "Expected 'Interrupted' in history.entries[-1].body"
                raise AssertionError(msg)

    def test_lock_and_failed_atomic_save_preserve_checkpoint(self) -> None:  # noqa: D102
        with app.History(self.cwd) as history:
            history.save()
            previous = history.path.read_bytes()
            with pytest.raises(app.HistoryError), app.History(self.cwd):
                self.fail("Concurrent history ownership was allowed")
            history.prompts.append("new")
            with (
                patch.object(Path, "replace", side_effect=OSError("disk error")),
                pytest.raises(app.HistoryError),
            ):
                history.save()
            if history.path.read_bytes() != previous:
                msg = "Expected history.path.read_bytes() == previous"
                raise AssertionError(msg)
            if {p.name for p in history.directory.iterdir()} != {
                "history.json",
                "lock",
            }:
                msg = "Failed saves must clean up temporary files"
                raise AssertionError(msg)
        with app.History(self.cwd) as history:
            history.load()
            if history.prompts != []:
                msg = "Expected history.prompts == []"
                raise AssertionError(msg)

    def test_invalid_history_is_preserved_and_can_be_cleared(self) -> None:  # noqa: D102
        with app.History(Path.cwd()) as history:
            history.save()
            valid = json.loads(history.path.read_text())
            invalid = [
                "{",
                json.dumps({**valid, "version": 2}),
                json.dumps({**valid, "messages": [None]}),
                json.dumps(
                    {**valid, "messages": [{"role": "user", "content": "unfinished"}]},
                ),
                json.dumps({**valid, "entries": [{"body": 5}]}),
            ]
            for content in invalid:
                history.path.write_text(content)
                with pytest.raises(app.HistoryError):
                    history.load()
                if history.path.read_text() != content:
                    msg = "Expected history.path.read_text() == content"
                    raise AssertionError(msg)
        with app.History(self.cwd) as other:
            other.save()
        with patch.object(
            app.Agent,
            "request",
            side_effect=AssertionError("Unexpected HTTP"),
        ):
            app.main(["--clear-history"])
        if history.path.exists():
            msg = "Expected not history.path.exists()"
            raise AssertionError(msg)
        if not (other.path.exists()):
            msg = "Expected other.path.exists()"
            raise AssertionError(msg)
        with (
            contextlib.redirect_stderr(io.StringIO()),
            pytest.raises(SystemExit) as error,
        ):
            app.main(["--clear-history", "--prompt", "hello"])
        if error.value.code != 2:  # noqa: PLR2004
            msg = "Expected error.value.code == 2"
            raise AssertionError(msg)

    def test_cli_modes_share_context_and_restore_readline(self) -> None:  # noqa: D102
        with (
            patch.object(app.Agent, "discover"),
            patch.object(
                app.Agent,
                "completion",
                return_value=answer("reply")["choices"][0]["message"],
            ),
            contextlib.redirect_stdout(io.StringIO()) as output,
        ):
            app.main(["--prompt", "first"])
            if output.getvalue() != "reply\n":
                msg = "Expected output.getvalue() == 'reply\\n'"
                raise AssertionError(msg)
            with patch("builtins.input", side_effect=["second", EOFError()]):
                app.main([])
        if readline.get_history_item(1) != "first":
            msg = "Expected readline.get_history_item(1) == 'first'"
            raise AssertionError(msg)
        with app.History(Path.cwd()) as history:
            history.load()
            if history.prompts != ["first", "second"]:
                msg = "Expected history.prompts == ['first', 'second']"
                raise AssertionError(msg)
            if [message["content"] for message in history.messages] != [
                "first",
                "reply",
                "second",
                "reply",
            ]:
                msg = "CLI modes must continue the same conversation"
                raise AssertionError(msg)

    def test_terminal_recalls_saved_prompts_after_restart(self) -> None:  # noqa: C901, D102
        with app.History(Path.cwd()) as history:
            history.prompts = ["saved prompt"]
            history.save()
        code = """
from packages.coding_agent import main as app
app.Agent.discover = lambda self: None
app.Agent.completion = lambda self: {'role': 'assistant', 'content': 'replied'}
app.main([])
"""
        for keys in (b"\x1b[A\n", b"\x12saved\n\n"):
            master, slave = pty.openpty()
            fcntl.ioctl(master, termios.TIOCSWINSZ, struct.pack("HHHH", 24, 80, 0, 0))
            try:
                with subprocess.Popen(  # noqa: S603
                    [sys.executable, "-c", code],
                    stdin=slave,
                    stdout=slave,
                    stderr=slave,
                    env={**os.environ, "TERM": "xterm", "INPUTRC": "/dev/null"},
                ) as process:
                    try:
                        buffer = b""
                        deadline = time.monotonic() + 5
                        while b"\x1b[24;1H> " not in buffer:
                            if time.monotonic() >= deadline:
                                msg = f"History terminal did not start: {buffer!r}"
                                raise AssertionError(msg)
                            if select.select([master], [], [], 0.1)[0]:
                                buffer += os.read(master, 65536)
                        os.write(master, keys)
                        buffer = b""
                        while (
                            b"assistant> replied" not in buffer
                            or not buffer.endswith(b"> ")
                        ):
                            if time.monotonic() >= deadline:
                                msg = f"Recalled prompt was not answered: {buffer!r}"
                                raise AssertionError(msg)
                            if select.select([master], [], [], 0.1)[0]:
                                buffer += os.read(master, 65536)
                        os.write(master, b"\x04")
                        process.wait(timeout=5)
                        if process.returncode != 0:
                            msg = "History terminal did not exit successfully"
                            raise AssertionError(msg)
                    finally:
                        if process.poll() is None:
                            process.kill()
            finally:
                os.close(master)
                os.close(slave)
        with app.History(Path.cwd()) as history:
            history.load()
            if history.prompts != ["saved prompt"] * 3:
                msg = "Up and Ctrl-R must recall prompts from previous processes"
                raise AssertionError(msg)

    def test_cli_reports_storage_errors_without_contacting_model(self) -> None:  # noqa: D102
        for operation in ("load", "save"):
            with (
                patch.object(
                    app.History,
                    operation,
                    side_effect=app.HistoryError("disk error"),
                ),
                patch.object(
                    app.Agent,
                    "request",
                    side_effect=AssertionError("Unexpected HTTP"),
                ),
                contextlib.redirect_stderr(io.StringIO()) as error,
                pytest.raises(SystemExit) as exited,
            ):
                app.main(["--prompt", "hello"])
            if exited.value.code != 1 or "disk error" not in error.getvalue():
                msg = "Storage errors must stop the CLI with an actionable message"
                raise AssertionError(msg)

    def test_single_prompt_saves_full_command_output(self) -> None:  # noqa: D102
        command = f"printf '%{app.OUTPUT_LIMIT + 50}s' ''; printf needle"
        with (
            patch.object(app.Agent, "discover"),
            patch.object(
                app.Agent,
                "completion",
                side_effect=[
                    answer(
                        calls=[
                            call(
                                "shell",
                                "bash",
                                command=command,
                            ),
                        ],
                    )["choices"][0]["message"],
                    answer("done")["choices"][0]["message"],
                ],
            ),
            contextlib.redirect_stdout(io.StringIO()) as output,
        ):
            app.main(["--prompt", "print a long output"])
        with app.History(Path.cwd()) as history:
            history.load()
            if (
                len(history.entries[1].body) <= app.OUTPUT_LIMIT
                or "needle" not in history.entries[1].body
            ):
                msg = "Single-prompt history must retain full command output"
                raise AssertionError(msg)
            if "needle" in history.messages[-2]["content"]:
                msg = "Model context must still bound command output"
                raise AssertionError(msg)
        if output.getvalue() != "done\n":
            msg = "Saving command output must not change single-prompt stdout"
            raise AssertionError(msg)


@contextlib.contextmanager
def server(responses: list[Any]) -> Iterator[tuple[str, list[tuple[str, Any]]]]:  # noqa: D103
    requests = []

    class Handler(BaseHTTPRequestHandler):
        def do_GET(self) -> None:
            self.respond()

        def do_POST(self) -> None:
            self.respond()

        def respond(self) -> None:
            body = self.rfile.read(int(self.headers.get("Content-Length", 0)))
            requests.append((self.path, json.loads(body) if body else None))
            self.send_response(200)
            self.end_headers()
            response = responses.pop(0)
            self.wfile.write(
                response
                if isinstance(response, bytes)
                else json.dumps(response).encode(),
            )

        def log_message(self, message_format: str, *args: object) -> None:
            pass

    http = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    thread = threading.Thread(target=http.serve_forever)
    thread.start()
    try:
        yield f"http://127.0.0.1:{http.server_port}", requests
    finally:
        http.shutdown()
        http.server_close()
        thread.join()


def answer(  # noqa: D103
    content: str | None = None,
    calls: list[dict[str, Any]] | None = None,
) -> dict[str, Any]:
    return {
        "choices": [
            {
                "message": {
                    "role": "assistant",
                    "content": content,
                    **({"tool_calls": calls} if calls is not None else {}),
                },
            },
        ],
    }


def call(identifier: str, name: str, **args: str) -> dict[str, Any]:  # noqa: D103
    return {
        "id": identifier,
        "type": "function",
        "function": {"name": name, "arguments": json.dumps(args)},
    }


class TestAgent(unittest.TestCase):  # noqa: D101
    def setUp(self) -> None:  # noqa: D102
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.agent = app.Agent(self.directory.name)

    def execute(self, name: str, **args: object) -> str:  # noqa: D102
        return self.agent.execute(name, json.dumps(args))

    def test_files(self) -> None:  # noqa: C901, D102, PLR0912
        if self.execute("write", path="nested/a", content="héllo") != "OK":
            msg = (
                "Expected self.execute('write', path='nested/a', "
                "content='héllo') == 'OK'"
            )
            raise AssertionError(msg)
        if self.execute("read", path="nested/a") != "héllo":
            msg = "Expected self.execute('read', path='nested/a') == 'héllo'"
            raise AssertionError(msg)
        if self.execute("edit", path="nested/a", old_text="hé", new_text="H") != "OK":
            msg = (
                "Expected self.execute('edit', path='nested/a', "
                "old_text='hé', new_text='H') == 'OK'"
            )
            raise AssertionError(msg)
        if (
            self.execute("read", path=str(Path(self.directory.name) / "nested/a"))
            != "Hllo"
        ):
            msg = (
                "Expected self.execute('read', "
                "path=str(Path(self.directory.name) / 'nested/a')) == 'Hllo'"
            )
            raise AssertionError(msg)
        self.execute("write", path="nested/a", content="aaa")
        for old in ("", "z", "a", "aa"):
            if "Tool error" not in self.execute(
                "edit",
                path="nested/a",
                old_text=old,
                new_text="b",
            ):
                msg = (
                    "Expected 'Tool error' in self.execute('edit', "
                    "path='nested/a', old_text=old, new_text='b')"
                )
                raise AssertionError(msg)
        if self.execute("read", path="nested/a") != "aaa":
            msg = "Expected self.execute('read', path='nested/a') == 'aaa'"
            raise AssertionError(msg)
        for name, args in [
            ("read", {"path": "missing"}),
            ("write", {"path": ".", "content": "x"}),
            ("write", {"path": "nested/a/b", "content": "x"}),
        ]:
            if "Tool error" not in self.execute(name, **args):
                msg = "Expected 'Tool error' in self.execute(name, **args)"
                raise AssertionError(msg)
        if "Tool error" not in self.agent.execute("read", "not json"):
            msg = "Expected 'Tool error' in self.agent.execute('read', 'not json')"
            raise AssertionError(msg)
        if "Tool error" not in self.execute("other"):
            msg = "Expected 'Tool error' in self.execute('other')"
            raise AssertionError(msg)
        if "Tool error" not in self.execute("read", path=3):
            msg = "Expected 'Tool error' in self.execute('read', path=3)"
            raise AssertionError(msg)
        self.execute("write", path="large", content="x" * 20000)
        if len(self.execute("read", path="large")) != app.OUTPUT_LIMIT:
            msg = "Expected len(self.execute('read', path='large')) == app.OUTPUT_LIMIT"
            raise AssertionError(msg)
        if not (self.execute("read", path="large").endswith("[output truncated]")):
            msg = (
                "Expected self.execute('read', "
                "path='large').endswith('[output truncated]')"
            )
            raise AssertionError(msg)

    def test_bash(self) -> None:  # noqa: D102
        if "exit status: 0\nhello" not in self.execute("bash", command="printf hello"):
            msg = (
                "Expected 'exit status: 0\\nhello' in self.execute('bash', "
                "command='printf hello')"
            )
            raise AssertionError(msg)
        if "exit status: 7\nerror" not in self.execute(
            "bash",
            command="printf error >&2; exit 7",
        ):
            msg = (
                "Expected 'exit status: 7\\nerror' in self.execute('bash', "
                "command='printf error >&2; exit 7')"
            )
            raise AssertionError(msg)
        if self.directory.name not in self.execute("bash", command="pwd"):
            msg = "Expected self.directory.name in self.execute('bash', command='pwd')"
            raise AssertionError(msg)
        with patch.object(app, "BASH_TIMEOUT", 0.05):
            if "timed out" not in self.execute("bash", command="while :; do :; done"):
                msg = (
                    "Expected 'timed out' in self.execute('bash', command='while "
                    ":; do :; done')"
                )
                raise AssertionError(msg)
        output = self.execute("bash", command="printf '%20000s' x")
        if len(output) != app.OUTPUT_LIMIT:
            msg = "Expected len(output) == app.OUTPUT_LIMIT"
            raise AssertionError(msg)
        if not (output.endswith("[output truncated]")):
            msg = "Expected output.endswith('[output truncated]')"
            raise AssertionError(msg)

    def test_nix_arguments(self) -> None:  # noqa: D102
        with patch.object(self.agent, "run", return_value="exit status: 0\nok") as run:
            output = self.execute("nix", arguments='build "./path with spaces#pkg"')
        run.assert_called_once_with(
            [
                "nix",
                "--extra-experimental-features",
                "nix-command flakes",
                "build",
                "./path with spaces#pkg",
            ],
            app.NIX_TIMEOUT,
        )
        if output != "exit status: 0\nok":
            msg = "Nix must return the command status and output"
            raise AssertionError(msg)
        if "Tool error (nix)" not in self.execute("nix", arguments='build "'):
            msg = "Invalid quoting must be reported as a tool error"
            raise AssertionError(msg)

    def test_http_sequence(self) -> None:  # noqa: D102
        calls = [
            call("one", "write", path="a", content="text"),
            call("two", "read", path="a"),
        ]
        with server(
            [
                {"data": [{"id": "local"}]},
                answer(calls=calls),
                answer("done"),
                answer("again"),
            ],
        ) as (url, requests):
            agent = app.Agent(self.directory.name, url)
            if agent.turn("do it") != "done":
                msg = "Expected agent.turn('do it') == 'done'"
                raise AssertionError(msg)
            if agent.turn("next") != "again":
                msg = "Expected agent.turn('next') == 'again'"
                raise AssertionError(msg)
        if requests[0][0] != "/v1/models":
            msg = "Expected requests[0][0] == '/v1/models'"
            raise AssertionError(msg)
        payload = requests[1][1]
        if payload["model"] != "local":
            msg = "Expected payload['model'] == 'local'"
            raise AssertionError(msg)
        if payload["stream"]:
            msg = "Expected not payload['stream']"
            raise AssertionError(msg)
        if [t["function"]["name"] for t in payload["tools"]] != [
            "read",
            "write",
            "edit",
            "bash",
            "nix",
        ]:
            msg = (
                "Expected [t['function']['name'] for t in payload['tools']] "
                "== ['read', 'write', 'edit', 'bash', 'nix']"
            )
            raise AssertionError(msg)
        results = requests[2][1]["messages"][-2:]
        if [(r["tool_call_id"], r["content"]) for r in results] != [
            ("one", "OK"),
            ("two", "text"),
        ]:
            msg = (
                "Expected [(r['tool_call_id'], r['content']) for r in "
                "results] == [('one', 'OK'), ('two', 'text')]"
            )
            raise AssertionError(msg)
        if requests[3][1]["messages"][-2:] != [
            {"role": "assistant", "content": "done"},
            {"role": "user", "content": "next"},
        ]:
            msg = (
                "Expected requests[3][1]['messages'][-2:] == [{'role': "
                "'assistant', 'content': 'done'}, {'role': 'user', 'content':"
                " 'next'}]"
            )
            raise AssertionError(msg)

    def test_errors_and_limit(self) -> None:  # noqa: D102
        response: Any
        for response in (
            b"bad JSON",
            {},
            {"choices": []},
            answer(),
            answer(calls=[{}]),
        ):
            with self.subTest(response=response), server([response]) as (url, _):
                agent = app.Agent(self.directory.name, url)
                agent.model = "local"
                with pytest.raises(app.AgentError, match="Protocol error"):
                    agent.turn("test")
                if len(agent.messages) != 1:
                    msg = "Expected len(agent.messages) == 1"
                    raise AssertionError(msg)
        with (
            server([{"data": []}]) as (url, _),
            pytest.raises(app.AgentError, match="model ID"),
        ):
            app.Agent(base_url=url).discover()
        with pytest.raises(app.AgentError, match="Connection/HTTP error"):
            app.Agent(base_url=url).discover()
        self.agent.model = "local"
        message = answer(calls=[call("one", "read", path="missing")])["choices"][0][
            "message"
        ]
        with patch.object(self.agent, "completion", return_value=message) as completion:
            with pytest.raises(app.AgentError, match="20 model requests"):
                self.agent.turn("loop")
            if completion.call_count != app.MAX_REQUESTS:
                msg = "Expected completion.call_count == app.MAX_REQUESTS"
                raise AssertionError(msg)
        with (
            patch.object(self.agent, "completion", return_value=message),
            patch.object(self.agent, "execute", side_effect=KeyboardInterrupt),
            pytest.raises(KeyboardInterrupt),
        ):
            self.agent.turn("cancel")
        if len(self.agent.messages) != 1:
            msg = "Expected len(self.agent.messages) == 1"
            raise AssertionError(msg)

    def test_failed_or_cancelled_turn_preserves_files_and_previous_conversation(  # noqa: D102
        self,
    ) -> None:
        for failure in (app.AgentError("connection lost"), KeyboardInterrupt()):
            with self.subTest(failure=type(failure).__name__):
                agent = app.Agent(self.directory.name)
                agent.model = "local"
                with patch.object(
                    agent,
                    "completion",
                    return_value=answer("done")["choices"][0]["message"],
                ):
                    agent.turn("first turn")
                previous_messages = list(agent.messages)
                write = answer(
                    calls=[
                        call("write-file", "write", path="saved", content="keep me"),
                    ],
                )["choices"][0]["message"]
                with (
                    patch.object(agent, "completion", side_effect=[write, failure]),
                    pytest.raises(type(failure)),
                ):
                    agent.turn("interrupted turn")
                if agent.messages != previous_messages:
                    msg = "Expected agent.messages == previous_messages"
                    raise AssertionError(msg)
                if (Path(self.directory.name) / "saved").read_text() != "keep me":
                    msg = (
                        "Expected (Path(self.directory.name) / 'saved').read_text() "
                        "== 'keep me'"
                    )
                    raise AssertionError(msg)

    def test_cli_sends_prompt_and_displays_reply(self) -> None:  # noqa: D102
        with (
            patch.object(app, "Agent", return_value=self.agent),
            patch.object(self.agent, "turn", return_value="model reply") as turn,
            patch("builtins.input", side_effect=["user prompt", EOFError()]),
            contextlib.redirect_stdout(io.StringIO()) as output,
        ):
            app.main([])
        if turn.call_args_list != [unittest.mock.call("user prompt")]:
            msg = "The CLI must send the prompt to the agent exactly once"
            raise AssertionError(msg)
        if output.getvalue() != "model reply\n\n":
            msg = "The CLI must display the model reply without a startup banner"
            raise AssertionError(msg)

    def test_noninteractive_prompt(self) -> None:  # noqa: D102
        for flag in ("-p", "--prompt"):
            with (
                self.subTest(flag=flag),
                patch.object(app, "Agent", return_value=self.agent),
                patch.object(self.agent, "turn", return_value="model reply") as turn,
                patch("builtins.input", side_effect=AssertionError("Unexpected input")),
                contextlib.redirect_stdout(io.StringIO()) as output,
            ):
                app.main([flag, "user prompt"])
            turn.assert_called_once_with("user prompt")
            if output.getvalue() != "model reply\n":
                msg = "Non-interactive mode must print only the reply"
                raise AssertionError(msg)

    def test_noninteractive_failure(self) -> None:  # noqa: D102
        for failure, status in (
            (app.AgentError("offline"), 1),
            (KeyboardInterrupt(), 130),
        ):
            with (
                self.subTest(status=status),
                patch.object(app, "Agent", return_value=self.agent),
                patch.object(self.agent, "turn", side_effect=failure),
                contextlib.redirect_stderr(io.StringIO()) as error,
                pytest.raises(SystemExit) as raised,
            ):
                app.main(["--prompt", "user prompt"])
            if raised.value.code != status or not error.getvalue():
                msg = (
                    "Failures must report an error and exit with the appropriate status"
                )
                raise AssertionError(msg)

    def test_noninteractive_empty_prompt(self) -> None:  # noqa: D102
        usage_error = 2
        for prompt in ("", " \n"):
            with (
                self.subTest(prompt=prompt),
                patch.object(app, "Agent") as agent,
                contextlib.redirect_stderr(io.StringIO()),
                pytest.raises(SystemExit) as raised,
            ):
                app.main(["--prompt", prompt])
            agent.assert_not_called()
            if raised.value.code != usage_error:
                msg = "Empty prompts must be rejected as usage errors"
                raise AssertionError(msg)

    def test_executable_starts_without_banner_and_exits_on_eof(self) -> None:  # noqa: D102
        executable = os.environ.get("PACKAGE_E2E_EXECUTABLE")
        if not executable:
            self.skipTest("Nix package executable not supplied")
        result = subprocess.run(  # noqa: S603
            [executable],
            input="",
            text=True,
            capture_output=True,
            timeout=10,
            check=False,
        )
        if result.returncode != 0:
            msg = result.stderr
            raise AssertionError(msg)
        if result.stdout != "> \n":
            msg = "Expected result.stdout == '> \\n'"
            raise AssertionError(msg)


class TestViewer(unittest.TestCase):  # noqa: D101
    def test_chat_and_view_render_the_same_tree(self) -> None:  # noqa: D102
        viewer = app.Viewer(app.Agent())
        viewer.event("chat", "assistant> preview\nfull response", None)
        viewer.event("tool", "tool> bash false", None)
        viewer.event("output", "exit status: 1\nfailed output", None)
        viewer.event("finish", "", False)  # noqa: FBT003
        viewer.entries[0].expanded = True
        viewer.entries[1].expanded = True
        screen = MagicMock()
        screen.getmaxyx.return_value = (12, 60)
        screen.get_wch.return_value = "q"
        output = io.StringIO()
        with (
            contextlib.redirect_stdout(output),
            patch.object(output, "isatty", return_value=True),
            patch.object(
                shutil,
                "get_terminal_size",
                return_value=os.terminal_size((60, 12)),
            ),
            patch.object(curses, "has_colors", return_value=False),
            patch.object(curses, "curs_set"),
        ):
            viewer.render_chat()
            viewer.screen(screen)
        chat = output.getvalue()
        rendered = [call.args[2] for call in screen.addstr.call_args_list]
        if rendered != [row.text for row in viewer.rows(59)]:
            msg = "Both modes must render identical tree rows"
            raise AssertionError(msg)
        if any(text not in chat for text in rendered) or "\x1b[31m" not in chat:
            msg = "Chat must include expanded children and failed command color"
            raise AssertionError(msg)

    def test_search_reveals_wrapped_matches_and_repeats_on_same_line(self) -> None:  # noqa: D102
        viewer = app.Viewer(app.Agent())
        viewer.width, viewer.height = 20, 3
        viewer.event("chat", "assistant> long\n" + "x" * 90 + "needle needle", None)
        viewer.pattern = "needle"
        viewer.search(1)
        first = viewer.match
        visible = viewer.rows(viewer.width)[viewer.top : viewer.top + viewer.height]
        if not any(viewer.matched(row) and "needle" in row.text for row in visible):
            msg = "Search must scroll to the matching wrapped segment"
            raise AssertionError(msg)
        viewer.search(1, repeat=True)
        if viewer.match is None or first is None or viewer.match <= first:
            msg = "n must advance to later matches on the same line"
            raise AssertionError(msg)
        viewer.pattern = "$"
        viewer.search(1)
        viewer.search(1, repeat=True)
        if viewer.match is None:
            msg = "Zero-width matches at line ends must remain navigable"
            raise AssertionError(msg)

    def test_paging_at_last_parent_does_not_jump_backwards(self) -> None:  # noqa: D102
        viewer = app.Viewer(app.Agent())
        for index in range(30):
            viewer.event("chat", f"assistant> {index}", None)
        for _ in viewer.entries:
            viewer.navigate("j", 10, viewer.rows(80))
        top = viewer.top
        viewer.navigate(" ", 10, viewer.rows(80))
        if viewer.top < top:
            msg = "Forward paging at the final parent must not scroll backwards"
            raise AssertionError(msg)

    def test_read_output_resembling_error_is_not_duplicated(self) -> None:  # noqa: D102
        with tempfile.TemporaryDirectory() as directory:
            Path(directory, "example").write_text("Tool error is ordinary file content")
            agent = app.Agent(directory)
            viewer = app.Viewer(agent)
            agent.event = viewer.event
            agent.model = "local"
            with patch.object(
                agent,
                "completion",
                side_effect=[
                    answer(None, [call("read", "read", path="example")])["choices"][0][
                        "message"
                    ],
                    answer("done")["choices"][0]["message"],
                ],
            ):
                agent.turn("read example")
            entry = viewer.entries[0]
            if (
                entry.body != "Tool error is ordinary file content"
                or entry.success is not True
            ):
                msg = "Successful file output must be retained exactly once"
                raise AssertionError(msg)

    def test_unicode_and_tiny_terminals(self) -> None:  # noqa: D102
        viewer = app.Viewer(app.Agent())
        viewer.event("chat", "assistant> 界\ne\u0301界界", None)
        viewer.entries[0].expanded = True
        for width in (1, 2, 4, 20):
            for row in viewer.rows(width):
                cells = sum(
                    0
                    if unicodedata.combining(char)
                    else 2
                    if unicodedata.east_asian_width(char) in {"W", "F"}
                    else 1
                    for char in row.text
                )
                if cells > width:
                    msg = "Wide characters and indentation must fit the terminal"
                    raise AssertionError(msg)

    def test_tree_navigation_and_paging(self) -> None:  # noqa: D102
        page_height = 10
        viewer = app.Viewer(app.Agent())
        viewer.event("chat", "assistant> preview\n" + "detail\n" * 40, None)
        viewer.event("chat", "user> next", None)
        if len(viewer.rows(80)) != len(viewer.entries):
            msg = "New entries must be collapsed"
            raise AssertionError(msg)
        viewer.navigate("l", 10, viewer.rows(80))
        viewer.navigate(" ", 10, viewer.rows(80))
        if viewer.top != page_height or viewer.selected != 0:
            msg = "Paging must scroll inside an expanded child"
            raise AssertionError(msg)
        viewer.navigate("b", 10, viewer.rows(80))
        if viewer.top != 0:
            msg = "Backward paging must return to the first row"
            raise AssertionError(msg)
        viewer.navigate("j", 10, viewer.rows(80))
        if viewer.selected != 1 or not any(
            row.owner == 1 and row.line == -1
            for row in viewer.rows(80)[viewer.top : viewer.top + page_height]
        ):
            msg = "j must skip output and select the next parent"
            raise AssertionError(msg)
        viewer.navigate("k", 10, viewer.rows(80))
        viewer.navigate("h", 10, viewer.rows(80))
        if viewer.entries[0].expanded or len(viewer.rows(80)) != len(viewer.entries):
            msg = "h must hide the selected output"
            raise AssertionError(msg)
        viewer.navigate("g", 10, viewer.rows(80))
        viewer.navigate("k", 10, viewer.rows(80))
        if viewer.selected != 0:
            msg = "Parent navigation must stop at the start"
            raise AssertionError(msg)

    def test_search_expands_hidden_output_and_repeats(self) -> None:  # noqa: D102
        viewer = app.Viewer(app.Agent())
        for text in ("assistant> one\nhidden 1", "assistant> two\nhidden 2"):
            viewer.event("chat", text, None)
        viewer.pattern = r"hidden \d"
        viewer.search(1)
        if viewer.match != (0, 1, 0) or not viewer.entries[0].expanded:
            msg = "Search must reveal hidden output"
            raise AssertionError(msg)
        viewer.search(1, repeat=True)
        if viewer.match != (1, 1, 0) or not viewer.entries[1].expanded:
            msg = "Repeated search must advance to the next entry"
            raise AssertionError(msg)
        viewer.search(-1, repeat=True)
        if viewer.match != (0, 1, 0):
            msg = "Reverse repeat must find the previous match"
            raise AssertionError(msg)
        viewer.pattern = "["
        viewer.search(1)
        if "Invalid pattern" not in viewer.status or viewer.match is not None:
            msg = "Invalid patterns must clear stale matches without crashing"
            raise AssertionError(msg)
        viewer.pattern = "missing"
        viewer.search(1)
        if viewer.status != "Pattern not found":
            msg = "Missing patterns must be reported"
            raise AssertionError(msg)

    def test_tool_entries_use_execution_status_and_full_output(self) -> None:  # noqa: D102
        agent = app.Agent()
        viewer = app.Viewer(agent)
        agent.event = viewer.event
        agent.model = "local"
        calls = [
            call("ok", "bash", command="printf 'Tool error fake'; printf '%20000s' x"),
            call("bad", "bash", command="printf 'exit status: 0'; exit 7"),
            call("missing", "read", path="/nonexistent-coding-agent-test-file"),
        ]
        with patch.object(
            agent,
            "completion",
            side_effect=[
                answer("Running", calls)["choices"][0]["message"],
                answer("Finished")["choices"][0]["message"],
            ],
        ):
            agent.turn("test")
        entries = [entry for entry in viewer.entries if entry.tool]
        if [entry.success for entry in entries] != [True, False, False]:
            msg = "Tool status must come from execution, regardless of output text"
            raise AssertionError(msg)
        if len(entries[0].body) <= app.OUTPUT_LIMIT:
            msg = "Viewer must retain output beyond the model's truncation limit"
            raise AssertionError(msg)
        if (
            "exit status: 7" not in entries[1].body
            or "Tool error" not in entries[2].body
        ):
            msg = "Each tool must retain its own output"
            raise AssertionError(msg)
        if any(entry.expanded for entry in entries):
            msg = "Completed tools must start collapsed"
            raise AssertionError(msg)

    def test_wrapping_and_control_characters(self) -> None:  # noqa: D102
        viewer = app.Viewer(app.Agent())
        viewer.event("chat", "assistant> preview\n\x1b[31m" + "x" * 100, None)
        viewer.entries[0].expanded = True
        width = 20
        rows = viewer.rows(width)
        if any(len(row[2]) > width or "\x1b" in row[2] for row in rows):
            msg = "Output must wrap and must not execute terminal escape sequences"
            raise AssertionError(msg)

    def test_chat_restores_readline_hook(self) -> None:  # noqa: D102
        viewer = app.Viewer(app.Agent())
        library = ctypes.CDLL(readline.__file__)
        slot = ctypes.c_void_p.in_dll(library, "rl_getc_function")
        previous = slot.value
        with patch("builtins.input", return_value="hello") as read:
            if viewer.read_chat() != "hello":
                msg = "Chat must use native readline input"
                raise AssertionError(msg)
        read.assert_called_once_with("> ")
        if slot.value != previous:
            msg = "Readline input hook must be restored"
            raise AssertionError(msg)
        with patch("builtins.input", side_effect=EOFError), pytest.raises(EOFError):
            viewer.read_chat()
        if slot.value != previous:
            msg = "Readline input hook must be restored"
            raise AssertionError(msg)

    def test_transcript_keeps_output_after_failure(self) -> None:  # noqa: D102
        agent = app.Agent()
        viewer = app.Viewer(agent)
        agent.model = "local"
        response = answer(
            "Running command",
            [call("shell", "bash", command="printf hello; printf error >&2")],
        )["choices"][0]["message"]
        with (
            patch.object(viewer, "read_chat", side_effect=["do it", EOFError()]),
            patch.object(
                agent,
                "completion",
                side_effect=[response, app.AgentError("offline")],
            ),
            contextlib.redirect_stdout(io.StringIO()),
        ):
            viewer.run()
        transcript = "\n".join(viewer.lines)
        for expected in (
            "> do it",
            "assistant> Running command",
            "tool> bash",
            "exit status: 0",
            "helloerror",
            "offline",
        ):
            if expected not in transcript:
                raise AssertionError(expected)
        if len(agent.messages) != 1:
            msg = "Failed turn history must be discarded"
            raise AssertionError(msg)
        if agent.output is not None:
            msg = "Output hook must be restored"
            raise AssertionError(msg)
        if agent.event is not None or not any(
            entry.tool and entry.success for entry in viewer.entries
        ):
            msg = (
                "Failed turns must retain completed entries and restore the event hook"
            )
            raise AssertionError(msg)


class TestReadlineTerminal(unittest.TestCase):  # noqa: D101
    def setUp(self) -> None:  # noqa: D102
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        inputrc = Path(self.directory.name) / "inputrc"
        inputrc.write_text(
            'set editing-mode emacs\n"\\C-xj": "configured"\n'
            '"\\C-xv": vi-editing-mode\n',
        )
        self.completion_path = Path(self.directory.name) / "completion-example"
        self.completion_path.touch()
        self.master, slave = pty.openpty()
        fcntl.ioctl(self.master, termios.TIOCSWINSZ, struct.pack("HHHH", 24, 80, 0, 0))
        self.addCleanup(os.close, self.master)
        code = """
import json
from packages.coding_agent.main import Agent, Viewer
agent = Agent()
def turn(prompt):
    agent.emit("RESULT:" + json.dumps(prompt))
    return ""
agent.turn = turn
Viewer(agent).run()
"""
        try:
            self.process = subprocess.Popen(  # noqa: S603
                [sys.executable, "-c", code],
                stdin=slave,
                stdout=slave,
                stderr=slave,
                env={**os.environ, "TERM": "xterm", "INPUTRC": str(inputrc)},
            )
        finally:
            os.close(slave)
        self.addCleanup(self.stop)
        self.buffer = b""
        self.wait_prompt()

    def stop(self) -> None:  # noqa: D102
        if self.process.poll() is None:
            self.process.kill()
        self.process.wait(timeout=5)

    def wait_for(self, marker: bytes) -> None:  # noqa: D102
        deadline = time.monotonic() + 5
        while marker not in self.buffer:
            if time.monotonic() >= deadline:
                msg = f"Terminal did not display {marker!r}: {self.buffer!r}"
                raise AssertionError(msg)
            if select.select([self.master], [], [], 0.1)[0]:
                self.buffer += os.read(self.master, 65536)
        self.buffer = self.buffer.split(marker, 1)[1]

    def wait_prompt(self) -> None:
        """Wait for the input row, not a role label in a transcript redraw."""
        height = os.get_terminal_size(self.master).lines
        self.wait_for(f"\x1b[{height};1H> ".encode())

    def send(self, keys: bytes, expected: str) -> None:  # noqa: D102
        os.write(self.master, keys)
        self.wait_for(b"RESULT:" + json.dumps(expected).encode())
        self.wait_prompt()

    def test_native_editing_history_completion_and_inputrc(self) -> None:  # noqa: D102
        self.send(b"hello world\x01X\x05\n", "Xhello world")
        self.send(b"\x1b[A\n", "Xhello world")
        self.send(b"\x12hello\n\n", "Xhello world")
        self.send(b"one two\x17\x19\n", "one two")
        self.send(b"one two\x1bbX\n", "one Xtwo")
        self.send(b"\x18j\n", "configured")
        self.send(
            str(self.completion_path)[:-3].encode() + b"\t\n",
            str(self.completion_path),
        )
        os.write(self.master, b"\x04")
        if self.process.wait(timeout=5) != 0:
            msg = "EOF at an empty native readline prompt must exit cleanly"
            raise AssertionError(msg)

    def test_viewer_roundtrip_preserves_readline_draft_cursor_and_undo(self) -> None:  # noqa: D102
        os.write(self.master, b"hello\x01X\x1b")
        self.wait_for(b"\x1b[?1049h")
        os.write(self.master, b"q")
        self.wait_for(b"\x1b[?1049l")
        self.wait_prompt()
        self.send(b"\x1f\n", "hello")
        os.write(self.master, b"hello\x01\x1b")
        self.wait_for(b"\x1b[?1049h")
        os.write(self.master, b":q")
        self.wait_for(b"\x1b[?1049l")
        self.wait_prompt()
        self.send(b"X\n", "Xhello")

    def test_empty_prompt_repeated_viewing_and_search(self) -> None:  # noqa: D102
        self.send(b"searchable transcript\n", "searchable transcript")
        for quit_keys in (b"q", b":q", b"ZZ"):
            os.write(self.master, b"\x1b")
            self.wait_for(b"\x1b[?1049h")
            self.wait_for(b"searchable transcript")
            os.write(self.master, b"/searchable\n")
            self.wait_for(b"(END)")
            os.write(self.master, quit_keys)
            self.wait_for(b"\x1b[?1049l")
            self.wait_prompt()
        self.send(b"after viewing\n", "after viewing")
        os.write(self.master, b"\x04")
        if self.process.wait(timeout=5) != 0:
            msg = "EOF after viewing must exit cleanly"
            raise AssertionError(msg)

    def test_tree_expand_collapse_resize_and_reopen(self) -> None:  # noqa: D102
        self.send(b"tree example\n", "tree example")
        os.write(self.master, b"\x1b")
        self.wait_for(b"\x1b[?1049h")
        self.wait_for(b"[+]")
        os.write(self.master, b"gl")
        self.wait_for(b"-")
        self.wait_for(b"user> tree example")
        os.write(self.master, b"h")
        self.wait_for(b"+")
        fcntl.ioctl(self.master, termios.TIOCSWINSZ, struct.pack("HHHH", 12, 50, 0, 0))
        os.kill(self.process.pid, signal.SIGWINCH)
        os.write(self.master, b"jlq")
        self.wait_for(b"\x1b[?1049l")
        self.wait_prompt()
        os.write(self.master, b"\x1b")
        self.wait_for(b"\x1b[?1049h")
        self.wait_for(b"[-]")
        os.write(self.master, b"q")
        self.wait_for(b"\x1b[?1049l")
        self.wait_prompt()
        self.send(b"still working\n", "still working")

    def test_chat_resize_preserves_tree_and_draft(self) -> None:  # noqa: D102
        self.send(b"resize example\n", "resize example")
        os.write(self.master, b"draft\x1b")
        self.wait_for(b"\x1b[?1049h")
        os.write(self.master, b"glq")
        self.wait_for(b"\x1b[?1049l")
        self.wait_for(b"[-] user> resize example")
        self.wait_for(b"  user> resize example")
        self.wait_prompt()
        self.wait_for(b"draft")
        fcntl.ioctl(self.master, termios.TIOCSWINSZ, struct.pack("HHHH", 12, 50, 0, 0))
        os.kill(self.process.pid, signal.SIGWINCH)
        self.wait_for(b"  user> resize example")
        self.wait_prompt()
        self.wait_for(b"draft")
        self.send(b"\x01X\n", "Xdraft")

    def test_vi_escape_keeps_native_command_mode(self) -> None:  # noqa: D102
        os.write(self.master, b"\x18vhello\x1b")
        self.wait_for(b"\x08")
        self.send(b"0x\n", "ello")
        os.write(self.master, b"world\x1b")
        self.wait_for(b"\x08")
        os.write(self.master, b"\x1b")
        self.wait_for(b"\x1b[?1049h")
        os.write(self.master, b"q")
        self.wait_for(b"\x1b[?1049l")
        self.wait_prompt()
        self.send(b"x\n", "worl")

    def test_interrupt_cancels_readline_and_next_prompt_works(self) -> None:  # noqa: D102
        os.write(self.master, b"\x1b")
        self.wait_for(b"\x1b[?1049h")
        os.write(self.master, b"q")
        self.wait_for(b"\x1b[?1049l")
        self.wait_prompt()
        os.kill(self.process.pid, signal.SIGINT)
        self.wait_for(b"Cancelled.")
        self.wait_prompt()
        self.send(b"next\n", "next")


if __name__ == "__main__":
    unittest.main()
