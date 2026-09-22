# Copyright (c) 2026 VALAB/ITI
"""Verify tool execution, model protocol, and the interactive entry point."""

import contextlib
import ctypes
import io
import json
import os
import pty
import readline
import select
import signal
import subprocess
import sys
import tempfile
import threading
import time
import unittest
from collections.abc import Iterator
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from typing import Any
from unittest.mock import patch

import pytest

from packages.coding_agent import main as app


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
        self.wait_for(b"> ")

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

    def send(self, keys: bytes, expected: str) -> None:  # noqa: D102
        os.write(self.master, keys)
        self.wait_for(b"RESULT:" + json.dumps(expected).encode())
        self.wait_for(b"> ")

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
        self.wait_for(b"> ")
        self.send(b"\x1f\n", "hello")
        os.write(self.master, b"hello\x01\x1b")
        self.wait_for(b"\x1b[?1049h")
        os.write(self.master, b":q")
        self.wait_for(b"\x1b[?1049l")
        self.wait_for(b"> ")
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
            self.wait_for(b"> ")
        self.send(b"after viewing\n", "after viewing")
        os.write(self.master, b"\x04")
        if self.process.wait(timeout=5) != 0:
            msg = "EOF after viewing must exit cleanly"
            raise AssertionError(msg)

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
        self.wait_for(b"> ")
        self.send(b"x\n", "worl")

    def test_interrupt_cancels_readline_and_next_prompt_works(self) -> None:  # noqa: D102
        os.write(self.master, b"\x1b")
        self.wait_for(b"\x1b[?1049h")
        os.write(self.master, b"q")
        self.wait_for(b"\x1b[?1049l")
        self.wait_for(b"> ")
        os.kill(self.process.pid, signal.SIGINT)
        self.wait_for(b"Cancelled.")
        self.wait_for(b"> ")
        self.send(b"next\n", "next")


if __name__ == "__main__":
    unittest.main()
