# Copyright (c) 2026 VALAB/ITI
"""Verify tool execution, model protocol, and the interactive entry point."""

import contextlib
import curses
import io
import json
import os
import subprocess
import sys
import tempfile
import threading
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
    def setUp(self) -> None:  # noqa: D102
        self.agent = app.Agent()
        self.screen = unittest.mock.Mock()
        self.screen.getmaxyx.return_value = (6, 80)
        self.viewer = app.Viewer(self.screen, self.agent)

    def test_modes_preserve_draft_and_submit_only_on_enter(self) -> None:  # noqa: D102
        viewer = self.viewer
        with patch.object(self.agent, "turn") as turn:
            for key in ":hello\x1b":
                viewer.key(key)
            if viewer.mode != "view":
                msg = 'viewer.mode == "view"'
                raise AssertionError(msg)
            if viewer.draft != "hello":
                msg = 'viewer.draft == "hello"'
                raise AssertionError(msg)
            turn.assert_not_called()
            viewer.key(":")
            viewer.key("\n")
            turn.assert_called_once_with("hello")
        if "> hello" not in viewer.lines:
            msg = '"> hello" in viewer.lines'
            raise AssertionError(msg)
        if viewer.mode != "chat":
            msg = 'viewer.mode == "chat"'
            raise AssertionError(msg)
        viewer.key("\x1b")
        if viewer.key("q"):
            msg = 'not viewer.key("q")'
            raise AssertionError(msg)

    def test_search_scroll_and_wrap(self) -> None:  # noqa: D102
        viewer = self.viewer
        viewer.lines = ["first hit", "plain", "second hit", "last hit"]
        for key in "/hit\n":
            viewer.key(key)
        if viewer.match != 2:  # noqa: PLR2004
            msg = "viewer.match == 2"
            raise AssertionError(msg)
        viewer.draw()
        viewer.key("n")
        if viewer.match != 3:  # noqa: PLR2004
            msg = "viewer.match == 3"
            raise AssertionError(msg)
        viewer.key("n")
        if viewer.match != 0:
            msg = "viewer.match == 0"
            raise AssertionError(msg)
        viewer.key("N")
        if viewer.match != 3:  # noqa: PLR2004
            msg = "viewer.match == 3"
            raise AssertionError(msg)
        for key in "?missing\n":
            viewer.key(key)
        if viewer.notice != "Pattern not found: missing":
            msg = 'viewer.notice == "Pattern not found: missing"'
            raise AssertionError(msg)
        viewer.lines.extend(["more"] * 20)
        viewer.key("g")
        viewer.key(" ")
        if viewer.top != 4:  # noqa: PLR2004
            msg = "viewer.top == 4"
            raise AssertionError(msg)
        viewer.key("G")
        viewer.draw()
        if viewer.top != 20:  # noqa: PLR2004
            msg = "viewer.top == 20"
            raise AssertionError(msg)

    def test_transcript_keeps_tool_output_after_failure(self) -> None:  # noqa: D102
        self.agent.model = "local"
        self.agent.output = self.viewer.append
        response = answer(
            "Running command",
            [call("shell", "bash", command="printf hello; printf error >&2")],
        )["choices"][0]["message"]
        self.viewer.draft = "do it"
        with patch.object(
            self.agent,
            "completion",
            side_effect=[response, app.AgentError("offline")],
        ):
            self.viewer.submit()
        transcript = "\n".join(self.viewer.lines)
        for expected in (
            "> do it",
            "assistant> Running command",
            "tool> bash",
            "exit status: 0",
            "helloerror",
            "offline",
        ):
            if expected not in transcript:
                msg = "expected in transcript"
                raise AssertionError(msg)
        if len(self.agent.messages) != 1:
            msg = "len(self.agent.messages) == 1"
            raise AssertionError(msg)

    def test_full_command_output_is_visible_but_model_result_is_bounded(self) -> None:  # noqa: D102
        self.agent.output = self.viewer.append
        result = self.agent.bash("printf '%20000s' x; printf tail")
        if len(result) != app.OUTPUT_LIMIT:
            msg = "len(result) == app.OUTPUT_LIMIT"
            raise AssertionError(msg)
        if not ("tail" not in result):
            msg = '"tail" not in result'
            raise AssertionError(msg)
        if not ("\n".join(self.viewer.lines).endswith("xtail")):
            msg = '"\\n".join(self.viewer.lines).endswith("xtail")'
            raise AssertionError(msg)

    def test_terminal_uses_viewer(self) -> None:  # noqa: D102
        with (
            patch.object(sys.stdin, "isatty", return_value=True),
            patch.object(sys.stdout, "isatty", return_value=True),
            patch.object(curses, "wrapper") as wrapper,
            patch.object(curses, "set_escdelay"),
        ):
            app.main([])
        wrapper.assert_called_once()

    def test_chat_editing_history_and_completion(self) -> None:  # noqa: D102
        viewer = self.viewer
        for key in ":helo":
            viewer.key(key)
        viewer.key(curses.KEY_LEFT)
        viewer.key("l")
        if viewer.draft != "hello":
            msg = 'viewer.draft == "hello"'
            raise AssertionError(msg)
        with patch.object(self.agent, "turn"):
            viewer.key("\n")
        viewer.key(curses.KEY_UP)
        if viewer.draft != "hello":
            msg = 'viewer.draft == "hello"'
            raise AssertionError(msg)
        viewer.key(curses.KEY_DOWN)
        if viewer.draft != "":
            msg = 'viewer.draft == ""'
            raise AssertionError(msg)
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "example"
            path.touch()
            for key in directory + "/exam":
                viewer.key(key)
            viewer.key("\t")
            if viewer.draft != str(path):
                msg = "viewer.draft == str(path)"
                raise AssertionError(msg)


if __name__ == "__main__":
    unittest.main()
