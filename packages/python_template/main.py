#!/usr/bin/env python3
"""Python Hello World."""

from __future__ import annotations

import contextlib
import io
import os
import unittest
from unittest import mock

from hypothesis import given
from hypothesis import strategies as st


def render_message() -> str:
    """Return the default program message."""
    return "Hello World"


class MainTests(unittest.TestCase):
    """Unit tests for the tiny CLI behavior."""

    def test_render_message(self) -> None:
        """Ensure the static message remains stable."""
        if render_message() != "Hello World":
            msg = "render_message() must return 'Hello World'"
            raise AssertionError(msg)

    def test_main_prints_message_when_not_debug(self) -> None:
        """main() should emit the message when DEBUG is not enabled."""
        output = io.StringIO()
        with (
            mock.patch.dict(os.environ, {"DEBUG": "0"}),
            contextlib.redirect_stdout(
                output,
            ),
        ):
            main()
        if output.getvalue().strip() != "Hello World":
            msg = "main() should print 'Hello World' when DEBUG != 1"
            raise AssertionError(msg)

    def test_main_calls_run_tests_when_debug(self) -> None:
        """main() should execute run_tests() when DEBUG is enabled."""
        with (
            mock.patch.dict(os.environ, {"DEBUG": "1"}),
            mock.patch(
                "__main__.run_tests",
            ) as run_tests_mock,
        ):
            main()
        if run_tests_mock.call_count != 1:
            msg = "main() should call run_tests() exactly once when DEBUG=1"
            raise AssertionError(msg)


class PropertyTests(unittest.TestCase):
    """Property-based tests for stable program behavior."""

    @given(st.integers())  # type: ignore[untyped-decorator]
    def test_render_message_is_constant(self, generated_number: int) -> None:
        """render_message() should stay constant for all generated inputs."""
        _ = generated_number
        if render_message() != "Hello World":
            msg = "render_message() must return 'Hello World'"
            raise AssertionError(msg)

    @given(st.text())  # type: ignore[untyped-decorator]
    def test_render_message_returns_string(self, generated_text: str) -> None:
        """render_message() should always return a string."""
        _ = generated_text
        if not isinstance(render_message(), str):
            msg = "render_message() must return a string"
            raise TypeError(msg)


def run_tests() -> None:
    """Run this module's unittest suite."""
    suite = unittest.defaultTestLoader.loadTestsFromTestCase(MainTests)
    result = unittest.TextTestRunner(verbosity=2).run(suite)
    if not result.wasSuccessful():
        raise SystemExit(1)


def main() -> None:
    """Run main."""
    if os.getenv("DEBUG") == "1":
        run_tests()
    else:
        print(render_message())  # noqa: T201


if __name__ == "__main__":
    main()
