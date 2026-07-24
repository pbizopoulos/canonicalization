#!/usr/bin/env python3
"""Provide a minimal executable Python package."""

from __future__ import annotations

import os
import subprocess

SAMPLE_MESSAGE = "Hello World Python"


def render_message() -> str:
    """Return the package's sample message."""
    return SAMPLE_MESSAGE


def main() -> None:
    """Print the package's sample message."""
    print(render_message())  # noqa: T201


def test_render_message_returns_sample_message() -> None:
    """Renders the package's sample message."""
    if render_message() != SAMPLE_MESSAGE:
        msg = "rendered sample message should match"
        raise AssertionError(msg)


def test_main_prints_sample_message() -> None:
    """Prints the sample message from the executable."""
    completed = subprocess.run(  # noqa: S603
        [os.environ["PACKAGE_E2E_EXECUTABLE"]],
        check=False,
        capture_output=True,
        text=True,
    )
    if (
        completed.returncode != 0
        or completed.stdout != f"{SAMPLE_MESSAGE}\n"
        or completed.stderr
    ):
        msg = "executable output should match the sample message"
        raise AssertionError(msg)


if __name__ == "__main__":
    main()
