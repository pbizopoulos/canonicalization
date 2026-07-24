#!/usr/bin/env python3
"""Provide a minimal executable Python package."""

from __future__ import annotations

import contextlib
import io

SAMPLE_MESSAGE = "Hello World Python"


def render_message() -> str:
    """Return the package's sample message."""
    return SAMPLE_MESSAGE


def main() -> None:
    """Print the package's sample message."""
    print(render_message())  # noqa: T201


def test_render_message_returns_sample_message() -> None:
    """The package should render its sample message."""
    if render_message() != SAMPLE_MESSAGE:
        msg = "rendered sample message should match"
        raise AssertionError(msg)


def test_main_prints_sample_message() -> None:
    """The executable should print the sample message."""
    output = io.StringIO()
    with contextlib.redirect_stdout(output):
        main()
    if output.getvalue() != f"{SAMPLE_MESSAGE}\n":
        msg = "executable output should match the sample message"
        raise AssertionError(msg)


if __name__ == "__main__":
    main()
