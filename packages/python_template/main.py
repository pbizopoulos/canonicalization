#!/usr/bin/env python3
# Copyright (c) 2026- Paschalis Bizopoulos
"""Provide a minimal executable Python package."""

from __future__ import annotations

import os
import subprocess

SAMPLE_MESSAGE = "Hello World Python"


def main() -> None:
    """Print the package's sample message."""
    print(SAMPLE_MESSAGE)  # noqa: T201


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
        message = "executable output should match the sample message"
        raise AssertionError(message)


if __name__ == "__main__":
    main()
