# Copyright (c) 2026- Paschalis Bizopoulos
"""Tests for remove_empty_lines."""

from __future__ import annotations

import os
import subprocess
import tempfile
from pathlib import Path

from packages.remove_empty_lines.main import (
    process_file,
    remove_empty_lines,
)


def test_remove_empty_lines_preserves_nonempty_and_invalid_utf8_lines() -> None:
    """Removes only whitespace-only UTF-8 lines."""
    contents = b"first\r\n \t\r\n\xff\nlast"
    if remove_empty_lines(contents) != b"first\r\n\xff\nlast":
        message = "only UTF-8 whitespace lines should be removed"
        raise AssertionError(message)


def test_process_file_skips_binary_files_and_symbolic_links() -> None:
    """Leaves binary files and symbolic-link targets untouched."""
    with tempfile.TemporaryDirectory() as temporary_directory:
        directory = Path(temporary_directory)
        binary_path = directory / "binary.bin"
        binary_contents = b"\x00line\n\n"
        binary_path.write_bytes(binary_contents)
        target_path = directory / "target.txt"
        target_path.write_text("line\n\n", encoding="utf-8")
        link_path = directory / "link.txt"
        link_path.symlink_to(target_path)
        process_file(binary_path)
        process_file(link_path)
        if binary_path.read_bytes() != binary_contents:
            message = "binary files should be unchanged"
            raise AssertionError(message)
        if target_path.read_text(encoding="utf-8") != "line\n\n":
            message = "symbolic-link targets should be unchanged"
            raise AssertionError(message)


def test_main_processes_explicit_paths() -> None:
    """Runs the installed executable on explicit paths."""
    with tempfile.TemporaryDirectory() as temporary_directory:
        path = Path(temporary_directory) / "text.txt"
        path.write_text("first\n\nlast\n", encoding="utf-8")
        completed = subprocess.run(  # noqa: S603
            [os.environ["PACKAGE_E2E_EXECUTABLE"], str(path)],
            check=False,
            capture_output=True,
            text=True,
        )
        if completed.returncode != 0 or completed.stdout or completed.stderr:
            message = "the executable should succeed without console output"
            raise AssertionError(message)
        if path.read_text(encoding="utf-8") != "first\nlast\n":
            message = "the executable should remove only empty lines"
            raise AssertionError(message)
