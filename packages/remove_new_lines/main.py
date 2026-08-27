#!/usr/bin/env python3
# Copyright (c) 2026- Paschalis Bizopoulos
"""Remove new lines from explicitly selected text files."""

from __future__ import annotations

import os
import subprocess
import sys
import tempfile
from pathlib import Path


def remove_new_lines(contents: bytes) -> bytes:
    """Remove CR and LF bytes without changing other bytes."""
    return contents.replace(b"\n", b"").replace(b"\r", b"")


def process_file(path: Path) -> None:
    """Rewrite one regular text file without following symbolic links."""
    if path.is_symlink() or not path.is_file():
        return
    contents = path.read_bytes()
    if is_binary(contents):
        return
    updated_contents = remove_new_lines(contents)
    if updated_contents != contents:
        path.write_bytes(updated_contents)


def is_binary(contents: bytes) -> bool:
    """Avoid rewriting files that contain a NUL byte."""
    return b"\0" in contents


def main() -> None:
    """Remove new lines from the supplied file paths."""
    for path in map(Path, sys.argv[1:]):
        process_file(path)


def test_remove_new_lines_preserves_non_newline_bytes() -> None:
    """Retains all non-newline bytes."""
    if remove_new_lines(b"first\r\n\xff\nlast") != b"first\xfflast":
        message = "only CR and LF bytes should be removed"
        raise AssertionError(message)


def test_process_file_skips_binary_files_and_symbolic_links() -> None:
    """Leaves binary files and symbolic-link targets untouched."""
    with tempfile.TemporaryDirectory() as temporary_directory:
        directory = Path(temporary_directory)
        binary_path = directory / "binary.bin"
        binary_contents = b"\x00line\n"
        binary_path.write_bytes(binary_contents)
        target_path = directory / "target.txt"
        target_path.write_text("line\n", encoding="utf-8")
        link_path = directory / "link.txt"
        link_path.symlink_to(target_path)
        process_file(binary_path)
        process_file(link_path)
        if binary_path.read_bytes() != binary_contents:
            message = "binary files should be unchanged"
            raise AssertionError(message)
        if target_path.read_text(encoding="utf-8") != "line\n":
            message = "symbolic-link targets should be unchanged"
            raise AssertionError(message)


def test_main_processes_explicit_paths() -> None:
    """Runs the installed executable on explicit paths."""
    with tempfile.TemporaryDirectory() as temporary_directory:
        path = Path(temporary_directory) / "text.txt"
        path.write_text("first\r\nlast\n", encoding="utf-8")
        completed = subprocess.run(  # noqa: S603
            [os.environ["PACKAGE_E2E_EXECUTABLE"], str(path)],
            check=False,
            capture_output=True,
            text=True,
        )
        if completed.returncode != 0 or completed.stdout or completed.stderr:
            message = "the executable should succeed without console output"
            raise AssertionError(message)
        if path.read_text(encoding="utf-8") != "firstlast":
            message = "the executable should remove only new lines"
            raise AssertionError(message)


if __name__ == "__main__":
    main()
