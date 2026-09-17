# Copyright (c) 2026- Paschalis Bizopoulos
"""Tests for remove_new_lines."""

from __future__ import annotations

import os
import subprocess
import tempfile
from pathlib import Path

from hypothesis import example, given
from hypothesis import strategies as st

from packages.remove_new_lines.main import (
    process_file,
    remove_new_lines,
)


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


@given(contents=st.binary(max_size=1024))
@example(contents=b"\r\n\x00\xff\rhello\n\r")
@example(contents=b"")
def test_removes_only_cr_and_lf(contents: bytes) -> None:
    """The result is exactly the ordered subsequence of non-newline bytes."""
    expected = bytes(value for value in contents if value not in {10, 13})
    if remove_new_lines(contents) != expected:
        msg = "newline removal lost or changed another byte"
        raise AssertionError(msg)


@given(contents=st.binary(max_size=256))
@example(contents=b"text\r\n\xff")
def test_generated_binary_files_and_symlinks_are_untouched(contents: bytes) -> None:
    """Neither a binary file nor a symlink target may be rewritten."""
    with tempfile.TemporaryDirectory() as directory:
        root = Path(directory)
        binary = root / "binary"
        binary_contents = b"\0\r\n" + contents
        binary.write_bytes(binary_contents)
        target = root / "target"
        target_contents = b"text\r\n" + contents
        target.write_bytes(target_contents)
        link = root / "link"
        link.symlink_to(target)
        process_file(binary)
        process_file(link)
        if (
            binary.read_bytes() != binary_contents
            or target.read_bytes() != target_contents
            or not link.is_symlink()
        ):
            msg = "file processing changed a protected file"
            raise AssertionError(msg)
