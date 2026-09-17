# Copyright (c) 2026- Paschalis Bizopoulos
"""Tests for remove_empty_lines."""

from __future__ import annotations

import os
import subprocess
import tempfile
from pathlib import Path

from hypothesis import example, given
from hypothesis import strategies as st

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


@given(contents=st.binary())
@example(contents=b"first\n \n\xff\nlast")
def test_remove_empty_lines_is_idempotent(contents: bytes) -> None:
    """Removing empty lines a second time leaves the result unchanged."""
    result = remove_empty_lines(contents)
    if remove_empty_lines(result) != result:
        message = "empty-line removal must be idempotent"
        raise AssertionError(message)


_LINE = st.one_of(
    st.tuples(
        st.text(alphabet=" \t\v\f\x1c\u0085\u00a0\u2003", max_size=12).map(str.encode),
        st.just(value=False),
    ),
    st.tuples(
        st.text(alphabet="abc é中\t\u2003", max_size=12).map(
            lambda text: b"x" + text.encode(),
        ),
        st.just(value=True),
    ),
    st.tuples(
        st.lists(st.integers(128, 255), max_size=12).map(
            lambda values: b"\xff" + bytes(values),
        ),
        st.just(value=True),
    ),
)


@given(
    lines=st.lists(_LINE, max_size=20),
    ending=st.sampled_from([b"\n", b"\r", b"\r\n"]),
    terminated=st.booleans(),
)
@example(
    lines=[(b"x", True), (b"\xc2\xa0", False), (b"\xff", True)],
    ending=b"\r\n",
    terminated=False,
)
@example(lines=[(b"", False)], ending=b"\n", terminated=False)
def test_removes_exactly_the_generated_empty_lines(
    lines: list[tuple[bytes, bool]],
    ending: bytes,
    *,
    terminated: bool,
) -> None:
    """Retain each nonempty or undecodable line byte-for-byte and in order."""
    source = []
    expected = []
    for index, (body, keep) in enumerate(lines):
        line = body + (ending if terminated or index < len(lines) - 1 else b"")
        source.append(line)
        if keep:
            expected.append(line)
    result = remove_empty_lines(b"".join(source))
    if result != b"".join(expected):
        msg = "empty-line removal changed retained data"
        raise AssertionError(msg)
    if remove_empty_lines(result) != result:
        msg = "empty-line removal did not converge"
        raise AssertionError(msg)
