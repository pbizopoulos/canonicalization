# Copyright (c) 2026- Paschalis Bizopoulos
"""Tests for nix_alphabetize."""

from __future__ import annotations

import os
import subprocess
import tempfile
from pathlib import Path

from packages.nix_alphabetize.main import (
    format_text,
)


def test_preserves_string_order_and_sorts_other_constructs() -> None:
    """Canonicalizes formals, bindings, and non-string lists."""
    formatted = format_text(
        '{ z = [ 3 1 2 ]; strings = [ "c" "a" ]; f = { z, a }: z + a; }',
    )
    if not (
        formatted.index("f =") < formatted.index("strings =") < formatted.index("z =")
    ):
        raise AssertionError
    if not (formatted.index("1") < formatted.index("2") < formatted.index("3")):
        raise AssertionError
    if not (formatted.index('"c"') < formatted.index('"a"')):
        raise AssertionError


def test_dotted_bindings_collapse_safely() -> None:
    """Collapses compatible bindings without changing conflicting paths."""
    formatted = format_text("{ b.z = 1; b.x = 2; a = 1; }")
    if "b = {" not in formatted:
        raise AssertionError
    if "x = 2;" not in formatted:
        raise AssertionError
    if "z = 1;" not in formatted:
        raise AssertionError
    conflicting = format_text("{ a = 1; a.b = 2; }")
    if "a = 1;" not in conflicting:
        raise AssertionError
    if "a.b = 2;" not in conflicting:
        raise AssertionError


def test_installed_executable_formats_files() -> None:
    """Runs the installed command on explicit file paths."""
    executable = os.environ.get("PACKAGE_E2E_EXECUTABLE")
    if executable is None:
        return
    with tempfile.TemporaryDirectory() as directory:
        path = Path(directory) / "example.nix"
        path.write_text("{ b = 2; a = 1; }", encoding="utf-8")
        completed = subprocess.run(  # noqa: S603
            [executable, str(path)],
            capture_output=True,
            check=False,
            text=True,
        )
        if not (completed.returncode == 0):
            raise AssertionError
        if completed.stdout:
            raise AssertionError
        if completed.stderr:
            raise AssertionError
        if not (
            path.read_text(encoding="utf-8").index("a = 1")
            < path.read_text(
                encoding="utf-8",
            ).index("b = 2")
        ):
            raise AssertionError
