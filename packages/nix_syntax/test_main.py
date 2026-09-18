# Copyright (c) 2026- Paschalis Bizopoulos
"""Validate complete Nix documents through the packaged command."""

from __future__ import annotations

import os
import subprocess
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from pathlib import Path


def test_cli_validates_files_and_reports_each_failure(tmp_path: Path) -> None:
    """Accept valid Unicode Nix, report malformed and missing files, and never edit."""
    valid = tmp_path / "valid.nix"
    valid.write_text(
        '{ "é"."a b" = "${toString 1}"; f = { x, ... }: x; }',
        encoding="utf-8",
    )
    invalid = tmp_path / "invalid.nix"
    invalid.write_text("{ invalid = ; }", encoding="utf-8")
    missing = tmp_path / "missing.nix"
    originals = {path: path.read_bytes() for path in (valid, invalid)}
    for paths, code in [([valid], 0), ([invalid, valid, missing], 1)]:
        result = subprocess.run(  # noqa: S603
            [os.environ["PACKAGE_E2E_EXECUTABLE"], *map(str, paths)],
            capture_output=True,
            text=True,
            check=False,
            timeout=10,
        )
        if result.returncode != code or result.stdout:
            raise AssertionError(result)
        if code and not all(str(path) in result.stderr for path in (invalid, missing)):
            raise AssertionError(result.stderr)
        if not code and result.stderr:
            raise AssertionError(result.stderr)
    if any(path.read_bytes() != source for path, source in originals.items()):
        message = "validation modified an input"
        raise AssertionError(message)
