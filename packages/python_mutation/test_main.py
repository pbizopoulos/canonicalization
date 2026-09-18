# Copyright (c) 2026- Paschalis Bizopoulos
"""Integration tests for isolated mutation campaigns."""

from __future__ import annotations

import json
import os
import subprocess
import sys
from typing import TYPE_CHECKING

import pytest

from packages.python_mutation.main import (
    MutationError,
    campaign,
    copy_sources,
)

if TYPE_CHECKING:
    from pathlib import Path


def make_target(root: Path, source: str, tests: str, *, name: str = "example") -> Path:
    """Create a minimal canonical package for an isolated test."""
    package = root / "packages" / name
    package.mkdir(parents=True)
    (root / "flake.nix").write_text("{}", encoding="utf-8")
    (package / "default.nix").write_text("{}", encoding="utf-8")
    (package / "main.py").write_text(source, encoding="utf-8")
    (package / "test_main.py").write_text(tests, encoding="utf-8")
    return package


def test_campaign_covers_subprocesses_and_reports_survivors(tmp_path: Path) -> None:
    """CLI-only tests kill mutations while untested functions survive."""
    root = tmp_path / "source with spaces"
    source = (
        "def value():\n    return 1\n\n"
        "def unused():\n    return 2\n\n"
        "def main():\n    print(value())\n"
    )
    tests = (
        "import os, subprocess\n"
        "def test_cli():\n"
        "    result = subprocess.run([os.environ['PACKAGE_E2E_EXECUTABLE']],\n"
        "        capture_output=True, text=True)\n"
        "    assert result.returncode == 0\n"
        "    assert result.stdout == '1\\n'\n"
    )
    package = make_target(root, source, tests)
    workspace = tmp_path / "workspace with spaces"
    copy_sources(root, workspace)
    if not (campaign(workspace, "example", sys.executable, "", 10)):
        message = "mutation runner expectation failed"
        raise AssertionError(message)
    summary = json.loads((workspace / "summary.json").read_text(encoding="utf-8"))
    if not (summary["killed"] > 0):
        message = "mutation runner expectation failed"
        raise AssertionError(message)
    if not (summary["survived"] > 0):
        message = "mutation runner expectation failed"
        raise AssertionError(message)
    if not ((workspace / "report.html").stat().st_size > 0):
        message = "mutation runner expectation failed"
        raise AssertionError(message)
    if not ((workspace / "session.sqlite").is_file()):
        message = "mutation runner expectation failed"
        raise AssertionError(message)
    if (package / "main.py").read_text(encoding="utf-8") != source:
        message = "mutation runner expectation failed"
        raise AssertionError(message)
    if (workspace / "packages/example/main.py").read_text(encoding="utf-8") != source:
        message = "mutation runner expectation failed"
        raise AssertionError(message)
    if list(workspace.rglob("*.pyc")):
        message = "mutation runner expectation failed"
        raise AssertionError(message)


def test_campaign_direct_import_and_no_mutations(tmp_path: Path) -> None:
    """Tests can import canonical paths, including modules without mutations."""
    root = tmp_path / "source"
    make_target(
        root,
        "",
        "from packages.example import main\n"
        "def test_import():\n    assert main is not None\n",
    )
    workspace = tmp_path / "workspace"
    copy_sources(root, workspace)
    if not (campaign(workspace, "example", sys.executable, "", 10)):
        message = "mutation runner expectation failed"
        raise AssertionError(message)
    if json.loads((workspace / "summary.json").read_text(encoding="utf-8")) != {}:
        message = "mutation runner expectation failed"
        raise AssertionError(message)


def test_baseline_failure_aborts_before_mutation(tmp_path: Path) -> None:
    """Failing, empty, and uncollectable suites cannot produce mutation scores."""
    cases = [
        "def test_failure():\n    assert False\n",
        "",
        "raise ImportError('missing dependency')\n",
    ]
    for index, tests in enumerate(cases):
        root = tmp_path / f"source-{index}"
        make_target(root, "answer = 1\n", tests)
        workspace = tmp_path / f"workspace-{index}"
        copy_sources(root, workspace)
        with pytest.raises(MutationError, match=r"baseline\.log"):
            campaign(workspace, "example", sys.executable, "", 10)
        if (workspace / "session.sqlite").exists():
            message = "baseline failures must not initialize mutations"
            raise AssertionError(message)


def test_cli_errors_and_help(tmp_path: Path) -> None:
    """The installed CLI rejects invalid targets and nonfinite timeouts."""
    executable = os.environ["PACKAGE_E2E_EXECUTABLE"]
    for arguments, code in [
        (["--help"], 0),
        ([str(tmp_path)], 1),
        ([str(tmp_path), "--timeout", "nan"], 2),
    ]:
        result = subprocess.run(  # noqa: S603
            [executable, *arguments],
            capture_output=True,
            text=True,
            check=False,
        )
        if result.returncode != code:
            message = "mutation runner expectation failed"
            raise AssertionError(message)


@pytest.mark.parametrize(
    "layout",
    ["empty", "nonpython", "untested", "single_untested"],
)
def test_cli_repository_without_runnable_packages(tmp_path: Path, layout: str) -> None:
    """Report empty repositories, skipped packages, and invalid explicit targets."""
    (tmp_path / "flake.nix").write_text("{}", encoding="utf-8")
    target = tmp_path
    if layout == "nonpython":
        package = tmp_path / "packages" / "web"
        package.mkdir(parents=True)
        (package / "default.nix").write_text("{}", encoding="utf-8")
        (package / "index.html").write_text("hello", encoding="utf-8")
    elif layout in {"untested", "single_untested"}:
        package = make_target(tmp_path, "", "")
        (package / "test_main.py").unlink()
        if layout == "single_untested":
            target = package
    result = subprocess.run(  # noqa: S603
        [os.environ["PACKAGE_E2E_EXECUTABLE"], str(target)],
        capture_output=True,
        text=True,
        check=False,
    )
    expected_code = 0 if layout == "untested" else 1
    if result.returncode != expected_code:
        raise AssertionError(result.stdout + result.stderr)
    if layout == "untested":
        if (
            "Skipping example: no test_main.py" not in result.stdout
            or "0 passed, 0 failed, 1 skipped" not in result.stdout
        ):
            msg = "repository did not report its skipped package"
            raise AssertionError(msg)
    elif (
        layout != "single_untested" and "no Python packages found" not in result.stderr
    ):
        msg = "empty repository diagnostic missing"
        raise AssertionError(msg)
    if (tmp_path / "tmp").exists():
        msg = "a non-runnable target started a workspace"
        raise AssertionError(msg)
