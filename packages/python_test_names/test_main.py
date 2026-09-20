# Copyright (c) 2026- Paschalis Bizopoulos
"""CLI requirements for reading test names as sentences."""

from __future__ import annotations

import os
import subprocess
from typing import TYPE_CHECKING

import pytest

if TYPE_CHECKING:
    from pathlib import Path


def make_package(root: Path, name: str, source: str) -> Path:
    """Create a canonical package fixture without executing its source."""
    package = root / "packages" / name
    package.mkdir(parents=True)
    (root / "flake.nix").touch()
    (package / "default.nix").touch()
    (package / "main.py").touch()
    (package / "test_main.py").write_text(source)
    return package


def run_cli(cwd: Path, *arguments: str) -> subprocess.CompletedProcess[str]:
    """Invoke the installed command from a chosen working directory."""
    return subprocess.run(  # noqa: S603
        [os.environ["PACKAGE_E2E_EXECUTABLE"], *arguments],
        cwd=cwd,
        capture_output=True,
        text=True,
        check=False,
        timeout=10,
    )


@pytest.mark.parametrize("explicit", [False, True])
def test_package_names_become_sentences_without_executing_source(
    tmp_path: Path,
    *,
    explicit: bool,
) -> None:
    """Preserve the test prefix and parse decorators, async tests and test classes."""
    source = (
        "raise RuntimeError('must not execute')\n"
        "@unknown_decorator()\n"
        "def test_saves_valid_input(): pass\n"
        "async def test_async_behavior(): pass\n"
        "def helper():\n    def test_nested(): pass\n"
        "class Helper:\n    def test_hidden(self): pass\n"
        "class TestBehavior:\n    def test_method(self): pass\n"
        "def test_double__underscore(): pass\n"
    )
    package = make_package(tmp_path, "example", source)
    result = run_cli(
        tmp_path if explicit else package,
        *([str(package)] if explicit else []),
    )
    if not (result.returncode == 0):
        raise AssertionError(result.stderr)
    if result.stdout != (
        "test async behavior\ntest double  underscore\ntest method\n"
        "test saves valid input\n"
    ):
        msg = f"Unexpected CLI result: {result}"
        raise AssertionError(msg)
    if result.stderr != "":
        msg = "Expectation failed: result.stderr == ''"
        raise AssertionError(msg)
    if (package / "test_main.py").read_text() != source:
        msg = "Expectation failed: (package / 'test_main.py').read_text() == source"
        raise AssertionError(msg)
    if (tmp_path / "tmp").exists():
        msg = "Expectation failed: not (tmp_path / 'tmp').exists()"
        raise AssertionError(msg)


def test_unittest_aliases_and_local_subclasses_are_recognized(tmp_path: Path) -> None:
    """List methods of statically recognized unittest classes."""
    package = make_package(
        tmp_path,
        "example",
        "import unittest as unit\n"
        "from unittest import TestCase as Case, IsolatedAsyncioTestCase\n"
        "class Reports(unit.TestCase):\n    def test_report(self): pass\n"
        "class Base(Case): pass\n"
        "class Derived(Base):\n    def test_derived(self): pass\n"
        "class AsyncChecks(IsolatedAsyncioTestCase):\n"
        "    async def test_async(self): pass\n",
    )
    result = run_cli(package)
    if not (result.returncode == 0):
        raise AssertionError(result.stderr)
    if result.stdout != "test async\ntest derived\ntest report\n":
        msg = f"Unexpected CLI result: {result}"
        raise AssertionError(msg)


@pytest.mark.parametrize("explicit", [False, True])
def test_repository_groups_sorted_packages_and_skips_untested_packages(
    tmp_path: Path,
    *,
    explicit: bool,
) -> None:
    """Match runner target discovery while reporting sentences by package."""
    make_package(tmp_path, "zebra", "def test_last(): pass\n")
    make_package(tmp_path, "alpha", "def test_first(): pass\n")
    missing = make_package(tmp_path, "untested", "")
    (missing / "test_main.py").unlink()
    (tmp_path / "packages/linked").symlink_to(missing, target_is_directory=True)
    result = run_cli(tmp_path, *([str(tmp_path)] if explicit else []))
    if not (result.returncode == 0):
        raise AssertionError(result.stderr)
    if result.stdout != "packages/alpha:\ntest first\npackages/zebra:\ntest last\n":
        msg = f"Unexpected CLI result: {result}"
        raise AssertionError(msg)
    if result.stderr != "Skipping untested: no test_main.py\n":
        msg = f"Unexpected CLI result: {result}"
        raise AssertionError(msg)


def test_repository_continues_after_a_malformed_test_file(tmp_path: Path) -> None:
    """Report a parse error while still printing later packages."""
    make_package(tmp_path, "broken", "def invalid(")
    make_package(tmp_path, "valid", "def test_still_listed(): pass\n")
    result = run_cli(tmp_path)
    if result.returncode != 1:
        msg = "Expectation failed: result.returncode == 1"
        raise AssertionError(msg)
    if "test still listed\n" not in result.stdout:
        msg = "Expectation failed: 'test still listed\\n' in result.stdout"
        raise AssertionError(msg)
    if "python_test_names: broken:" not in result.stderr:
        msg = "Expectation failed: 'python_test_names: broken:' in result.stderr"
        raise AssertionError(msg)


@pytest.mark.parametrize("layout", ["missing", "syntax", "encoding", "linked"])
def test_invalid_test_files_return_failure(tmp_path: Path, layout: str) -> None:
    """Reject missing, malformed, undecodable and linked source files."""
    package = make_package(tmp_path, "example", "")
    source = package / "test_main.py"
    if layout == "missing":
        source.unlink()
    elif layout == "syntax":
        source.write_text("def invalid(")
    elif layout == "encoding":
        source.write_bytes(b"\xff")
    else:
        source.unlink()
        source.symlink_to(package / "main.py")
    result = run_cli(package)
    if result.returncode != 1:
        msg = "Expectation failed: result.returncode == 1"
        raise AssertionError(msg)
    if "python_test_names:" not in result.stderr:
        msg = "Expectation failed: 'python_test_names:' in result.stderr"
        raise AssertionError(msg)
    if result.stdout != "":
        msg = "Expectation failed: result.stdout == ''"
        raise AssertionError(msg)


def test_empty_test_file_succeeds_without_sentences(tmp_path: Path) -> None:
    """An empty test file has no specifications to print."""
    package = make_package(tmp_path, "example", "")
    result = run_cli(package)
    if result.returncode != 0:
        msg = "Expectation failed: result.returncode == 0"
        raise AssertionError(msg)
    if not (result.stdout == result.stderr == ""):
        msg = "Expectation failed: result.stdout == result.stderr == ''"
        raise AssertionError(msg)


def test_cli_help_invalid_targets_and_unsupported_options(tmp_path: Path) -> None:
    """Share target and help conventions without accepting execution budgets."""
    usage_error = 2
    result = run_cli(tmp_path, "--help")
    if result.returncode != 0:
        msg = "Expectation failed: result.returncode == 0"
        raise AssertionError(msg)
    if "[target]" not in result.stdout:
        msg = "Expectation failed: '[target]' in result.stdout"
        raise AssertionError(msg)
    if "current directory" not in result.stdout:
        msg = "Expectation failed: 'current directory' in result.stdout"
        raise AssertionError(msg)
    if run_cli(tmp_path).returncode != 1:
        msg = "Expectation failed: run_cli(tmp_path).returncode == 1"
        raise AssertionError(msg)
    if run_cli(tmp_path, "--timeout", "60").returncode != usage_error:
        msg = f"Unexpected CLI result: {result}"
        raise AssertionError(msg)
    if run_cli(tmp_path, "--max-examples", "100").returncode != usage_error:
        msg = f"Unexpected CLI result: {result}"
        raise AssertionError(msg)
    (tmp_path / "flake.nix").touch()
    result = run_cli(tmp_path)
    if result.returncode != 1:
        msg = "Expectation failed: result.returncode == 1"
        raise AssertionError(msg)
    if "no Python packages found" not in result.stderr:
        msg = "Expectation failed: 'no Python packages found' in result.stderr"
        raise AssertionError(msg)
