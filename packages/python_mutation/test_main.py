# Copyright (c) 2026- Paschalis Bizopoulos
"""Integration tests for isolated mutation campaigns."""

from __future__ import annotations

import json
import os
import subprocess
import sys
from typing import TYPE_CHECKING

import pytest

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


def _prepare_flake(
    root: Path,
    tests: str,
    source: str = "def main():\n    print('ready')\n",
) -> dict[str, str]:
    """Provide an offline flake backed by this check's real Python environment."""
    package = make_target(root, source, tests)
    (package / "main.py").write_text(source, encoding="utf-8")
    dependency = root / "prm/nixpkgs"
    dependency.mkdir(parents=True)
    (dependency / "flake.nix").write_text("{ outputs = _: {}; }", encoding="utf-8")
    (dependency / "default.nix").write_text(
        "_: { lib = { concatMap = f: xs: builtins.concatLists (map f xs); "
        'makeBinPath = _: ""; }; writeText = builtins.toFile; }',
        encoding="utf-8",
    )
    (root / "flake.nix").write_text(
        '{ inputs.nixpkgs.url = "path:./prm/nixpkgs"; '
        "outputs = _: { packages.${builtins.currentSystem}.example.python"
        ".withPackages = "
        f"_ : {json.dumps(sys.prefix)}; }}; }}",
        encoding="utf-8",
    )
    for args in (["init", "--quiet"], ["add", "."]):
        subprocess.run(  # noqa: S603
            ["git", "-C", str(root), *args],  # noqa: S607
            check=True,
            capture_output=True,
            timeout=10,
        )
    environment = dict(os.environ)
    store = root.parent / "nix"
    environment["NIX_REMOTE"] = (
        f"local?store={store / 'store'}&state={store / 'state'}&log={store / 'log'}"
    )
    environment["NIX_CONFIG"] = (
        "experimental-features = nix-command flakes\nbuild-users-group =\n"
    )
    return environment


def _run_cli(
    root: Path,
    environment: dict[str, str],
    *arguments: str,
) -> subprocess.CompletedProcess[str]:
    return subprocess.run(  # noqa: S603
        [os.environ["PACKAGE_E2E_EXECUTABLE"], str(root), *arguments],
        env=environment,
        capture_output=True,
        text=True,
        check=False,
        timeout=120,
    )


def test_campaign_reports_killed_and_surviving_mutations_without_changing_source(
    tmp_path: Path,
) -> None:
    """Discover a package and report mutations exercised by CLI-only tests."""
    root = tmp_path / "source with spaces"
    source = (
        "def value():\n    return 1\n\n"
        "def unused():\n    return 2\n\ndef main():\n    print(value())\n"
    )
    tests = (
        "import os, subprocess\n"
        "def test_cli():\n"
        "    result = subprocess.run([os.environ['PACKAGE_E2E_EXECUTABLE']],\n"
        "        capture_output=True, text=True)\n"
        "    assert result.returncode == 0\n"
        "    assert result.stdout == '1\\n'\n"
    )
    environment = _prepare_flake(root, tests, source)
    result = _run_cli(root, environment, "--timeout", "10")
    if result.returncode or "1 passed, 0 failed, 0 skipped" not in result.stdout:
        raise AssertionError(result.stdout + result.stderr)
    (workspace,) = (root / "tmp").glob("python-mutation-example-*")
    summary = json.loads((workspace / "summary.json").read_text())
    if summary.get("killed", 0) <= 0 or summary.get("survived", 0) <= 0:
        raise AssertionError(summary)
    if not (workspace / "report.html").stat().st_size:
        msg = "the campaign must retain a readable report"
        raise AssertionError(msg)
    if (root / "packages/example/main.py").read_text() != source:
        msg = "mutations must never alter the original source"
        raise AssertionError(msg)


@pytest.mark.parametrize(
    "tests",
    [
        "def test_failure():\n    assert False\n",
        "",
        "raise ImportError('missing dependency')\n",
    ],
)
def test_failing_empty_or_uncollectable_suites_fail_without_publishing_mutation_scores(
    tmp_path: Path,
    tests: str,
) -> None:
    """An invalid baseline cannot produce a misleading mutation report."""
    root = tmp_path / "source"
    environment = _prepare_flake(root, tests)
    result = _run_cli(root, environment)
    (workspace,) = (root / "tmp").glob("python-mutation-example-*")
    if result.returncode != 1 or "baseline.log" not in result.stderr:
        raise AssertionError(result.stdout + result.stderr)
    if (
        not (workspace / "baseline.log").is_file()
        or (workspace / "summary.json").exists()
    ):
        msg = "retain baseline diagnostics without publishing mutation scores"
        raise AssertionError(msg)


def test_a_package_without_mutable_code_completes_with_an_empty_report(
    tmp_path: Path,
) -> None:
    """A passing import-only suite needs no artificial mutations to succeed."""
    root = tmp_path / "source"
    environment = _prepare_flake(
        root,
        "from packages.example import main\ndef test_import():\n"
        "    assert main is not None\n",
        "",
    )
    result = _run_cli(root, environment)
    (workspace,) = (root / "tmp").glob("python-mutation-example-*")
    if result.returncode or json.loads((workspace / "summary.json").read_text()) != {}:
        raise AssertionError(result.stdout + result.stderr)


@pytest.mark.parametrize("target_directory", [".", "packages/example"])
def test_omitted_target_runs_current_directory(
    tmp_path: Path,
    target_directory: str,
) -> None:
    """Run the current package or repository without an explicit target."""
    root = tmp_path / "source with spaces"
    environment = _prepare_flake(
        root,
        "from packages.example import main\ndef test_import():\n"
        "    assert main is not None\n",
        "",
    )
    result = subprocess.run(  # noqa: S603
        [os.environ["PACKAGE_E2E_EXECUTABLE"]],
        cwd=root / target_directory,
        env=environment,
        capture_output=True,
        text=True,
        check=False,
        timeout=120,
    )
    if result.returncode:
        raise AssertionError(result.stdout + result.stderr)
    if not (root / "tmp").is_dir():
        msg = "current-directory run did not retain diagnostics"
        raise AssertionError(msg)
    if target_directory == "." and "1 passed, 0 failed, 0 skipped" not in result.stdout:
        raise AssertionError(result.stdout)


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
