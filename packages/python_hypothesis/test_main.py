# Copyright (c) 2026- Paschalis Bizopoulos
"""Integration tests for explicit and generated property test runs."""

from __future__ import annotations

import json
import os
import subprocess
import sys
from typing import TYPE_CHECKING

import pytest

if TYPE_CHECKING:
    from pathlib import Path


def make_target(root: Path, tests: str, *, name: str = "example") -> Path:
    """Create a minimal target with a CLI and property tests."""
    package = root / "packages" / name
    package.mkdir(parents=True)
    (root / "flake.nix").write_text("{}", encoding="utf-8")
    (package / "default.nix").write_text("{}", encoding="utf-8")
    (package / "main.py").write_text(
        "def main():\n    print('ready')\n",
        encoding="utf-8",
    )
    (package / "test_main.py").write_text(tests, encoding="utf-8")
    return package


def _prepare_flake(
    root: Path,
    tests: str,
    source: str = "def main():\n    print('ready')\n",
) -> dict[str, str]:
    """Provide an offline flake backed by this check's real Python environment."""
    package = make_target(root, tests)
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


def test_repository_run_generates_examples_executes_the_package_and_preserves_source(
    tmp_path: Path,
) -> None:
    """Discover a package, generate examples, and retain diagnostics through the CLI."""
    root = tmp_path / "source with spaces"
    tests = (
        "import os, subprocess\n"
        "from hypothesis import given, example, strategies as st\n"
        "from pathlib import Path\n"
        "@given(st.integers())\n@example(0)\n"
        "def test_property(value):\n"
        "    with Path('examples').open('a') as output:\n"
        "        output.write(str(value) + '\\n')\n"
        "def test_cli():\n"
        "    result = subprocess.run([os.environ['PACKAGE_E2E_EXECUTABLE']],\n"
        "        capture_output=True, text=True)\n"
        "    assert result.returncode == 0\n"
        "    assert result.stdout == 'ready\\n'\n"
    )
    environment = _prepare_flake(root, tests)
    original = (root / "packages/example/main.py").read_bytes()
    result = _run_cli(root, environment, "--max-examples", "5")
    if result.returncode or "1 passed, 0 failed, 0 skipped" not in result.stdout:
        raise AssertionError(result.stdout + result.stderr)
    (workspace,) = (root / "tmp").glob("python-hypothesis-example-*")
    expected_examples = 6
    if len((workspace / "examples").read_text().splitlines()) != expected_examples:
        msg = "one explicit and five generated examples must run"
        raise AssertionError(msg)
    if "5 passing examples" not in (workspace / "tests.log").read_text():
        msg = "retained diagnostics must include Hypothesis statistics"
        raise AssertionError(msg)
    if (root / "packages/example/main.py").read_bytes() != original or (
        root / "packages/example/examples"
    ).exists():
        msg = "running tests must leave the source untouched"
        raise AssertionError(msg)


@pytest.mark.parametrize(
    ("tests", "timeout", "diagnostic"),
    [
        (
            (
                "from hypothesis import given, strategies as st\n"
                "@given(st.integers())\n"
                "def test_failure(value):\n    assert value != 0\n"
            ),
            "20",
            "Falsifying example",
        ),
        ("import time\ndef test_stalled():\n    time.sleep(30)\n", "0.5", "timed out"),
    ],
)
def test_failed_or_stalled_suites_return_failure_and_retain_diagnostics(
    tmp_path: Path,
    tests: str,
    timeout: str,
    diagnostic: str,
) -> None:
    """Fail with retained diagnostics for counterexamples and stalled suites."""
    root = tmp_path / "source"
    environment = _prepare_flake(root, tests)
    result = _run_cli(root, environment, "--timeout", timeout)
    (workspace,) = (root / "tmp").glob("python-hypothesis-example-*")
    if result.returncode != 1 or "0 passed, 1 failed, 0 skipped" not in result.stdout:
        raise AssertionError(result.stdout + result.stderr)
    if (
        diagnostic
        not in result.stdout + result.stderr + (workspace / "tests.log").read_text()
    ):
        msg = "failure diagnostics were not retained"
        raise AssertionError(msg)


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


def test_cli_validation(tmp_path: Path) -> None:
    """The installed command validates its target and resource budgets."""
    for arguments, code in [
        (["--help"], 0),
        ([str(tmp_path)], 1),
        ([str(tmp_path), "--timeout", "nan"], 2),
        ([str(tmp_path), "--timeout", "0"], 2),
        ([str(tmp_path), "--max-examples", "0"], 2),
    ]:
        result = subprocess.run(  # noqa: S603
            [os.environ["PACKAGE_E2E_EXECUTABLE"], *arguments],
            capture_output=True,
            text=True,
            check=False,
        )
        if result.returncode != code:
            raise AssertionError(result.stderr)


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
        package = make_target(tmp_path, "")
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
