# Copyright (c) 2026- Paschalis Bizopoulos
"""Integration tests for explicit and generated property test runs."""

from __future__ import annotations

import os
import subprocess
import sys
import tempfile
from pathlib import Path
from unittest.mock import patch

import pytest
from hypothesis import example, given
from hypothesis import strategies as st

from packages.python_hypothesis.main import (
    HypothesisError,
    copy_sources,
    main,
    prepare_tests,
    run_command,
    target_root,
)


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


def test_generated_examples_and_workspace_cli(tmp_path: Path) -> None:
    """The runner generates examples and executes the copied package CLI."""
    package = make_target(
        tmp_path / "source with spaces",
        "import os, subprocess\n"
        "from hypothesis import given, example, strategies as st\n"
        "from pathlib import Path\n"
        "@given(st.integers())\n"
        "@example(0)\n"
        "def test_property(value):\n"
        "    with Path('examples').open('a') as output:\n"
        "        output.write(str(value) + '\\n')\n"
        "def test_cli():\n"
        "    result = subprocess.run([os.environ['PACKAGE_E2E_EXECUTABLE']],\n"
        "        capture_output=True, text=True)\n"
        "    assert result.returncode == 0\n"
        "    assert result.stdout == 'ready\\n'\n",
    )
    workspace = tmp_path / "workspace with spaces"
    copy_sources(target_root(package), workspace)
    command = prepare_tests(workspace, "example", sys.executable, "", 5)
    run_command(command, workspace, workspace / "tests.log", timeout=20)
    expected_examples = 6
    if len((workspace / "examples").read_text().splitlines()) != expected_examples:
        message = "expected one explicit and five generated examples"
        raise AssertionError(message)
    if "5 passing examples" not in (workspace / "tests.log").read_text():
        message = "Hypothesis statistics missing"
        raise AssertionError(message)
    if (package / "examples").exists():
        message = "source tree was modified"
        raise AssertionError(message)


def test_failures_and_timeouts_retain_logs(tmp_path: Path) -> None:
    """Failures propagate and a stalled suite is terminated."""
    package = make_target(
        tmp_path / "source",
        "from hypothesis import given, strategies as st\n"
        "@given(st.integers())\n"
        "def test_failure(value):\n    assert value != 0\n",
    )
    workspace = tmp_path / "workspace"
    copy_sources(target_root(package), workspace)
    command = prepare_tests(workspace, "example", sys.executable, "", 5)
    with pytest.raises(HypothesisError):
        run_command(command, workspace, workspace / "tests.log", timeout=20)
    if "Falsifying example" not in (workspace / "tests.log").read_text():
        message = "counterexample missing from retained log"
        raise AssertionError(message)
    with pytest.raises(subprocess.TimeoutExpired):
        run_command(
            [sys.executable, "-c", "import time; time.sleep(30)"],
            workspace,
            workspace / "timeout.log",
            timeout=0.1,
        )


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


def test_main_builds_environment_and_preserves_results(tmp_path: Path) -> None:
    """The CLI applies budgets and preserves its isolated results."""
    package = make_target(tmp_path, "def test_ok():\n    pass\n")
    with (
        patch("sys.argv", ["python_hypothesis", str(package), "--max-examples", "7"]),
        patch(
            "packages.python_hypothesis.main.build_environment",
            return_value=(sys.executable, ""),
        ),
    ):
        main()
    logs = list((tmp_path / "tmp").glob("python-hypothesis-*/tests.log"))
    if len(logs) != 1 or "1 passed" not in logs[0].read_text():
        message = "successful run did not retain its report"
        raise AssertionError(message)


_COPY_COMPONENT = st.sampled_from(
    [
        ("prm", True),
        ("assets", True),
        ("tmp_extra", True),
        ("tmp", False),
        (".git", False),
        ("__pycache__", False),
        (".pytest_cache", False),
        (".mypy_cache", False),
        (".ruff_cache", False),
    ],
)


@given(
    records=st.lists(
        st.tuples(
            st.sampled_from(["packages/example", "packages/other", "prm"]),
            st.lists(_COPY_COMPONENT, max_size=4),
            st.binary(max_size=40),
            st.booleans(),
        ),
        max_size=15,
    ),
)
@example(
    records=[
        ("packages/example", [("prm", True)], b"asset", False),
        ("packages/example", [("prm", True), ("tmp", False)], b"scratch", False),
        ("prm", [("assets", True)], b"cache", True),
        ("packages/other", [("tmp_extra", True)], b"source", False),
    ],
)
@example(records=[])
def test_copied_workspace_matches_source_manifest(
    records: list[tuple[str, list[tuple[str, bool]], bytes, bool]],
) -> None:
    """Copy support assets exactly while excluding metadata at every depth."""
    with tempfile.TemporaryDirectory() as directory:
        root = Path(directory)
        source = root / "source"
        (source / "packages" / "example").mkdir(parents=True)
        original: dict[Path, bytes] = {}
        expected: dict[Path, bytes] = {}
        for index, (base, components, contents, cached) in enumerate(records):
            relative = Path(base).joinpath(
                *(name for name, _ in components),
                f"file_{index}" + (".pyc" if cached else ".bin"),
            )
            path = source / relative
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_bytes(contents)
            original[relative] = contents
            if not cached and all(keep for _, keep in components):
                expected[relative] = contents
        external = root / "external"
        external.mkdir()
        (external / "secret").write_bytes(b"do not copy")
        (source / "packages" / "linked").symlink_to(external, target_is_directory=True)
        workspace = root / "workspace"
        copy_sources(source, workspace)
        copied = {
            path.relative_to(workspace): path.read_bytes()
            for path in workspace.rglob("*")
            if path.is_file()
        }
        if copied != expected:
            msg = "copied files differ from the expected source manifest"
            raise AssertionError(msg)
        for relative in expected:
            (workspace / relative).write_bytes(b"changed in workspace")
        remaining = {
            path.relative_to(source): path.read_bytes()
            for path in source.rglob("*")
            if path.is_file()
        }
        if (
            remaining != original
            or (external / "secret").read_bytes() != b"do not copy"
        ):
            msg = "workspace changes affected the original source"
            raise AssertionError(msg)


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


@pytest.mark.parametrize(
    "failure",
    [
        HypothesisError("environment build failed"),
        OSError("unreadable source"),
        subprocess.TimeoutExpired("tests", 1),
    ],
)
def test_repository_continues_after_package_errors(
    tmp_path: Path,
    capsys: pytest.CaptureFixture[str],
    failure: Exception,
) -> None:
    """Build, filesystem, and timeout failures do not prevent later packages running."""
    first = make_target(tmp_path, "", name="alpha")
    second = make_target(tmp_path, "", name="zeta")
    with (
        patch("sys.argv", ["python_hypothesis", str(tmp_path)]),
        patch(
            "packages.python_hypothesis.main.run_package",
            side_effect=[failure, None],
        ) as run,
        pytest.raises(SystemExit) as error,
    ):
        main()
    if error.value.code != 1 or [call.args[0] for call in run.call_args_list] != [
        first,
        second,
    ]:
        msg = "repository stopped early or hid a package failure"
        raise AssertionError(msg)
    captured = capsys.readouterr()
    if (
        "1 passed, 1 failed, 0 skipped" not in captured.out
        or "python_hypothesis: alpha:" not in captured.err
    ):
        msg = "repository summary did not identify the failed package"
        raise AssertionError(msg)


def test_repository_interrupt_stops_later_packages(tmp_path: Path) -> None:
    """An interrupt keeps exit code 130 and never starts the next package."""
    make_target(tmp_path, "", name="alpha")
    make_target(tmp_path, "", name="zeta")
    with (
        patch("sys.argv", ["python_hypothesis", str(tmp_path)]),
        patch(
            "packages.python_hypothesis.main.run_package",
            side_effect=KeyboardInterrupt,
        ) as run,
        pytest.raises(SystemExit) as error,
    ):
        main()
    interrupted_exit_code = 130
    if error.value.code != interrupted_exit_code or run.call_count != 1:
        msg = "repository continued after an interrupt"
        raise AssertionError(msg)


@pytest.mark.parametrize("first_fails", [False, True])
def test_repository_runs_isolated_suites_and_summarizes(
    tmp_path: Path,
    capsys: pytest.CaptureFixture[str],
    *,
    first_fails: bool,
) -> None:
    """Keep per-package budgets and later successful suites after a failure."""
    root = tmp_path / "repository with spaces"
    second = make_target(
        root,
        "from hypothesis import given, strategies as st\n"
        "@given(st.integers())\n"
        "def test_property(value):\n    pass\n",
        name="zeta",
    )
    make_target(
        root,
        f"def test_first():\n    assert {not first_fails}\n",
        name="alpha",
    )
    skipped = make_target(root, "", name="untested")
    (skipped / "test_main.py").unlink()
    (root / "packages" / "linked").symlink_to(second, target_is_directory=True)
    nested = root / "packages" / "nested" / "child"
    nested.mkdir(parents=True)
    (nested / "main.py").write_text("", encoding="utf-8")
    code: int | str | None = 0
    timeout = 20
    with (
        patch(
            "sys.argv",
            ["python_hypothesis", str(root), "--max-examples", "3", "--timeout", "20"],
        ),
        patch(
            "packages.python_hypothesis.main.build_environment",
            return_value=(sys.executable, ""),
        ) as build,
        patch("packages.python_hypothesis.main.run_command", wraps=run_command) as run,
    ):
        try:
            main()
        except SystemExit as error:
            code = error.code
    if code != int(first_fails):
        msg = "repository exit status did not reflect its suites"
        raise AssertionError(msg)
    if [call.args[1] for call in build.call_args_list] != ["alpha", "zeta"]:
        msg = "package discovery or execution order was incorrect"
        raise AssertionError(msg)
    workspaces = [call.args[2] for call in build.call_args_list]
    if len(set(workspaces)) != len(workspaces) or any(
        call.kwargs["timeout"] != timeout for call in run.call_args_list
    ):
        msg = "package isolation or suite budgets were lost"
        raise AssertionError(msg)
    logs = [(workspace / "tests.log").read_text() for workspace in workspaces]
    if (
        "3 passing examples" not in logs[1]
        or ("1 failed" if first_fails else "1 passed") not in logs[0]
    ):
        msg = "per-package results or generated-example budgets were lost"
        raise AssertionError(msg)
    captured = capsys.readouterr()
    expected = (
        "1 passed, 1 failed, 1 skipped"
        if first_fails
        else "2 passed, 0 failed, 1 skipped"
    )
    if (
        expected not in captured.out
        or "Skipping untested: no test_main.py" not in captured.out
    ):
        msg = "repository summary did not describe every package"
        raise AssertionError(msg)
