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


def make_target(root: Path, tests: str) -> Path:
    """Create a minimal target with a CLI and property tests."""
    package = root / "packages" / "example"
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
