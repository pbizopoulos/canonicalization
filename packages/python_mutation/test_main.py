# Copyright (c) 2026- Paschalis Bizopoulos
"""Integration tests for isolated mutation campaigns."""

from __future__ import annotations

import contextlib
import io
import json
import os
import subprocess
import sys
import tempfile
from collections import Counter
from pathlib import Path
from unittest.mock import patch

import pytest
from hypothesis import example, given
from hypothesis import strategies as st

from packages.python_mutation.main import (
    MutationError,
    campaign,
    copy_sources,
    main,
    nix_string,
    run_command,
    summarize,
    target_root,
)


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


def test_timeout_terminates_process_group(tmp_path: Path) -> None:
    """A stalled command is terminated and its log remains available."""
    with pytest.raises(subprocess.TimeoutExpired):
        run_command(
            [sys.executable, "-c", "import time; time.sleep(30)"],
            tmp_path,
            tmp_path / "timeout.log",
            timeout=0.1,
        )
    if not ((tmp_path / "timeout.log").exists()):
        message = "mutation runner expectation failed"
        raise AssertionError(message)


def test_summary_distinguishes_engine_failures_and_timeouts(tmp_path: Path) -> None:
    """Timeouts remain separate from killed tests and infrastructure failures."""
    rows = [
        [
            {"job_id": "timeout"},
            {"worker_outcome": "normal", "test_outcome": "killed", "output": "timeout"},
        ],
        [
            {"job_id": "failure"},
            {
                "worker_outcome": "exception",
                "test_outcome": "incompetent",
                "output": "failure",
            },
        ],
        [{"job_id": "pending"}, None],
    ]
    (tmp_path / "results.jsonl").write_text(
        "\n".join(json.dumps(row) for row in rows),
        encoding="utf-8",
    )
    if summarize(tmp_path):
        message = "mutation runner expectation failed"
        raise AssertionError(message)
    if json.loads((tmp_path / "summary.json").read_text(encoding="utf-8")) != {
        "timeout": 1,
        "error": 1,
        "pending": 1,
    }:
        message = "mutation runner expectation failed"
        raise AssertionError(message)


def test_copy_preserves_assets_and_excludes_scratch(tmp_path: Path) -> None:
    """Copies support code without recursing into generated scratch trees."""
    root = tmp_path / "source"
    package = make_target(root, "", "")
    for name in ("prm", "tmp", "__pycache__", ".git"):
        (package / name).mkdir()
        (package / name / "asset").write_text("data", encoding="utf-8")
    (root / "prm").mkdir()
    (root / "prm" / "asset").write_text("support", encoding="utf-8")
    workspace = tmp_path / "workspace"
    copy_sources(root, workspace)
    if (workspace / "packages/example/prm/asset").read_text(encoding="utf-8") != "data":
        message = "mutation runner expectation failed"
        raise AssertionError(message)
    if (workspace / "prm/asset").read_text(encoding="utf-8") != "support":
        message = "mutation runner expectation failed"
        raise AssertionError(message)
    for name in ("tmp", "__pycache__", ".git"):
        if (workspace / "packages/example" / name).exists():
            message = "mutation runner expectation failed"
            raise AssertionError(message)
    if target_root(package) != root:
        message = "mutation runner expectation failed"
        raise AssertionError(message)
    (package / "test_main.py").unlink()
    with pytest.raises(MutationError):
        target_root(package)


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


def test_main_retains_workspace_and_propagates_campaign_result(tmp_path: Path) -> None:
    """Command orchestration uses the resolved target environment and status."""
    package = make_target(tmp_path, "", "")
    with (
        patch("sys.argv", ["python_mutation", str(package)]),
        patch(
            "packages.python_mutation.main.build_environment",
            return_value=(sys.executable, "tools"),
        ),
        patch("packages.python_mutation.main.campaign", return_value=False) as run,
        pytest.raises(SystemExit) as error,
    ):
        main()
    if error.value.code != 1:
        message = "mutation runner expectation failed"
        raise AssertionError(message)
    workspace, name, python, tools, timeout = run.call_args.args
    if not (workspace.is_relative_to(tmp_path / "tmp")):
        message = "mutation runner expectation failed"
        raise AssertionError(message)
    if (name, python, tools, timeout) != ("example", sys.executable, "tools", 60):
        message = "mutation runner expectation failed"
        raise AssertionError(message)
    if not ((workspace / "packages/example/main.py").is_file()):
        message = "mutation runner expectation failed"
        raise AssertionError(message)


def test_nix_literal_escapes_interpolation() -> None:
    """Repository paths cannot inject expressions into the generated Nix file."""
    if nix_string('a${b}"c') != '"a\\${b}\\"c"':
        message = "mutation runner expectation failed"
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


_OUTCOMES = st.sampled_from(
    [
        ("killed", "normal", "killed", ""),
        ("survived", "normal", "survived", ""),
        ("timeout", "normal", "killed", "timeout"),
        ("error", "exception", "survived", "timeout"),
        ("error", "normal", "incompetent", "timeout"),
        ("pending", None, "", ""),
    ],
)


@given(
    outcomes=st.lists(_OUTCOMES, max_size=30),
    diff=st.text(alphabet="abc +-\né", max_size=30),
)
@example(
    outcomes=[
        ("survived", "normal", "survived", ""),
        ("error", "exception", "survived", "timeout"),
        ("timeout", "normal", "killed", "timeout"),
        ("pending", None, "", ""),
    ],
    diff="- old\n+ new",
)
@example(outcomes=[], diff="")
def test_summary_matches_labeled_outcomes(
    outcomes: list[tuple[str, str | None, str, str]],
    diff: str,
) -> None:
    """Count each result once, retain survivor diffs, and distinguish engine errors."""
    rows = []
    survivors = []
    counts = Counter(status for status, _, _, _ in outcomes)
    for index, (status, worker, test, output) in enumerate(outcomes):
        mutation_diff = f"{diff}\nmutation {index}"
        result = (
            None
            if worker is None
            else {
                "worker_outcome": worker,
                "test_outcome": test,
                "output": output,
                "diff": mutation_diff,
            }
        )
        rows.append(json.dumps([{"job_id": str(index)}, result]))
        if status == "survived":
            survivors.append(f"Survived {index}:\n{mutation_diff}")
    with tempfile.TemporaryDirectory() as directory:
        workspace = Path(directory)
        (workspace / "results.jsonl").write_text("\n".join(rows), encoding="utf-8")
        report = io.StringIO()
        with contextlib.redirect_stdout(report):
            succeeded = summarize(workspace)
        summary = json.loads((workspace / "summary.json").read_text(encoding="utf-8"))
    if summary != dict(counts) or succeeded != (
        not counts["error"] and not counts["pending"]
    ):
        msg = "mutation results were miscounted or misclassified"
        raise AssertionError(msg)
    header = (
        ", ".join(
            f"{status}: {counts[status]}"
            for status in ("killed", "survived", "timeout", "error", "pending")
        )
        if outcomes
        else "No mutations generated."
    )
    expected_report = header + "\n" + ("\n".join(survivors) + "\n" if survivors else "")
    if report.getvalue() != expected_report:
        msg = "mutation report lost a survivor or reported an error as a survivor"
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


@pytest.mark.parametrize(
    "failure",
    [
        MutationError("environment build failed"),
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
    first = make_target(tmp_path, "", "", name="alpha")
    second = make_target(tmp_path, "", "", name="zeta")
    with (
        patch("sys.argv", ["python_mutation", str(tmp_path)]),
        patch(
            "packages.python_mutation.main.run_package",
            side_effect=[failure, True],
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
        or "python_mutation: alpha:" not in captured.err
    ):
        msg = "repository summary did not identify the failed package"
        raise AssertionError(msg)


def test_repository_interrupt_stops_later_packages(tmp_path: Path) -> None:
    """An interrupt keeps exit code 130 and never starts the next package."""
    make_target(tmp_path, "", "", name="alpha")
    make_target(tmp_path, "", "", name="zeta")
    with (
        patch("sys.argv", ["python_mutation", str(tmp_path)]),
        patch(
            "packages.python_mutation.main.run_package",
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
def test_repository_summarizes_isolated_campaigns_and_survivors(
    tmp_path: Path,
    capsys: pytest.CaptureFixture[str],
    *,
    first_fails: bool,
) -> None:
    """Aggregate campaign failures while successful campaigns may contain survivors."""
    root = tmp_path / "repository with spaces"
    second = make_target(root, "", "", name="zeta")
    make_target(root, "", "", name="alpha")
    skipped = make_target(root, "", "", name="untested")
    (skipped / "test_main.py").unlink()
    (root / "packages" / "linked").symlink_to(second, target_is_directory=True)
    nested = root / "packages" / "nested" / "child"
    nested.mkdir(parents=True)
    (nested / "main.py").write_text("", encoding="utf-8")

    def report_campaign(
        workspace: Path,
        name: str,
        python: str,
        tools: str,
        timeout: float,
    ) -> bool:
        if (python, tools, timeout) != (sys.executable, "tools", 12):
            msg = "per-package campaign settings were lost"
            raise AssertionError(msg)
        outcome = (
            "survived" if name == "zeta" else "incompetent" if first_fails else "killed"
        )
        row = [
            {"job_id": name},
            {
                "worker_outcome": "normal",
                "test_outcome": outcome,
                "output": "",
                "diff": "mutation diff",
            },
        ]
        (workspace / "results.jsonl").write_text(
            json.dumps(row) + "\n",
            encoding="utf-8",
        )
        return summarize(workspace)

    with (
        patch("sys.argv", ["python_mutation", str(root), "--timeout", "12"]),
        patch(
            "packages.python_mutation.main.build_environment",
            return_value=(sys.executable, "tools"),
        ) as build,
        patch(
            "packages.python_mutation.main.campaign",
            side_effect=report_campaign,
        ) as run,
        pytest.raises(SystemExit) as error,
    ):
        main()
    if error.value.code != int(first_fails):
        msg = "repository exit status confused survivors with failures"
        raise AssertionError(msg)
    if [call.args[1] for call in build.call_args_list] != ["alpha", "zeta"] or [
        call.args[1] for call in run.call_args_list
    ] != ["alpha", "zeta"]:
        msg = "package discovery or execution order was incorrect"
        raise AssertionError(msg)
    workspaces = [call.args[2] for call in build.call_args_list]
    if len(set(workspaces)) != len(workspaces):
        msg = "campaigns shared a workspace"
        raise AssertionError(msg)
    summaries = [
        json.loads((workspace / "summary.json").read_text()) for workspace in workspaces
    ]
    if summaries != [{"error" if first_fails else "killed": 1}, {"survived": 1}]:
        msg = "per-package summaries were lost"
        raise AssertionError(msg)
    captured = capsys.readouterr()
    expected = (
        "1 passed, 1 failed, 1 skipped"
        if first_fails
        else "2 passed, 0 failed, 1 skipped"
    )
    if expected not in captured.out or "Survived zeta:" not in captured.out:
        msg = "repository summary or survivor details missing"
        raise AssertionError(msg)
