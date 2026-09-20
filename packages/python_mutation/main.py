#!/usr/bin/env python3
# Copyright (c) 2026- Paschalis Bizopoulos
"""Run Cosmic Ray against isolated copies of canonical Python packages."""

from __future__ import annotations

import argparse
import contextlib
import json
import math
import os
import shlex
import shutil
import signal
import subprocess
import sys
import tempfile
from collections import Counter
from pathlib import Path


class MutationError(Exception):
    """An invalid target or unsuccessful mutation campaign."""


def target_root(package: Path) -> Path:
    """Validate the canonical target and return its flake root."""
    root = package.parent.parent
    if (
        package.parent.name != "packages"
        or not (root / "flake.nix").is_file()
        or not package.name.isidentifier()
        or not all(
            (package / name).is_file()
            for name in ("default.nix", "main.py", "test_main.py")
        )
    ):
        message = (
            "expected a canonical packages/NAME with default.nix, main.py "
            "and test_main.py inside a flake"
        )
        raise MutationError(message)
    return root


def copy_sources(root: Path, workspace: Path) -> None:
    """Copy package sources and supporting assets without scratch or metadata."""
    ignored = shutil.ignore_patterns(
        "tmp",
        ".git",
        "__pycache__",
        ".pytest_cache",
        ".mypy_cache",
        ".ruff_cache",
        "*.pyc",
    )
    for package in sorted((root / "packages").iterdir()):
        if package.is_dir() and not package.is_symlink():
            shutil.copytree(
                package,
                workspace / "packages" / package.name,
                ignore=ignored,
            )
    if (root / "prm").is_dir():
        shutil.copytree(root / "prm", workspace / "prm", ignore=ignored)


def stop_group(pid: int) -> None:
    """Terminate a subprocess session, including its descendants."""
    with contextlib.suppress(ProcessLookupError):
        os.killpg(pid, signal.SIGKILL)


def run_command(
    command: list[str],
    workspace: Path,
    log: Path,
    *,
    timeout: float | None = None,
) -> None:
    """Capture command output and clean up subprocess groups on interruption."""
    with (
        log.open("w", encoding="utf-8") as output,
        subprocess.Popen(  # noqa: S603
            command,
            cwd=workspace,
            stdout=output,
            stderr=subprocess.STDOUT,
            start_new_session=True,
        ) as process,
    ):
        try:
            code = process.wait(timeout=timeout)
        except (KeyboardInterrupt, subprocess.TimeoutExpired):
            active = workspace / "active-test-pgid"
            if active.exists():
                stop_group(int(active.read_text(encoding="utf-8")))
            stop_group(process.pid)
            process.wait()
            raise
    if code:
        message = f"command failed ({code}); see {log}"
        raise MutationError(message)


def nix_string(value: str) -> str:
    """Quote literal strings without enabling Nix interpolation."""
    return json.dumps(value).replace("${", r"\${")


def build_environment(root: Path, name: str, workspace: Path) -> tuple[str, str]:
    """Build a target-specific interpreter and resolve its external tools."""
    expression = workspace / "environment.nix"
    expression.write_text(
        "let\n"
        f"  flake = builtins.getFlake {nix_string('git+' + root.as_uri())};\n"
        "  system = builtins.currentSystem;\n"
        "  pkgs = import flake.inputs.nixpkgs { inherit system; };\n"
        f"  package = flake.packages.${{system}}.${{{nix_string(name)}}};\n"
        "  dependencies = pkgs.lib.concatMap (name: package.${name} or []) [\n"
        '    "buildInputs" "checkInputs" "nativeBuildInputs" "nativeCheckInputs"\n'
        '    "propagatedBuildInputs" "propagatedNativeBuildInputs"\n'
        "  ];\n"
        "  python = package.python.withPackages (ps:\n"
        "    (package.propagatedBuildInputs or []) ++ [ps.hypothesis ps.pytest]);\n"
        'in pkgs.writeText "mutation-environment.json" (builtins.toJSON {\n'
        '  python = "${python}/bin/python";\n'
        "  path = pkgs.lib.makeBinPath dependencies;\n"
        "})\n",
        encoding="utf-8",
    )
    log = workspace / "environment.log"
    run_command(
        [
            "nix",
            "build",
            "--impure",
            "--no-link",
            "--print-out-paths",
            "--file",
            str(expression),
        ],
        workspace,
        log,
    )
    paths = [
        line
        for line in log.read_text(encoding="utf-8").splitlines()
        if Path(line).is_absolute() and Path(line).is_file()
    ]
    if len(paths) != 1:
        message = f"could not resolve target environment; see {log}"
        raise MutationError(message)
    environment = json.loads(Path(paths[0]).read_text(encoding="utf-8"))
    return str(environment["python"]), str(environment["path"])


def prepare_tests(workspace: Path, name: str, python: str, tool_path: str) -> list[str]:
    """Make both pytest and the CLI executable import the workspace source."""
    launcher = workspace / "package-executable"
    launcher.write_text(
        "#!/bin/sh\nexec "
        + shlex.join([python, str(workspace / "package-entry.py")])
        + ' "$@"\n',
        encoding="utf-8",
    )
    launcher.chmod(0o755)
    (workspace / "package-entry.py").write_text(
        f"from packages.{name}.main import main\nmain()\n",
        encoding="utf-8",
    )
    bootstrap = workspace / "run-tests.py"
    bootstrap.write_text(
        "import os, sys\n"
        "from pathlib import Path\n"
        "os.dup2(1, 2)\n"
        f"os.environ['PACKAGE_E2E_EXECUTABLE'] = {str(launcher)!r}\n"
        f"tools = {tool_path!r}\n"
        "os.environ['PATH'] = tools + os.pathsep + os.environ.get('PATH', '')\n"
        "os.environ['PYTHONDONTWRITEBYTECODE'] = '1'\n"
        "os.environ.pop('PYTHONPATH', None)\n"
        "os.environ.pop('PYTEST_ADDOPTS', None)\n"
        "os.environ['PYTEST_DISABLE_PLUGIN_AUTOLOAD'] = '1'\n"
        "pid = Path('active-test-pgid')\n"
        "pid.write_text(str(os.getpgrp()))\n"
        "try:\n"
        "    from hypothesis import Phase, settings\n"
        '    settings.register_profile("coverage", phases=[Phase.explicit])\n'
        '    settings.load_profile("coverage")\n'
        "    import pytest\n"
        "    sys.exit(pytest.main(['-p', 'no:cacheprovider',\n"
        f"        '--import-mode=importlib', '-q', 'packages/{name}/test_main.py']))\n"
        "finally:\n"
        "    pid.unlink(missing_ok=True)\n",
        encoding="utf-8",
    )
    return [python, "-B", str(bootstrap)]


def summarize(workspace: Path) -> bool:
    """Report engine outcomes without treating survivors as command failures."""
    counts: Counter[str] = Counter()
    survivors: list[str] = []
    for line in (workspace / "results.jsonl").read_text(encoding="utf-8").splitlines():
        item, result = json.loads(line)
        if result is None:
            status = "pending"
        elif (
            result["worker_outcome"] != "normal"
            or result["test_outcome"] == "incompetent"
        ):
            status = "error"
        elif result["output"] == "timeout":
            status = "timeout"
        else:
            status = result["test_outcome"]
        counts[status] += 1
        if status == "survived":
            survivors.append(f"Survived {item['job_id']}:\n{result['diff']}")
    summary = dict(counts)
    (workspace / "summary.json").write_text(
        json.dumps(summary, indent=2) + "\n",
        encoding="utf-8",
    )
    if not counts:
        sys.stdout.write("No mutations generated.\n")
    else:
        statuses = ("killed", "survived", "timeout", "error", "pending")
        sys.stdout.write(", ".join(f"{key}: {counts[key]}" for key in statuses) + "\n")
    if survivors:
        sys.stdout.write("\n".join(survivors) + "\n")
    return not (counts["error"] or counts["pending"])


def campaign(
    workspace: Path,
    name: str,
    python: str,
    tool_path: str,
    timeout: float,
) -> bool:
    """Baseline, mutate, and report one copied package."""
    command = prepare_tests(workspace, name, python, tool_path)
    sys.stdout.write("Running baseline tests...\n")
    sys.stdout.flush()
    run_command(command, workspace, workspace / "baseline.log", timeout=timeout)
    config = workspace / "cosmic-ray.toml"
    config.write_text(
        "[cosmic-ray]\n"
        f"module-path = {json.dumps('packages/' + name + '/main.py')}\n"
        f"timeout = {timeout}\n"
        "excluded-modules = []\n"
        f"test-command = {json.dumps(shlex.join(command))}\n"
        '[cosmic-ray.distributor]\nname = "local"\n',
        encoding="utf-8",
    )
    engine = ["cosmic-ray"]
    session = str(workspace / "session.sqlite")
    run_command(
        [*engine, "init", str(config), session],
        workspace,
        workspace / "init.log",
    )
    sys.stdout.write("Running mutations...\n")
    sys.stdout.flush()
    run_command(
        [*engine, "exec", str(config), session],
        workspace,
        workspace / "engine.log",
    )
    run_command([*engine, "dump", session], workspace, workspace / "results.jsonl")
    run_command(
        ["cr-html", session],
        workspace,
        workspace / "report.html",
    )
    return summarize(workspace)


def run_package(package: Path, timeout: float) -> bool:
    """Run one package in its own environment and retain diagnostics."""
    root = target_root(package)
    scratch = root / "tmp"
    scratch.mkdir(exist_ok=True)
    workspace = Path(
        tempfile.mkdtemp(prefix=f"python-mutation-{package.name}-", dir=scratch),
    )
    sys.stdout.write(f"Mutation workspace and reports: {workspace}\n")
    sys.stdout.flush()
    copy_sources(root, workspace)
    python, tool_path = build_environment(root, package.name, workspace)
    return campaign(workspace, package.name, python, tool_path, timeout)


def run_repository(root: Path, timeout: float) -> bool:
    """Run each Python package, continuing after failures and summarizing results."""
    directory = root / "packages"
    packages = (
        sorted(
            path
            for path in directory.iterdir()
            if path.is_dir() and not path.is_symlink() and (path / "main.py").is_file()
        )
        if directory.is_dir()
        else []
    )
    if not packages:
        message = f"no Python packages found under {directory}"
        raise MutationError(message)
    outcomes: dict[str, str] = {}
    for package in packages:
        if not (package / "test_main.py").is_file():
            outcomes[package.name] = "skipped"
            sys.stdout.write(f"Skipping {package.name}: no test_main.py\n")
            continue
        sys.stdout.write(f"Running {package.name}...\n")
        sys.stdout.flush()
        try:
            outcomes[package.name] = (
                "passed" if run_package(package, timeout) else "failed"
            )
        except (MutationError, OSError, subprocess.TimeoutExpired) as error:
            outcomes[package.name] = "failed"
            sys.stderr.write(f"python_mutation: {package.name}: {error}\n")
    sys.stdout.write("\nRepository summary:\n")
    for package_name, status in outcomes.items():
        sys.stdout.write(f"  {package_name}: {status}\n")
    sys.stdout.write(
        ", ".join(
            f"{sum(value == status for value in outcomes.values())} {status}"
            for status in ("passed", "failed", "skipped")
        )
        + "\n",
    )
    return "failed" not in outcomes.values()


def main() -> None:
    """Run an explicit mutation campaign and preserve its diagnostics."""
    parser = argparse.ArgumentParser(
        description=__doc__,
        epilog=(
            "Repository targets run Python packages sequentially, skip packages "
            "without test_main.py, and summarize all results."
        ),
    )
    parser.add_argument(
        "target",
        type=Path,
        nargs="?",
        default=Path(),
        help=(
            "canonical packages/NAME directory or flake repository root "
            "(default: current directory)"
        ),
    )
    parser.add_argument(
        "--timeout",
        type=float,
        default=60.0,
        help=(
            "seconds per test-suite invocation, excluding environment build "
            "(default: 60)"
        ),
    )
    args = parser.parse_args()
    if not math.isfinite(args.timeout) or args.timeout <= 0:
        parser.error("--timeout must be positive and finite")
    try:
        target = args.target.resolve()
        if (target / "flake.nix").is_file():
            success = run_repository(target, args.timeout)
        else:
            success = run_package(target, args.timeout)
    except (MutationError, OSError, subprocess.TimeoutExpired) as error:
        sys.stderr.write(f"python_mutation: {error}\n")
        sys.exit(1)
    except KeyboardInterrupt:
        sys.stderr.write("python_mutation: interrupted; diagnostics retained\n")
        sys.exit(130)
    sys.exit(0 if success else 1)


if __name__ == "__main__":
    main()
