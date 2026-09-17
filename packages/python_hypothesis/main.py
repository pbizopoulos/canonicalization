#!/usr/bin/env python3
# Copyright (c) 2026- Paschalis Bizopoulos
"""Run Hypothesis against isolated copies of canonical Python packages."""

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
from pathlib import Path


class HypothesisError(Exception):
    """An invalid target or unsuccessful property test run."""


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
        raise HypothesisError(message)
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
        raise HypothesisError(message)


def nix_string(value: str) -> str:
    """Quote literal strings without enabling Nix interpolation."""
    return json.dumps(value).replace("${", r"\${")


def build_environment(root: Path, name: str, workspace: Path) -> tuple[str, str]:
    """Build a target-specific interpreter and resolve its external tools."""
    expression = workspace / "environment.nix"
    expression.write_text(
        "let\n"
        f"  flake = builtins.getFlake {nix_string('git+file://' + str(root))};\n"
        "  system = builtins.currentSystem;\n"
        "  pkgs = import flake.inputs.nixpkgs { inherit system; };\n"
        f"  package = flake.packages.${{system}}.${{{nix_string(name)}}};\n"
        "  dependencies = pkgs.lib.concatMap (name: package.${name} or []) [\n"
        '    "buildInputs" "checkInputs" "nativeBuildInputs" "nativeCheckInputs"\n'
        '    "propagatedBuildInputs" "propagatedNativeBuildInputs"\n'
        "  ];\n"
        "  python = package.python.withPackages (ps:\n"
        "    (package.propagatedBuildInputs or []) ++ [ps.hypothesis ps.pytest]);\n"
        'in pkgs.writeText "hypothesis-environment.json" (builtins.toJSON {\n'
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
        if line.startswith("/nix/store/")
    ]
    if len(paths) != 1:
        message = f"could not resolve target environment; see {log}"
        raise HypothesisError(message)
    environment = json.loads(Path(paths[0]).read_text(encoding="utf-8"))
    return str(environment["python"]), str(environment["path"])


def prepare_tests(
    workspace: Path,
    name: str,
    python: str,
    tool_path: str,
    max_examples: int,
) -> list[str]:
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
        "    from hypothesis import settings\n"
        f'    settings.register_profile("ondemand", max_examples={max_examples},'
        " deadline=None)\n"
        '    settings.load_profile("ondemand")\n'
        "    import pytest\n"
        "    sys.exit(pytest.main(['-p', 'no:cacheprovider',\n"
        f"        '-p', '_hypothesis_pytestplugin', '--hypothesis-show-statistics',\n"
        f"        '--import-mode=importlib', '-q', 'packages/{name}/test_main.py']))\n"
        "finally:\n"
        "    pid.unlink(missing_ok=True)\n",
        encoding="utf-8",
    )
    return [python, "-B", str(bootstrap)]


def main() -> None:
    """Run property tests with a suite timeout and retain diagnostics."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("package", type=Path, help="canonical packages/NAME directory")
    parser.add_argument(
        "--max-examples",
        type=int,
        default=100,
        help="successful generated examples per property (default: 100)",
    )
    parser.add_argument(
        "--timeout",
        type=float,
        default=60.0,
        help="seconds for the test suite, excluding environment build (default: 60)",
    )
    args = parser.parse_args()
    if args.max_examples <= 0:
        parser.error("--max-examples must be positive")
    if not math.isfinite(args.timeout) or args.timeout <= 0:
        parser.error("--timeout must be positive and finite")
    try:
        package = args.package.resolve()
        root = target_root(package)
        scratch = root / "tmp"
        scratch.mkdir(exist_ok=True)
        workspace = Path(
            tempfile.mkdtemp(prefix=f"python-hypothesis-{package.name}-", dir=scratch),
        )
        sys.stdout.write(f"Hypothesis workspace and logs: {workspace}\n")
        sys.stdout.flush()
        copy_sources(root, workspace)
        python, tool_path = build_environment(root, package.name, workspace)
        command = prepare_tests(
            workspace,
            package.name,
            python,
            tool_path,
            args.max_examples,
        )
        log = workspace / "tests.log"
        try:
            run_command(command, workspace, log, timeout=args.timeout)
        finally:
            if log.exists():
                sys.stdout.write(log.read_text(encoding="utf-8"))
    except (HypothesisError, OSError, subprocess.TimeoutExpired) as error:
        sys.stderr.write(f"python_hypothesis: {error}\n")
        sys.exit(1)
    except KeyboardInterrupt:
        sys.stderr.write("python_hypothesis: interrupted; diagnostics retained\n")
        sys.exit(130)


if __name__ == "__main__":
    main()
