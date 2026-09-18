# Copyright (c) 2026- Paschalis Bizopoulos
"""Exercise resource management and convergence through the installed CLI."""

from __future__ import annotations

import os
import subprocess
from typing import TYPE_CHECKING

import pytest

if TYPE_CHECKING:
    from pathlib import Path


def _run(
    root: Path,
    *arguments: str,
    code: int = 0,
) -> subprocess.CompletedProcess[str]:
    result = subprocess.run(  # noqa: S603
        [os.environ["PACKAGE_E2E_EXECUTABLE"], *arguments],
        cwd=root,
        capture_output=True,
        text=True,
        check=False,
        timeout=30,
    )
    if result.returncode != code:
        raise AssertionError(result.stdout + result.stderr)
    return result


def _git(root: Path, *arguments: str) -> str:
    return subprocess.run(  # noqa: S603
        ["git", *arguments],  # noqa: S607
        cwd=root,
        capture_output=True,
        text=True,
        check=True,
        timeout=10,
    ).stdout


@pytest.fixture
def repository(tmp_path: Path) -> Path:
    """Create an isolated indexed flake without contacting a forge."""
    _git(tmp_path, "init", "--quiet")
    for name in (".gitignore", "flake.nix", "flake.lock", "README"):
        (tmp_path / name).write_text("", encoding="utf-8")
    _git(tmp_path, "add", ".")
    return tmp_path


def test_package_and_host_lifecycle(repository: Path) -> None:
    """Create, rename, and remove resources with their generated checks and assets."""
    root = repository
    _run(root, "add", "packages/report", "python", 'A "quoted" report.')
    package = root / "packages/report"
    tests = package / "test_main.py"
    if tests.exists() or (root / "checks/report_coverage").exists():
        message = "untested packages should not receive tests or coverage checks"
        raise AssertionError(message)
    resource = package / "prm/nested/asset.txt"
    resource.parent.mkdir(parents=True)
    resource.write_text("keep this resource", encoding="utf-8")
    tests.write_text("def test_result():\n    pass\n", encoding="utf-8")
    _run(root, "add", "hosts/demoHost")
    _run(root, "canonicalize")
    tracked = _git(root, "ls-files").splitlines()
    for name in (
        "packages/report/test_main.py",
        "packages/report/prm/nested/asset.txt",
        "checks/report_coverage/default.nix",
        "checks/demoHostVmWithDisko/default.nix",
    ):
        if name not in tracked:
            raise AssertionError(name)
    _run(root, "mv", "packages/report", "packages/renamed", "--dry-run")
    if not package.is_dir() or (root / "packages/renamed").exists():
        message = "dry-run moved a package"
        raise AssertionError(message)
    _run(root, "mv", "packages/report", "packages/renamed")
    _run(root, "mv", "hosts/demoHost", "hosts/newHost")
    if (
        root / "packages/renamed/prm/nested/asset.txt"
    ).read_text() != "keep this resource":
        message = "rename lost package resources"
        raise AssertionError(message)
    for name in (
        "checks/renamed_coverage/default.nix",
        "checks/newHostVmWithDisko/default.nix",
    ):
        if not (root / name).is_file():
            raise AssertionError(name)
    _run(root, "rm", "packages/renamed", "--dry-run")
    if not (root / "packages/renamed/main.py").is_file():
        message = "dry-run removed the package"
        raise AssertionError(message)
    _run(root, "rm", "packages/renamed")
    _run(root, "rm", "hosts/newHost")
    if any(
        (root / name).exists()
        for name in (
            "packages/report",
            "packages/renamed",
            "hosts/demoHost",
            "hosts/newHost",
            "checks/report_coverage",
            "checks/renamed_coverage",
            "checks/demoHostVmWithDisko",
            "checks/newHostVmWithDisko",
        )
    ):
        message = "removed resources left package or check directories behind"
        raise AssertionError(message)
    _run(root, "canonicalize")
    _run(root, "canonicalize", "--dry-run")


def test_convergence_preserves_source_and_scratch_and_is_idempotent(
    repository: Path,
) -> None:
    """Convergence stages source, preserves scratch, and reaches a stable checkout."""
    root = repository
    _run(root, "add", "packages/example", "python")
    for name in (
        "tmp/root-state",
        "packages/example/tmp/package-state",
        "packages/example/prm/ms.tex",
    ):
        path = root / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(name, encoding="utf-8")
    (root / "discarded").write_text("generated artifact", encoding="utf-8")
    _run(root, "canonicalize")
    if (root / "discarded").exists():
        message = "convergence retained an unsupported artifact"
        raise AssertionError(message)
    tracked = _git(root, "ls-files").splitlines()
    if "packages/example/prm/ms.tex" not in tracked or any(
        "tmp/" in p for p in tracked
    ):
        raise AssertionError(tracked)
    before = {name: (root / name).read_bytes() for name in tracked}
    index = _git(root, "ls-files", "--stage")
    _run(root, "canonicalize")
    if before != {
        name: (root / name).read_bytes() for name in tracked
    } or index != _git(root, "ls-files", "--stage"):
        message = "a second convergence changed the checkout"
        raise AssertionError(message)
    for name in ("tmp/root-state", "packages/example/tmp/package-state"):
        if (root / name).read_text() != name:
            raise AssertionError(name)


def test_invalid_source_is_rejected_before_cleanup(repository: Path) -> None:
    """A failed convergence preserves both source and unrelated work."""
    _run(repository, "add", "packages/example", "python")
    source = repository / "packages/example/main.py"
    source.write_text("def test_misplaced(): pass\n", encoding="utf-8")
    artifact = repository / "work-in-progress"
    artifact.write_text("keep", encoding="utf-8")
    result = _run(repository, "canonicalize", code=1)
    if "test_main.py" not in result.stderr or artifact.read_text() != "keep":
        raise AssertionError(result.stderr)
    if source.read_text() != "def test_misplaced(): pass\n":
        message = "failed convergence rewrote invalid source"
        raise AssertionError(message)


@pytest.mark.parametrize(
    "arguments",
    [
        ("mv", "packages/example", "hosts/example"),
        ("add", "packages/bad-name", "python"),
        ("add", "hosts/bad-name"),
        ("rm", "../outside"),
    ],
)
def test_invalid_resource_requests_do_not_change_the_checkout(
    repository: Path,
    arguments: tuple[str, ...],
) -> None:
    """Reject malformed resource operations without staging changes."""
    before = _git(repository, "status", "--porcelain")
    _run(repository, *arguments, code=1)
    if _git(repository, "status", "--porcelain") != before:
        message = "invalid operation changed the checkout"
        raise AssertionError(message)


def test_cli_help_and_retired_commands(tmp_path: Path) -> None:
    """Help works outside a repository and retired commands fail clearly."""
    for command in ((), ("add",), ("mv",), ("rm",), ("init",), ("canonicalize",)):
        option = _run(tmp_path, *command, "--help").stdout
        alias = _run(tmp_path, "help", *command).stdout
        if option != alias or "usage:" not in option:
            raise AssertionError(option)
    _run(tmp_path, "status", code=2)
    _run(tmp_path, "check", code=2)


def test_home_convergence_records_only_clean_published_submodules(
    tmp_path: Path,
) -> None:
    """Advance a gitlink only after its checkout is clean and known to origin."""
    root = tmp_path
    relative = "forge.example/owner/demo"
    checkout = root / relative
    checkout.mkdir(parents=True)
    _git(root, "init", "--quiet")
    _git(checkout, "init", "--quiet")
    _git(checkout, "config", "user.name", "Test")
    _git(checkout, "config", "user.email", "test@example.org")
    _git(checkout, "config", "commit.gpgSign", "false")
    _git(checkout, "remote", "add", "origin", "git@forge.example:owner/demo")
    source = checkout / "README"
    source.write_text("first", encoding="utf-8")
    _git(checkout, "add", "README")
    _git(checkout, "commit", "--quiet", "-m", "first")
    first = _git(checkout, "rev-parse", "HEAD").strip()
    _git(checkout, "update-ref", "refs/remotes/origin/main", first)
    (root / ".gitmodules").write_text(
        f'[submodule "{relative}"]\npath = {relative}\n'
        "url = git@forge.example:owner/demo\n",
        encoding="utf-8",
    )
    (root / ".gitignore").write_text(
        "*\n!/.gitignore\n!/.gitmodules\n"
        "!/forge.example/\n!/forge.example/owner/\n!/forge.example/owner/demo\n",
        encoding="utf-8",
    )
    _git(root, "add", "--force", ".gitignore", ".gitmodules", relative)
    source.write_text("second", encoding="utf-8")
    rejected = _run(root, "canonicalize", code=1)
    if "dirty" not in rejected.stderr or first not in _git(
        root,
        "ls-files",
        "--stage",
        relative,
    ):
        raise AssertionError(rejected.stderr)
    _git(checkout, "add", "README")
    _git(checkout, "commit", "--quiet", "-m", "second")
    rejected = _run(root, "canonicalize", code=1)
    if "remote-tracking" not in rejected.stderr or first not in _git(
        root,
        "ls-files",
        "--stage",
        relative,
    ):
        raise AssertionError(rejected.stderr)
    second = _git(checkout, "rev-parse", "HEAD").strip()
    _git(checkout, "update-ref", "refs/remotes/origin/main", second)
    _run(root, "canonicalize")
    if (
        second not in _git(root, "ls-files", "--stage", relative)
        or source.read_text() != "second"
    ):
        message = "home convergence lost the published submodule state"
        raise AssertionError(message)
