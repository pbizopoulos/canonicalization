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
    _run(root, "converge")
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
    _run(root, "converge")
    _run(root, "converge", "--dry-run")


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
    _run(root, "converge")
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
    _run(root, "converge")
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
    result = _run(repository, "converge", code=1)
    if "test_main.py" not in result.stderr or artifact.read_text() != "keep":
        raise AssertionError(result.stderr)
    if source.read_text() != "def test_misplaced(): pass\n":
        message = "failed convergence rewrote invalid source"
        raise AssertionError(message)


@pytest.mark.parametrize(
    "arguments",
    [
        ("mv", "packages/example", "hosts/example"),
        ("add", "packages/bad--name", "python"),
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
    for command in ((), ("add",), ("mv",), ("rm",), ("init",), ("converge",)):
        option = _run(tmp_path, *command, "--help").stdout
        alias = _run(tmp_path, "help", *command).stdout
        if option != alias or "usage:" not in option:
            raise AssertionError(option)
    _run(tmp_path, "status", code=2)
    _run(tmp_path, "check", code=2)


@pytest.fixture
def home_repository(tmp_path: Path) -> Path:
    """Create a home repository with a locally initialized submodule."""
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
    return root


def test_home_convergence_preserves_dirty_and_unpublished_submodule_state(
    home_repository: Path,
) -> None:
    """Leave dirty files and gitlink advancement to native Git."""
    root = home_repository
    relative = "forge.example/owner/demo"
    checkout = root / relative
    source = checkout / "README"
    first = _git(checkout, "rev-parse", "HEAD").strip()
    source.write_text("second", encoding="utf-8")
    _run(root, "converge")
    if source.read_text() != "second":
        message = "convergence changed dirty submodule content"
        raise AssertionError(message)
    _git(checkout, "add", "README")
    _git(checkout, "commit", "--quiet", "-m", "second")
    second = _git(checkout, "rev-parse", "HEAD").strip()
    for published in (False, True):
        if published:
            _git(checkout, "update-ref", "refs/remotes/origin/main", second)
        _run(root, "converge")
        if first not in _git(root, "ls-files", "--stage", relative):
            message = "convergence advanced the recorded submodule commit"
            raise AssertionError(message)
    _git(root, "add", relative)
    if second not in _git(root, "ls-files", "--stage", relative):
        message = "native Git could not advance the submodule commit"
        raise AssertionError(message)


def test_home_rename_preserves_dirty_checkout_and_recorded_commit(
    home_repository: Path,
) -> None:
    """Move dirty submodules safely and reject collisions before mutation."""
    root = home_repository
    relative = "forge.example/owner/demo"
    checkout = root / relative
    source = checkout / "README"
    first = _git(checkout, "rev-parse", "HEAD").strip()
    source.write_text("second", encoding="utf-8")
    _git(checkout, "add", "README")
    _git(checkout, "commit", "--quiet", "-m", "second")
    _git(root, "submodule", "absorbgitdirs", relative)
    _git(root, "config", f"submodule.{relative}.url", "git@forge.example:owner/demo")
    destination = "forge.example/owner/renamed"
    _git(
        root,
        "config",
        "--file",
        ".gitmodules",
        f"submodule.{relative}.url",
        "git@forge.example:owner/renamed",
    )
    source.write_text("staged", encoding="utf-8")
    _git(checkout, "add", "README")
    source.write_text("unstaged", encoding="utf-8")
    (checkout / "untracked").write_text("keep", encoding="utf-8")
    (root / "unrelated").write_text("keep home file", encoding="utf-8")
    before = _git(checkout, "status", "--porcelain")
    indexed = _git(checkout, "show", ":README")
    target = root / destination
    target.mkdir()
    rejected = _run(root, "converge", code=1)
    if "target already exists" not in rejected.stderr or not checkout.exists():
        raise AssertionError(rejected.stderr)
    if destination in _git(
        root,
        "config",
        "--file",
        ".gitmodules",
        f"submodule.{relative}.path",
    ):
        message = "collision changed the submodule path"
        raise AssertionError(message)
    target.rmdir()
    rejected = _run(root, "converge", code=1)
    if "stage .gitmodules" not in rejected.stderr or not checkout.exists():
        raise AssertionError(rejected.stderr)
    _git(root, "add", ".gitmodules")
    _run(root, "converge", "--dry-run", code=1)
    if not checkout.exists() or target.exists():
        message = "dry-run moved the checkout"
        raise AssertionError(message)
    _run(root, "converge")
    if (
        checkout.exists()
        or _git(target, "status", "--porcelain") != before
        or _git(target, "show", ":README") != indexed
        or (target / "untracked").read_text() != "keep"
        or (root / "unrelated").read_text() != "keep home file"
        or first not in _git(root, "ls-files", "--stage", destination)
        or _git(target, "remote", "get-url", "origin").strip()
        != "git@forge.example:owner/renamed"
    ):
        message = "home rename failed to preserve Git state or synchronize the URL"
        raise AssertionError(message)
    _run(root, "converge", "--dry-run")


@pytest.mark.parametrize("kind", ["python", "html", "latex", "nix"])
def test_dash_case_packages_normalize_nix_names(repository: Path, kind: str) -> None:
    """Keep dashed resource paths and normalize generated package names."""
    _run(repository, "add", "packages/dash-case", kind)
    package = repository / "packages/dash-case"
    expected = 'builtins.replaceStrings [ "-" ] [ "_" ] (baseNameOf ./.)'
    if expected not in (package / "default.nix").read_text():
        msg = "generated Nix package name was not normalized"
        raise AssertionError(msg)
    _run(repository, "converge")
    _run(repository, "converge", "--dry-run")
    _run(repository, "mv", "packages/dash-case", "packages/another-name")
    _run(repository, "converge")
    _run(repository, "rm", "packages/another-name")


def test_git_discovers_canonical_subcommand(repository: Path) -> None:
    """Expose the installed CLI through Git's external command lookup."""
    environment = dict(os.environ)
    executable_directory = os.path.dirname(environment["PACKAGE_E2E_EXECUTABLE"])  # noqa: PTH120
    environment["PATH"] = executable_directory + os.pathsep + environment["PATH"]
    result = subprocess.run(
        ["git", "canonical", "help"],  # noqa: S607
        cwd=repository,
        env=environment,
        capture_output=True,
        text=True,
        check=True,
        timeout=30,
    )
    if "usage: git canonical" not in result.stdout:
        msg = "Git did not discover the canonical CLI"
        raise AssertionError(msg)
