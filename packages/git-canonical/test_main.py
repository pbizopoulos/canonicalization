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
    for command in (
        (),
        ("add",),
        ("mv",),
        ("rm",),
        ("init",),
        ("converge",),
        ("test-names",),
    ):
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


def _make_test_names_package(root: Path, name: str, source: str) -> Path:
    """Create a canonical package fixture without executing its source."""
    package = root / "packages" / name
    package.mkdir(parents=True)
    (root / "flake.nix").touch()
    (package / "default.nix").touch()
    (package / "main.py").touch()
    (package / "test_main.py").write_text(source)
    return package


def _run_test_names(cwd: Path, *arguments: str) -> subprocess.CompletedProcess[str]:
    """Invoke the installed command from a chosen working directory."""
    return subprocess.run(  # noqa: S603
        [os.environ["PACKAGE_E2E_EXECUTABLE"], "test-names", *arguments],
        cwd=cwd,
        capture_output=True,
        text=True,
        check=False,
        timeout=10,
    )


@pytest.mark.parametrize("explicit", [False, True])
@pytest.mark.parametrize("package_name", ["example", "my-package"])
def test_names_package_names_become_sentences_without_executing_source(
    tmp_path: Path,
    package_name: str,
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
    package = _make_test_names_package(tmp_path, package_name, source)
    result = _run_test_names(
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


def test_names_unittest_aliases_and_local_subclasses_are_recognized(
    tmp_path: Path,
) -> None:
    """List methods of statically recognized unittest classes."""
    package = _make_test_names_package(
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
    result = _run_test_names(package)
    if not (result.returncode == 0):
        raise AssertionError(result.stderr)
    if result.stdout != "test async\ntest derived\ntest report\n":
        msg = f"Unexpected CLI result: {result}"
        raise AssertionError(msg)


@pytest.mark.parametrize("explicit", [False, True])
def names_repository_groups_sorted_packages_and_skips_untested_packages(
    tmp_path: Path,
    *,
    explicit: bool,
) -> None:
    """Match runner target discovery while reporting sentences by package."""
    _make_test_names_package(tmp_path, "zebra", "def test_last(): pass\n")
    _make_test_names_package(tmp_path, "alpha", "def test_first(): pass\n")
    missing = _make_test_names_package(tmp_path, "untested", "")
    (missing / "test_main.py").unlink()
    (tmp_path / "packages/linked").symlink_to(missing, target_is_directory=True)
    result = _run_test_names(tmp_path, *([str(tmp_path)] if explicit else []))
    if not (result.returncode == 0):
        raise AssertionError(result.stderr)
    if result.stdout != "packages/alpha:\ntest first\npackages/zebra:\ntest last\n":
        msg = f"Unexpected CLI result: {result}"
        raise AssertionError(msg)
    if result.stderr != "Skipping untested: no test_main.py\n":
        msg = f"Unexpected CLI result: {result}"
        raise AssertionError(msg)


def names_repository_continues_after_a_malformed_test_file(tmp_path: Path) -> None:
    """Report a parse error while still printing later packages."""
    _make_test_names_package(tmp_path, "broken", "def invalid(")
    _make_test_names_package(tmp_path, "valid", "def test_still_listed(): pass\n")
    result = _run_test_names(tmp_path)
    if result.returncode != 1:
        msg = "Expectation failed: result.returncode == 1"
        raise AssertionError(msg)
    if "test still listed\n" not in result.stdout:
        msg = "Expectation failed: 'test still listed\\n' in result.stdout"
        raise AssertionError(msg)
    if "git canonical test-names: broken:" not in result.stderr:
        msg = "Expectation failed: 'git canonical test-names: broken:' in result.stderr"
        raise AssertionError(msg)


@pytest.mark.parametrize("layout", ["missing", "syntax", "encoding", "linked"])
def test_names_invalid_test_files_return_failure(tmp_path: Path, layout: str) -> None:
    """Reject missing, malformed, undecodable and linked source files."""
    package = _make_test_names_package(tmp_path, "example", "")
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
    result = _run_test_names(package)
    if result.returncode != 1:
        msg = "Expectation failed: result.returncode == 1"
        raise AssertionError(msg)
    if "git canonical test-names:" not in result.stderr:
        msg = "Expectation failed: 'git canonical test-names:' in result.stderr"
        raise AssertionError(msg)
    if result.stdout != "":
        msg = "Expectation failed: result.stdout == ''"
        raise AssertionError(msg)


def test_names_empty_test_file_succeeds_without_sentences(tmp_path: Path) -> None:
    """An empty test file has no specifications to print."""
    package = _make_test_names_package(tmp_path, "example", "")
    result = _run_test_names(package)
    if result.returncode != 0:
        msg = "Expectation failed: result.returncode == 0"
        raise AssertionError(msg)
    if not (result.stdout == result.stderr == ""):
        msg = "Expectation failed: result.stdout == result.stderr == ''"
        raise AssertionError(msg)


def test_names_cli_help_invalid_targets_and_unsupported_options(tmp_path: Path) -> None:
    """Share target and help conventions without accepting execution budgets."""
    usage_error = 2
    result = _run_test_names(tmp_path, "--help")
    if result.returncode != 0:
        msg = "Expectation failed: result.returncode == 0"
        raise AssertionError(msg)
    if "[target]" not in result.stdout:
        msg = "Expectation failed: '[target]' in result.stdout"
        raise AssertionError(msg)
    if "current directory" not in result.stdout:
        msg = "Expectation failed: 'current directory' in result.stdout"
        raise AssertionError(msg)
    if _run_test_names(tmp_path).returncode != 1:
        msg = "Expectation failed: run_cli(tmp_path).returncode == 1"
        raise AssertionError(msg)
    if _run_test_names(tmp_path, "--timeout", "60").returncode != usage_error:
        msg = f"Unexpected CLI result: {result}"
        raise AssertionError(msg)
    if _run_test_names(tmp_path, "--max-examples", "100").returncode != usage_error:
        msg = f"Unexpected CLI result: {result}"
        raise AssertionError(msg)
    (tmp_path / "flake.nix").touch()
    result = _run_test_names(tmp_path)
    if result.returncode != 1:
        msg = "Expectation failed: result.returncode == 1"
        raise AssertionError(msg)
    if "no Python packages found" not in result.stderr:
        msg = "Expectation failed: 'no Python packages found' in result.stderr"
        raise AssertionError(msg)


def _test_names_git(root: Path, *arguments: str) -> str:
    """Run fixture Git commands with a local identity and no signing."""
    return subprocess.run(  # noqa: S603
        [  # noqa: S607
            "git",
            "-c",
            "user.name=Test",
            "-c",
            "user.email=test@example.invalid",
            "-c",
            "commit.gpgsign=false",
            *arguments,
        ],
        cwd=root,
        capture_output=True,
        text=True,
        check=True,
        timeout=10,
    ).stdout


@pytest.fixture
def names_repository(tmp_path: Path) -> Path:
    """Create distinct previous, HEAD, staged and working-tree sentences."""
    _test_names_git(tmp_path, "init", "-q")
    package = _make_test_names_package(
        tmp_path,
        "example",
        "def test_previous(): pass\n",
    )
    _test_names_git(tmp_path, "add", ".")
    _test_names_git(tmp_path, "commit", "-qm", "Initial tests")
    source = package / "test_main.py"
    source.write_text("def test_committed(): pass\n")
    _test_names_git(tmp_path, "add", ".")
    _test_names_git(tmp_path, "commit", "-qm", "Rename the test")
    source.write_text("def test_staged(): pass\n")
    _test_names_git(tmp_path, "add", ".")
    source.write_text("def test_working(): pass\n")
    (package / "main.py").write_text("PRIVATE_IMPLEMENTATION = 1\n")
    return tmp_path


@pytest.mark.parametrize(
    ("arguments", "removed", "added"),
    [
        (("diff",), "staged", "working"),
        (("diff", "--staged"), "committed", "staged"),
        (("diff", "--cached"), "committed", "staged"),
        (("diff", "HEAD"), "committed", "working"),
        (("diff", "HEAD~1", "HEAD"), "previous", "committed"),
        (("show",), "previous", "committed"),
        (("show", "HEAD"), "previous", "committed"),
        (("diff", "-R"), "working", "staged"),
    ],
)
def test_names_git_compares_sentences_using_native_revision_and_index_semantics(
    names_repository: Path,
    arguments: tuple[str, ...],
    removed: str,
    added: str,
) -> None:
    """Let Git select versions while exposing only test-name sentences."""
    result = _run_test_names(names_repository, *arguments)
    if result.returncode != 0 or result.stderr:
        raise AssertionError(result)
    if (
        f"-test {removed}\n" not in result.stdout
        or f"+test {added}\n" not in result.stdout
    ):
        raise AssertionError(result.stdout)
    if "def test_" in result.stdout or "PRIVATE_IMPLEMENTATION" in result.stdout:
        raise AssertionError(result.stdout)
    if arguments[0] == "show" and "Rename the test" not in result.stdout:
        raise AssertionError(result.stdout)


def test_names_git_path_filters_intersect_test_scope_and_work_from_subdirectories(
    names_repository: Path,
) -> None:
    """Explicit source paths never widen the test-only view."""
    _make_test_names_package(names_repository, "other", "def test_other(): pass\n")
    _test_names_git(names_repository, "add", "packages/other")
    result = _run_test_names(
        names_repository,
        "diff",
        "HEAD",
        "--",
        "packages/example",
    )
    if "+test working" not in result.stdout or "test other" in result.stdout:
        raise AssertionError(result)
    excluded = _run_test_names(
        names_repository,
        "diff",
        "HEAD",
        "--",
        "packages/example/main.py",
    )
    if excluded.returncode != 0 or excluded.stdout:
        raise AssertionError(excluded)
    local = _run_test_names(
        names_repository / "packages/example",
        "diff",
        "HEAD",
        "--",
        ".",
    )
    if local.returncode != 0 or local.stdout != result.stdout:
        raise AssertionError(local)
    all_packages = _run_test_names(
        names_repository / "packages/example",
        "diff",
        "HEAD",
    )
    if all_packages.returncode != 0 or "+test other" not in all_packages.stdout:
        raise AssertionError(all_packages)


def test_names_body_edits_and_untracked_files_have_no_sentence_patch(
    names_repository: Path,
) -> None:
    """Keep Git tracking rules and hide edits outside extracted names."""
    (names_repository / "packages/example/test_main.py").write_text(
        "raise RuntimeError('never execute')\ndef test_staged(): assert False\n",
    )
    _make_test_names_package(
        names_repository,
        "untracked",
        "def test_untracked(): pass\n",
    )
    result = _run_test_names(names_repository, "diff")
    if result.returncode != 0 or result.stdout or result.stderr:
        raise AssertionError(result)


def test_names_deleted_packages_and_initial_commits_use_historical_sources(
    names_repository: Path,
) -> None:
    """Do not require historical packages to exist in the working tree."""
    _test_names_git(names_repository, "rm", "-rf", "packages/example")
    _test_names_git(names_repository, "commit", "-qm", "Delete package")
    deleted = _run_test_names(names_repository, "show")
    initial = _run_test_names(names_repository, "show", "HEAD~2")
    historical = _run_test_names(names_repository, "diff", "HEAD~2", "HEAD~1")
    for result, sentence in (
        (deleted, "-test committed"),
        (initial, "+test previous"),
        (historical, "+test committed"),
    ):
        if result.returncode != 0 or sentence not in result.stdout:
            raise AssertionError(result)


def test_names_git_preserves_exit_codes_and_reports_parse_and_revision_errors(
    names_repository: Path,
) -> None:
    """Surface Git failures and converter failures without raw source output."""
    changed = _run_test_names(names_repository, "diff", "--exit-code")
    if changed.returncode != 1 or "+test working" not in changed.stdout:
        raise AssertionError(changed)
    missing = _run_test_names(
        names_repository,
        "diff",
        "nonexistent-revision",
        "--",
    )
    if missing.returncode == 0 or not missing.stderr:
        raise AssertionError(missing)
    (names_repository / "packages/example/test_main.py").write_text("def invalid(")
    malformed = _run_test_names(names_repository, "diff")
    if malformed.returncode == 0 or "git canonical test-names:" not in malformed.stderr:
        raise AssertionError(malformed)


def test_names_git_rejects_conflicting_attributes_without_changing_repository(
    names_repository: Path,
) -> None:
    """Temporary Git configuration cannot silently lose to repository attributes."""
    attributes = names_repository / ".gitattributes"
    attributes.write_text("packages/*/test_main.py diff=custom\n")
    preserved = [
        names_repository / ".git/config",
        names_repository / ".git/index",
        attributes,
        names_repository / "packages/example/test_main.py",
    ]
    before = [path.read_bytes() for path in preserved]
    result = _run_test_names(names_repository, "diff")
    if result.returncode == 0 or "conflicting diff attribute" not in result.stderr:
        raise AssertionError(result)
    if result.stdout or before != [path.read_bytes() for path in preserved]:
        raise AssertionError(result)


def test_names_git_views_leave_configuration_index_sources_and_refs_unchanged(
    names_repository: Path,
) -> None:
    """Do not install attributes, stage files, or cache converted blobs."""
    paths = [
        names_repository / ".git/config",
        names_repository / ".git/index",
        names_repository / "packages/example/test_main.py",
    ]
    before = [path.read_bytes() for path in paths]
    refs = _test_names_git(names_repository, "show-ref")
    for arguments in (("diff",), ("diff", "--staged"), ("show",)):
        result = _run_test_names(names_repository, *arguments)
        if result.returncode != 0:
            raise AssertionError(result)
    if before != [path.read_bytes() for path in paths] or refs != _test_names_git(
        names_repository,
        "show-ref",
    ):
        message = "Git view changed repository state"
        raise AssertionError(message)
    if (names_repository / ".gitattributes").exists() or (
        names_repository / ".git/info/attributes"
    ).exists():
        message = "Git view installed repository attributes"
        raise AssertionError(message)


@pytest.mark.parametrize(
    "arguments",
    [
        ("diff", "--no-index"),
        ("diff", "--no-textconv"),
        ("diff", "--check"),
        ("show", "HEAD:packages/example/test_main.py"),
        ("show", "HEAD^{tree}"),
    ],
)
def test_names_git_rejects_views_that_cannot_produce_test_name_diffs(
    names_repository: Path,
    arguments: tuple[str, ...],
) -> None:
    """Blob/tree display and raw-source modes are not sentence diffs."""
    result = _run_test_names(names_repository, *arguments)
    if result.returncode == 0 or result.stdout or not result.stderr:
        raise AssertionError(result)


@pytest.mark.parametrize("committed", [False, True])
def test_names_git_rejects_symlink_diffs_in_working_and_historical_files(
    names_repository: Path,
    *,
    committed: bool,
) -> None:
    """Git must not display symlink targets as test sentences."""
    source = names_repository / "packages/example/test_main.py"
    source.unlink()
    source.symlink_to("PRIVATE_TARGET")
    if committed:
        _test_names_git(names_repository, "add", "packages/example/test_main.py")
        _test_names_git(names_repository, "commit", "-qm", "Link test file")
        source.unlink()
        source.write_text("def test_working(): pass\n")
    result = _run_test_names(names_repository, "show" if committed else "diff")
    if result.returncode == 0 or result.stdout or "regular files" not in result.stderr:
        raise AssertionError(result)


def test_names_git_rename_detection_and_formatting_remain_native(
    names_repository: Path,
) -> None:
    """Git handles file renames and user-selected patch presentation."""
    _test_names_git(names_repository, "reset", "--hard", "HEAD")
    _test_names_git(names_repository, "mv", "packages/example", "packages/renamed")
    renamed = _run_test_names(names_repository, "diff", "--staged", "-M")
    if (
        renamed.returncode != 0
        or "rename to packages/renamed/test_main.py" not in renamed.stdout
    ):
        raise AssertionError(renamed)
    if "main.py" in renamed.stdout.replace("test_main.py", ""):
        raise AssertionError(renamed.stdout)
    formatted = _run_test_names(
        names_repository,
        "show",
        "--format=%s",
        "--color=never",
        "-U0",
    )
    if formatted.returncode != 0 or not formatted.stdout.startswith(
        "Rename the test\n",
    ):
        raise AssertionError(formatted)


def test_names_merge_show_uses_gits_combined_sentence_diff(
    names_repository: Path,
) -> None:
    """Preserve combined merge presentation instead of inventing a first parent."""
    _test_names_git(names_repository, "reset", "--hard", "HEAD")
    _test_names_git(names_repository, "branch", "side", "HEAD~1")
    _test_names_git(names_repository, "checkout", "-q", "side")
    (names_repository / "packages/example/test_main.py").write_text(
        "def test_side(): pass\n",
    )
    _test_names_git(names_repository, "commit", "-qam", "Side change")
    _test_names_git(names_repository, "checkout", "-q", "-")
    subprocess.run(
        [  # noqa: S607
            "git",
            "-c",
            "user.name=Test",
            "-c",
            "user.email=test@example.invalid",
            "merge",
            "--no-commit",
            "side",
        ],
        cwd=names_repository,
        capture_output=True,
        check=False,
        timeout=10,
    )
    (names_repository / "packages/example/test_main.py").write_text(
        "def test_merged(): pass\n",
    )
    _test_names_git(names_repository, "add", "packages/example/test_main.py")
    _test_names_git(names_repository, "commit", "-qm", "Resolve merge")
    result = _run_test_names(names_repository, "show", "--color=never")
    if (
        result.returncode != 0
        or "diff --cc" not in result.stdout
        or "++test merged" not in result.stdout
    ):
        raise AssertionError(result)


def test_names_staged_diff_works_before_the_first_commit(tmp_path: Path) -> None:
    """Let Git compare an unborn HEAD to newly staged tests."""
    _test_names_git(tmp_path, "init", "-q")
    _make_test_names_package(tmp_path, "example", "def test_first(): pass\n")
    _test_names_git(tmp_path, "add", ".")
    result = _run_test_names(tmp_path, "diff", "--staged")
    if result.returncode != 0 or "+test first" not in result.stdout:
        raise AssertionError(result)


def test_names_git_views_preserve_definition_order_while_listings_remain_sorted(
    tmp_path: Path,
) -> None:
    """Appended tests remain appended in patches, including methods and async tests."""
    _test_names_git(tmp_path, "init", "-q")
    package = _make_test_names_package(tmp_path, "example", "def test_zebra(): pass\n")
    _test_names_git(tmp_path, "add", ".")
    _test_names_git(tmp_path, "commit", "-qm", "Initial test")
    source = package / "test_main.py"
    source.write_text(
        "def test_zebra(): pass\n"
        "class TestBehavior:\n    def test_middle(self): pass\n"
        "async def test_alpha(): pass\n",
    )
    expected = " test zebra\n+test middle\n+test alpha\n"
    working = _run_test_names(tmp_path, "diff", "--color=never")
    _test_names_git(tmp_path, "add", ".")
    staged = _run_test_names(tmp_path, "diff", "--staged", "--color=never")
    _test_names_git(tmp_path, "commit", "-qm", "Append tests")
    committed = _run_test_names(tmp_path, "show", "--color=never")
    for result in (working, staged, committed):
        if result.returncode != 0 or expected not in result.stdout:
            raise AssertionError(result)
    listing = _run_test_names(package)
    if (
        listing.returncode != 0
        or listing.stdout != "test alpha\ntest middle\ntest zebra\n"
    ):
        raise AssertionError(listing)


@pytest.mark.parametrize("arguments", [(), ("diff",), ("diff", "--staged"), ("show",)])
def test_names_are_available_through_gits_canonical_subcommand(
    names_repository: Path,
    arguments: tuple[str, ...],
) -> None:
    """The public Git command preserves listing, patch output and exit status."""
    environment = dict(os.environ)
    executable_directory = os.path.dirname(environment["PACKAGE_E2E_EXECUTABLE"])  # noqa: PTH120
    environment["PATH"] = executable_directory + os.pathsep + environment["PATH"]
    expected = _run_test_names(names_repository, *arguments)
    result = subprocess.run(  # noqa: S603
        ["git", "canonical", "test-names", *arguments],  # noqa: S607
        cwd=names_repository,
        env=environment,
        capture_output=True,
        text=True,
        check=False,
        timeout=30,
    )
    if (result.returncode, result.stdout, result.stderr) != (
        expected.returncode,
        expected.stdout,
        expected.stderr,
    ):
        raise AssertionError(result)
