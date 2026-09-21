# Copyright (c) 2026- Paschalis Bizopoulos
"""CLI requirements for reading test names as sentences."""

from __future__ import annotations

import os
import subprocess
from typing import TYPE_CHECKING

import pytest

if TYPE_CHECKING:
    from pathlib import Path


def make_package(root: Path, name: str, source: str) -> Path:
    """Create a canonical package fixture without executing its source."""
    package = root / "packages" / name
    package.mkdir(parents=True)
    (root / "flake.nix").touch()
    (package / "default.nix").touch()
    (package / "main.py").touch()
    (package / "test_main.py").write_text(source)
    return package


def run_cli(cwd: Path, *arguments: str) -> subprocess.CompletedProcess[str]:
    """Invoke the installed command from a chosen working directory."""
    return subprocess.run(  # noqa: S603
        [os.environ["PACKAGE_E2E_EXECUTABLE"], *arguments],
        cwd=cwd,
        capture_output=True,
        text=True,
        check=False,
        timeout=10,
    )


@pytest.mark.parametrize("explicit", [False, True])
def test_package_names_become_sentences_without_executing_source(
    tmp_path: Path,
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
    package = make_package(tmp_path, "example", source)
    result = run_cli(
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


def test_unittest_aliases_and_local_subclasses_are_recognized(tmp_path: Path) -> None:
    """List methods of statically recognized unittest classes."""
    package = make_package(
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
    result = run_cli(package)
    if not (result.returncode == 0):
        raise AssertionError(result.stderr)
    if result.stdout != "test async\ntest derived\ntest report\n":
        msg = f"Unexpected CLI result: {result}"
        raise AssertionError(msg)


@pytest.mark.parametrize("explicit", [False, True])
def test_repository_groups_sorted_packages_and_skips_untested_packages(
    tmp_path: Path,
    *,
    explicit: bool,
) -> None:
    """Match runner target discovery while reporting sentences by package."""
    make_package(tmp_path, "zebra", "def test_last(): pass\n")
    make_package(tmp_path, "alpha", "def test_first(): pass\n")
    missing = make_package(tmp_path, "untested", "")
    (missing / "test_main.py").unlink()
    (tmp_path / "packages/linked").symlink_to(missing, target_is_directory=True)
    result = run_cli(tmp_path, *([str(tmp_path)] if explicit else []))
    if not (result.returncode == 0):
        raise AssertionError(result.stderr)
    if result.stdout != "packages/alpha:\ntest first\npackages/zebra:\ntest last\n":
        msg = f"Unexpected CLI result: {result}"
        raise AssertionError(msg)
    if result.stderr != "Skipping untested: no test_main.py\n":
        msg = f"Unexpected CLI result: {result}"
        raise AssertionError(msg)


def test_repository_continues_after_a_malformed_test_file(tmp_path: Path) -> None:
    """Report a parse error while still printing later packages."""
    make_package(tmp_path, "broken", "def invalid(")
    make_package(tmp_path, "valid", "def test_still_listed(): pass\n")
    result = run_cli(tmp_path)
    if result.returncode != 1:
        msg = "Expectation failed: result.returncode == 1"
        raise AssertionError(msg)
    if "test still listed\n" not in result.stdout:
        msg = "Expectation failed: 'test still listed\\n' in result.stdout"
        raise AssertionError(msg)
    if "python_test_names: broken:" not in result.stderr:
        msg = "Expectation failed: 'python_test_names: broken:' in result.stderr"
        raise AssertionError(msg)


@pytest.mark.parametrize("layout", ["missing", "syntax", "encoding", "linked"])
def test_invalid_test_files_return_failure(tmp_path: Path, layout: str) -> None:
    """Reject missing, malformed, undecodable and linked source files."""
    package = make_package(tmp_path, "example", "")
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
    result = run_cli(package)
    if result.returncode != 1:
        msg = "Expectation failed: result.returncode == 1"
        raise AssertionError(msg)
    if "python_test_names:" not in result.stderr:
        msg = "Expectation failed: 'python_test_names:' in result.stderr"
        raise AssertionError(msg)
    if result.stdout != "":
        msg = "Expectation failed: result.stdout == ''"
        raise AssertionError(msg)


def test_empty_test_file_succeeds_without_sentences(tmp_path: Path) -> None:
    """An empty test file has no specifications to print."""
    package = make_package(tmp_path, "example", "")
    result = run_cli(package)
    if result.returncode != 0:
        msg = "Expectation failed: result.returncode == 0"
        raise AssertionError(msg)
    if not (result.stdout == result.stderr == ""):
        msg = "Expectation failed: result.stdout == result.stderr == ''"
        raise AssertionError(msg)


def test_cli_help_invalid_targets_and_unsupported_options(tmp_path: Path) -> None:
    """Share target and help conventions without accepting execution budgets."""
    usage_error = 2
    result = run_cli(tmp_path, "--help")
    if result.returncode != 0:
        msg = "Expectation failed: result.returncode == 0"
        raise AssertionError(msg)
    if "[target]" not in result.stdout:
        msg = "Expectation failed: '[target]' in result.stdout"
        raise AssertionError(msg)
    if "current directory" not in result.stdout:
        msg = "Expectation failed: 'current directory' in result.stdout"
        raise AssertionError(msg)
    if run_cli(tmp_path).returncode != 1:
        msg = "Expectation failed: run_cli(tmp_path).returncode == 1"
        raise AssertionError(msg)
    if run_cli(tmp_path, "--timeout", "60").returncode != usage_error:
        msg = f"Unexpected CLI result: {result}"
        raise AssertionError(msg)
    if run_cli(tmp_path, "--max-examples", "100").returncode != usage_error:
        msg = f"Unexpected CLI result: {result}"
        raise AssertionError(msg)
    (tmp_path / "flake.nix").touch()
    result = run_cli(tmp_path)
    if result.returncode != 1:
        msg = "Expectation failed: result.returncode == 1"
        raise AssertionError(msg)
    if "no Python packages found" not in result.stderr:
        msg = "Expectation failed: 'no Python packages found' in result.stderr"
        raise AssertionError(msg)


def git(root: Path, *arguments: str) -> str:
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
def repository(tmp_path: Path) -> Path:
    """Create distinct previous, HEAD, staged and working-tree sentences."""
    git(tmp_path, "init", "-q")
    package = make_package(tmp_path, "example", "def test_previous(): pass\n")
    git(tmp_path, "add", ".")
    git(tmp_path, "commit", "-qm", "Initial tests")
    source = package / "test_main.py"
    source.write_text("def test_committed(): pass\n")
    git(tmp_path, "add", ".")
    git(tmp_path, "commit", "-qm", "Rename the test")
    source.write_text("def test_staged(): pass\n")
    git(tmp_path, "add", ".")
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
def test_git_compares_sentences_using_native_revision_and_index_semantics(
    repository: Path,
    arguments: tuple[str, ...],
    removed: str,
    added: str,
) -> None:
    """Let Git select versions while exposing only test-name sentences."""
    result = run_cli(repository, *arguments)
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


def test_git_path_filters_intersect_test_scope_and_work_from_subdirectories(
    repository: Path,
) -> None:
    """Explicit source paths never widen the test-only view."""
    make_package(repository, "other", "def test_other(): pass\n")
    git(repository, "add", "packages/other")
    result = run_cli(repository, "diff", "HEAD", "--", "packages/example")
    if "+test working" not in result.stdout or "test other" in result.stdout:
        raise AssertionError(result)
    excluded = run_cli(repository, "diff", "HEAD", "--", "packages/example/main.py")
    if excluded.returncode != 0 or excluded.stdout:
        raise AssertionError(excluded)
    local = run_cli(repository / "packages/example", "diff", "HEAD", "--", ".")
    if local.returncode != 0 or local.stdout != result.stdout:
        raise AssertionError(local)
    all_packages = run_cli(repository / "packages/example", "diff", "HEAD")
    if all_packages.returncode != 0 or "+test other" not in all_packages.stdout:
        raise AssertionError(all_packages)


def test_body_edits_and_untracked_files_have_no_sentence_patch(
    repository: Path,
) -> None:
    """Keep Git tracking rules and hide edits outside extracted names."""
    (repository / "packages/example/test_main.py").write_text(
        "raise RuntimeError('never execute')\ndef test_staged(): assert False\n",
    )
    make_package(repository, "untracked", "def test_untracked(): pass\n")
    result = run_cli(repository, "diff")
    if result.returncode != 0 or result.stdout or result.stderr:
        raise AssertionError(result)


def test_deleted_packages_and_initial_commits_use_historical_sources(
    repository: Path,
) -> None:
    """Do not require historical packages to exist in the working tree."""
    git(repository, "rm", "-rf", "packages/example")
    git(repository, "commit", "-qm", "Delete package")
    deleted = run_cli(repository, "show")
    initial = run_cli(repository, "show", "HEAD~2")
    historical = run_cli(repository, "diff", "HEAD~2", "HEAD~1")
    for result, sentence in (
        (deleted, "-test committed"),
        (initial, "+test previous"),
        (historical, "+test committed"),
    ):
        if result.returncode != 0 or sentence not in result.stdout:
            raise AssertionError(result)


def test_git_preserves_exit_codes_and_reports_parse_and_revision_errors(
    repository: Path,
) -> None:
    """Surface Git failures and converter failures without raw source output."""
    changed = run_cli(repository, "diff", "--exit-code")
    if changed.returncode != 1 or "+test working" not in changed.stdout:
        raise AssertionError(changed)
    missing = run_cli(repository, "diff", "nonexistent-revision", "--")
    if missing.returncode == 0 or not missing.stderr:
        raise AssertionError(missing)
    (repository / "packages/example/test_main.py").write_text("def invalid(")
    malformed = run_cli(repository, "diff")
    if malformed.returncode == 0 or "python_test_names:" not in malformed.stderr:
        raise AssertionError(malformed)


def test_git_rejects_conflicting_attributes_without_changing_repository(
    repository: Path,
) -> None:
    """Temporary Git configuration cannot silently lose to repository attributes."""
    attributes = repository / ".gitattributes"
    attributes.write_text("packages/*/test_main.py diff=custom\n")
    preserved = [
        repository / ".git/config",
        repository / ".git/index",
        attributes,
        repository / "packages/example/test_main.py",
    ]
    before = [path.read_bytes() for path in preserved]
    result = run_cli(repository, "diff")
    if result.returncode == 0 or "conflicting diff attribute" not in result.stderr:
        raise AssertionError(result)
    if result.stdout or before != [path.read_bytes() for path in preserved]:
        raise AssertionError(result)


def test_git_views_leave_configuration_index_sources_and_refs_unchanged(
    repository: Path,
) -> None:
    """Do not install attributes, stage files, or cache converted blobs."""
    paths = [
        repository / ".git/config",
        repository / ".git/index",
        repository / "packages/example/test_main.py",
    ]
    before = [path.read_bytes() for path in paths]
    refs = git(repository, "show-ref")
    for arguments in (("diff",), ("diff", "--staged"), ("show",)):
        result = run_cli(repository, *arguments)
        if result.returncode != 0:
            raise AssertionError(result)
    if before != [path.read_bytes() for path in paths] or refs != git(
        repository,
        "show-ref",
    ):
        message = "Git view changed repository state"
        raise AssertionError(message)
    if (repository / ".gitattributes").exists() or (
        repository / ".git/info/attributes"
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
def test_git_rejects_views_that_cannot_produce_test_name_diffs(
    repository: Path,
    arguments: tuple[str, ...],
) -> None:
    """Blob/tree display and raw-source modes are not sentence diffs."""
    result = run_cli(repository, *arguments)
    if result.returncode == 0 or result.stdout or not result.stderr:
        raise AssertionError(result)


@pytest.mark.parametrize("committed", [False, True])
def test_git_rejects_symlink_diffs_in_working_and_historical_files(
    repository: Path,
    *,
    committed: bool,
) -> None:
    """Git must not display symlink targets as test sentences."""
    source = repository / "packages/example/test_main.py"
    source.unlink()
    source.symlink_to("PRIVATE_TARGET")
    if committed:
        git(repository, "add", "packages/example/test_main.py")
        git(repository, "commit", "-qm", "Link test file")
        source.unlink()
        source.write_text("def test_working(): pass\n")
    result = run_cli(repository, "show" if committed else "diff")
    if result.returncode == 0 or result.stdout or "regular files" not in result.stderr:
        raise AssertionError(result)


def test_git_rename_detection_and_formatting_remain_native(repository: Path) -> None:
    """Git handles file renames and user-selected patch presentation."""
    git(repository, "reset", "--hard", "HEAD")
    git(repository, "mv", "packages/example", "packages/renamed")
    renamed = run_cli(repository, "diff", "--staged", "-M")
    if (
        renamed.returncode != 0
        or "rename to packages/renamed/test_main.py" not in renamed.stdout
    ):
        raise AssertionError(renamed)
    if "main.py" in renamed.stdout.replace("test_main.py", ""):
        raise AssertionError(renamed.stdout)
    formatted = run_cli(repository, "show", "--format=%s", "--color=never", "-U0")
    if formatted.returncode != 0 or not formatted.stdout.startswith(
        "Rename the test\n",
    ):
        raise AssertionError(formatted)


def test_merge_show_uses_gits_combined_sentence_diff(repository: Path) -> None:
    """Preserve combined merge presentation instead of inventing a first parent."""
    git(repository, "reset", "--hard", "HEAD")
    git(repository, "branch", "side", "HEAD~1")
    git(repository, "checkout", "-q", "side")
    (repository / "packages/example/test_main.py").write_text("def test_side(): pass\n")
    git(repository, "commit", "-qam", "Side change")
    git(repository, "checkout", "-q", "-")
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
        cwd=repository,
        capture_output=True,
        check=False,
        timeout=10,
    )
    (repository / "packages/example/test_main.py").write_text(
        "def test_merged(): pass\n",
    )
    git(repository, "add", "packages/example/test_main.py")
    git(repository, "commit", "-qm", "Resolve merge")
    result = run_cli(repository, "show", "--color=never")
    if (
        result.returncode != 0
        or "diff --cc" not in result.stdout
        or "++test merged" not in result.stdout
    ):
        raise AssertionError(result)


def test_staged_diff_works_before_the_first_commit(tmp_path: Path) -> None:
    """Let Git compare an unborn HEAD to newly staged tests."""
    git(tmp_path, "init", "-q")
    make_package(tmp_path, "example", "def test_first(): pass\n")
    git(tmp_path, "add", ".")
    result = run_cli(tmp_path, "diff", "--staged")
    if result.returncode != 0 or "+test first" not in result.stdout:
        raise AssertionError(result)


def test_git_views_preserve_definition_order_while_listings_remain_sorted(
    tmp_path: Path,
) -> None:
    """Appended tests remain appended in patches, including methods and async tests."""
    git(tmp_path, "init", "-q")
    package = make_package(tmp_path, "example", "def test_zebra(): pass\n")
    git(tmp_path, "add", ".")
    git(tmp_path, "commit", "-qm", "Initial test")
    source = package / "test_main.py"
    source.write_text(
        "def test_zebra(): pass\n"
        "class TestBehavior:\n    def test_middle(self): pass\n"
        "async def test_alpha(): pass\n",
    )
    expected = " test zebra\n+test middle\n+test alpha\n"
    working = run_cli(tmp_path, "diff", "--color=never")
    git(tmp_path, "add", ".")
    staged = run_cli(tmp_path, "diff", "--staged", "--color=never")
    git(tmp_path, "commit", "-qm", "Append tests")
    committed = run_cli(tmp_path, "show", "--color=never")
    for result in (working, staged, committed):
        if result.returncode != 0 or expected not in result.stdout:
            raise AssertionError(result)
    listing = run_cli(package)
    if (
        listing.returncode != 0
        or listing.stdout != "test alpha\ntest middle\ntest zebra\n"
    ):
        raise AssertionError(listing)
