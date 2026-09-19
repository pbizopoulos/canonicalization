# Copyright (c) 2026- Paschalis Bizopoulos
"""End-to-end requirements for the canonical HOME browser."""

from __future__ import annotations

import asyncio
import os
import pty
import select
import subprocess
import termios
import time
from typing import TYPE_CHECKING

from textual.widgets import Tree

from packages.canonicalization_ui.main import CanonicalizationUI, Entry, discover

if TYPE_CHECKING:
    from pathlib import Path

    import pytest
    from textual.widgets.tree import TreeNode


def fixture_home(home: Path) -> Path:
    """Create a representative registered flake with packages and a host."""
    (home / ".gitmodules").write_text(
        '[submodule "demo"]\npath = github.com/owner/demo\n'
        '[submodule "missing"]\npath = forge.example/owner/missing\n',
    )
    repository = home / "github.com/owner/demo"
    package = repository / "packages/example"
    package.mkdir(parents=True)
    (repository / "flake.nix").touch()
    (repository / "hosts/laptop").mkdir(parents=True)
    (repository / "packages/document").mkdir()
    (repository / "tmp/hidden").mkdir(parents=True)
    (package / "test_main.py").write_text(
        'raise RuntimeError("must never execute")\n'
        "from hypothesis import given as generated\n"
        "import hypothesis as h\n"
        "@generated(value=strategy)\n"
        "def test_property(): pass\n"
        "@h.given(value=strategy)\n"
        "async def test_async_property(): pass\n"
        "@pytest.mark.parametrize('value', [1, 2])\n"
        "def test_plain(value): pass\n"
        "def helper():\n"
        "    def test_nested(): pass\n"
        "class TestBehavior:\n"
        "    def test_method(self): pass\n"
        "class Helper:\n"
        "    def test_hidden(self): pass\n",
    )
    return package


def flatten(entry: Entry) -> list[Entry]:
    """Read all model nodes in display order."""
    return [entry, *(item for child in entry.children for item in flatten(child))]


def test_home_tree_shows_packages_hosts_and_property_tests_without_importing_source(
    tmp_path: Path,
) -> None:
    """Preserve hierarchy, include all package kinds, and classify source tests."""
    package = fixture_home(tmp_path)
    entries = flatten(discover(tmp_path))
    labels = [entry.label for entry in entries]
    expected = [
        "github.com",
        "owner",
        "demo",
        "hosts",
        "laptop",
        "packages",
        "document",
        "example",
        "TestBehavior::test_method",
        "test_async_property [property]",
        "test_plain",
        "test_property [property]",
    ]
    start = labels.index("github.com")
    if labels[start:] != expected:
        raise AssertionError(labels)
    found = next(entry for entry in entries if entry.key == str(package))
    if found.expanded or len(found.children) != len(expected[-4:]):
        raise AssertionError(found)
    if not any("checkout missing" in label for label in labels):
        raise AssertionError(labels)


def test_unavailable_sources_are_reported_and_links_are_not_followed(
    tmp_path: Path,
) -> None:
    """Keep valid branches when a test is malformed or a directory is linked."""
    package = fixture_home(tmp_path)
    (package / "test_main.py").write_text("def broken(")
    (package.parent / "linked").symlink_to(package, target_is_directory=True)
    with (tmp_path / ".gitmodules").open("a") as stream:
        stream.write('[submodule "escape"]\npath = ../outside\n')
        stream.write('[submodule "link"]\npath = linked/repo\n')
    (tmp_path / "linked").symlink_to(package.parent.parent, target_is_directory=True)
    labels = [entry.label for entry in flatten(discover(tmp_path))]
    if not any("invalid submodule path" in label for label in labels):
        raise AssertionError(labels)
    if not any("linked directory" in label for label in labels):
        raise AssertionError(labels)
    if (
        not any("was never closed" in label for label in labels)
        or "laptop" not in labels
    ):
        raise AssertionError(labels)
    (package / "test_main.py").unlink()
    (package / "test_main.py").symlink_to(package.parent.parent / "flake.nix")
    if not any(
        "linked test file" in entry.label for entry in flatten(discover(tmp_path))
    ):
        msg = "linked source was not reported"
        raise AssertionError(msg)


def test_empty_or_malformed_home_metadata_is_readable(tmp_path: Path) -> None:
    """Display empty and invalid metadata states without crashing."""
    if discover(tmp_path).children[0].label != "No registered repositories":
        msg = "missing empty state"
        raise AssertionError(msg)
    modules = tmp_path / ".gitmodules"
    modules.write_text("")
    if discover(tmp_path).children[0].label != "No registered repositories":
        msg = "missing empty configuration state"
        raise AssertionError(msg)
    modules.write_text("[invalid")
    if "[unavailable]" not in discover(tmp_path).children[0].label:
        msg = "missing metadata error"
        raise AssertionError(msg)


def find_tree_node(tree: Tree[Entry], key: str) -> TreeNode[Entry]:
    """Find a resource in the UI by its stable key."""
    pending = [tree.root]
    while pending:
        node = pending.pop()
        if node.data and node.data.key == key:
            return node
        pending.extend(node.children)
    raise AssertionError(key)


def test_tree_navigation_refresh_and_quit_use_home_from_any_directory(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """Drive the actual UI and retain selected expanded packages on refresh."""
    package = fixture_home(tmp_path)
    monkeypatch.setenv("HOME", str(tmp_path))
    monkeypatch.chdir(package)

    async def exercise() -> None:
        app = CanonicalizationUI()
        async with app.run_test() as pilot:
            await pilot.pause()
            tree: Tree[Entry] = app.query_one(Tree)
            if tree.root.data is None or tree.root.data.key != str(tmp_path):
                msg = "UI did not use HOME"
                raise AssertionError(msg)
            tree.move_cursor(find_tree_node(tree, str(package)))
            await pilot.press("l", "l")
            await pilot.pause()
            if tree.cursor_node is None or not tree.cursor_node.is_expanded:
                msg = "package did not expand"
                raise AssertionError(msg)
            package_node = tree.cursor_node
            await pilot.press("j")
            if tree.cursor_node is not package_node.children[0]:
                msg = "j did not move down to the first test"
                raise AssertionError(msg)
            await pilot.press("k", "h", "h")
            if tree.cursor_node is not package_node or package_node.is_expanded:
                msg = "k/h did not return to and collapse the package"
                raise AssertionError(msg)
            await pilot.press("enter")
            (package / "test_main.py").write_text("def test_new(): pass\n")
            await pilot.press("r")
            await pilot.pause()
            selected = tree.cursor_node
            if (
                selected is None
                or selected.data is None
                or selected.data.key != str(package)
            ):
                msg = "refresh lost selection"
                raise AssertionError(msg)
            if not selected.is_expanded or [
                child.data.label for child in selected.children if child.data
            ] != ["test_new"]:
                msg = "refresh did not update expanded tests"
                raise AssertionError(msg)
            await pilot.press("q")
        if app.is_running:
            msg = "quit left application running"
            raise AssertionError(msg)

    asyncio.run(exercise())


def test_packaged_cli_starts_without_arguments_and_quits_in_a_terminal(
    tmp_path: Path,
) -> None:
    """Smoke-test the installed executable and its no-argument contract."""
    executable = os.environ["PACKAGE_E2E_EXECUTABLE"]
    fixture_home(tmp_path)
    rejected = subprocess.run(  # noqa: S603
        [executable, "unexpected"],
        capture_output=True,
        check=False,
        timeout=10,
    )
    if rejected.returncode == 0 or b"takes no arguments" not in rejected.stderr:
        raise AssertionError(rejected)
    master, slave = pty.openpty()
    termios.tcsetwinsize(slave, (40, 120))
    try:
        with subprocess.Popen(  # noqa: S603
            [executable],
            stdin=slave,
            stdout=slave,
            stderr=slave,
            cwd=tmp_path,
            env={**os.environ, "HOME": str(tmp_path), "TERM": "xterm-256color"},
        ) as process:
            output = b""
            deadline = time.monotonic() + 20
            try:
                while b"example" not in output and time.monotonic() < deadline:
                    readable, _, _ = select.select([master], [], [], 0.1)
                    if readable:
                        output += os.read(master, 65536)
                    if process.poll() is not None:
                        break
                if b"example" not in output:
                    raise AssertionError(output.decode(errors="replace"))
                os.write(master, b"q")
                if process.wait(timeout=10) != 0:
                    msg = "terminal process failed"
                    raise AssertionError(msg)
            finally:
                if process.poll() is None:
                    process.kill()
                    process.wait()
    finally:
        os.close(master)
        os.close(slave)
