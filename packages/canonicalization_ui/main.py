#!/usr/bin/env python3
# Copyright (c) 2026- Paschalis Bizopoulos
"""Browse canonical HOME repositories without executing their source."""

from __future__ import annotations

import ast
import os
import subprocess
import sys
from dataclasses import dataclass, field
from pathlib import Path
from typing import TYPE_CHECKING, ClassVar

from rich.text import Text
from textual.app import App, ComposeResult
from textual.widgets import Footer, Static, Tree

if TYPE_CHECKING:
    from textual.widgets.tree import TreeNode


@dataclass
class Entry:
    """One directory, test, or diagnostic in the displayed tree."""

    key: str
    label: str
    children: list[Entry] = field(default_factory=list)
    expanded: bool = True


def diagnostic(key: str, error: object) -> Entry:
    """Represent an error as inert, readable text."""
    return Entry(key + ":error", f"[unavailable] {error}")


def qualified_name(node: ast.expr) -> str:
    """Read a dotted Python name without evaluating it."""
    if isinstance(node, ast.Name):
        return node.id
    if isinstance(node, ast.Attribute):
        return qualified_name(node.value) + "." + node.attr
    return ""


def hypothesis_names(module: ast.Module) -> set[str]:
    """Resolve the supported import spellings of Hypothesis given."""
    given_names: set[str] = set()
    for node in module.body:
        if isinstance(node, ast.Import):
            given_names.update(
                (alias.asname or alias.name) + ".given"
                for alias in node.names
                if alias.name == "hypothesis"
            )
        elif isinstance(node, ast.ImportFrom) and node.module == "hypothesis":
            given_names.update(
                alias.asname or alias.name
                for alias in node.names
                if alias.name == "given"
            )
    return given_names


def unittest_classes(module: ast.Module) -> set[str]:
    """Recognize unittest subclasses regardless of their class names."""
    bases: set[str] = set()
    case_types = {"TestCase", "IsolatedAsyncioTestCase"}
    for node in module.body:
        if isinstance(node, ast.Import):
            bases.update(
                (alias.asname or alias.name) + "." + case
                for alias in node.names
                if alias.name == "unittest"
                for case in case_types
            )
        elif isinstance(node, ast.ImportFrom) and node.module == "unittest":
            bases.update(
                alias.asname or alias.name
                for alias in node.names
                if alias.name in case_types
            )
    classes = [node for node in module.body if isinstance(node, ast.ClassDef)]
    found: set[str] = set()
    while additions := {
        node.name
        for node in classes
        if node.name not in found
        and any(qualified_name(base) in bases | found for base in node.bases)
    }:
        found.update(additions)
    return found


def discover_tests(path: Path) -> list[Entry]:
    """List source-level pytest definitions and recognized Hypothesis decorators."""
    if path.is_symlink():
        return [diagnostic(str(path), "linked test file")]
    try:
        module = ast.parse(path.read_bytes(), filename=str(path))
    except FileNotFoundError:
        return []
    except (OSError, SyntaxError, UnicodeError) as error:
        return [diagnostic(str(path), error)]
    given_names = hypothesis_names(module)
    case_classes = unittest_classes(module)
    result = []
    definitions: list[tuple[str, ast.stmt]] = []
    for node in module.body:
        if isinstance(node, ast.ClassDef) and (
            node.name.startswith("Test") or node.name in case_classes
        ):
            definitions.extend((node.name + "::", child) for child in node.body)
        else:
            definitions.append(("", node))
    for prefix, node in definitions:
        if not isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
            continue
        if not node.name.startswith("test_"):
            continue
        property_test = any(
            qualified_name(
                decorator.func if isinstance(decorator, ast.Call) else decorator,
            )
            in given_names
            for decorator in node.decorator_list
        )
        name = prefix + node.name
        result.append(
            Entry(
                str(path) + "::" + name,
                name + (" [property]" if property_test else ""),
            ),
        )
    return sorted(result, key=lambda entry: entry.label)


def repository_entries(root: Path) -> list[Entry]:
    """Read the two canonical resource directories of a local flake."""
    if not root.is_dir():
        return [diagnostic(str(root), "checkout missing")]
    if not (root / "flake.nix").is_file():
        return [diagnostic(str(root), "no flake.nix")]
    result = []
    for name in ("hosts", "packages"):
        directory = root / name
        if directory.is_symlink() or not directory.exists():
            continue
        group = Entry(str(directory), name)
        try:
            for path in sorted(directory.iterdir()):
                if path.is_symlink() or not path.is_dir():
                    continue
                children = (
                    discover_tests(path / "test_main.py") if name == "packages" else []
                )
                group.children.append(
                    Entry(str(path), path.name, children, expanded=False),
                )
        except OSError as error:
            group.children.append(diagnostic(str(directory), error))
        if group.children:
            result.append(group)
    return result


def module_paths(modules: Path) -> list[str]:
    """Read registered paths with Git's configuration parser."""
    if not modules.exists():
        return []
    if modules.is_symlink():
        message = "linked .gitmodules"
        raise ValueError(message)
    completed = subprocess.run(  # noqa: S603
        [  # noqa: S607
            "git",
            "config",
            "--file",
            str(modules),
            "--null",
            "--get-regexp",
            r"^submodule\..*\.path$",
        ],
        capture_output=True,
        check=False,
        timeout=10,
    )
    if completed.returncode not in (0, 1) or completed.stderr:
        message = os.fsdecode(completed.stderr).strip() or "invalid .gitmodules"
        raise ValueError(message)
    return sorted(
        {
            os.fsdecode(record.partition(b"\n")[2])
            for record in completed.stdout.split(b"\0")
            if record
        },
    )


def discover(home: Path) -> Entry:
    """Build a bounded tree from HOME's registered submodule paths."""
    root = Entry(str(home), str(home))
    try:
        paths = module_paths(home / ".gitmodules")
    except (OSError, ValueError, subprocess.TimeoutExpired) as error:
        root.children.append(diagnostic(str(home / ".gitmodules"), error))
        return root
    nodes = {home: root}
    for value in paths:
        relative = Path(value)
        if relative.is_absolute() or ".." in relative.parts or not relative.parts:
            root.children.append(diagnostic(value, f"invalid submodule path: {value}"))
            continue
        parent = root
        current = home
        for part in relative.parts:
            current /= part
            if current not in nodes:
                nodes[current] = Entry(str(current), part)
                parent.children.append(nodes[current])
            parent = nodes[current]
            if current.is_symlink():
                parent.children = [diagnostic(str(current), "linked directory")]
                break
        else:
            parent.children = repository_entries(current)
    if not root.children:
        root.children.append(Entry("empty", "No registered repositories"))
    return root


class CanonicalTree(Tree[Entry]):
    """Navigate resources with arrow keys or Vim-style keys."""

    BINDINGS: ClassVar = [
        ("j", "cursor_down", "Down"),
        ("k", "cursor_up", "Up"),
        ("l", "expand_node", "Expand"),
        ("h", "collapse_node", "Collapse"),
    ]

    def action_expand_node(self) -> None:
        """Expand the current branch without toggling it closed."""
        if self.cursor_node is not None and self.cursor_node.allow_expand:
            self.cursor_node.expand()

    def action_collapse_node(self) -> None:
        """Collapse the current branch without toggling it open."""
        if self.cursor_node is not None:
            self.cursor_node.collapse()


class CanonicalizationUI(App[None]):
    """A read-only tree of canonical resources and their test specifications."""

    TITLE = "Canonicalization"
    BINDINGS: ClassVar = [("r", "refresh_tree", "Refresh"), ("q", "quit", "Quit")]
    CSS = "Tree { height: 1fr; } #legend { height: auto; }"

    def compose(self) -> ComposeResult:
        """Provide the navigable tree and its key legend."""
        yield CanonicalTree("HOME")
        yield Static(
            "Enter/Space: expand or collapse · [property]: Hypothesis @given",
            id="legend",
            markup=False,
        )
        yield Footer()

    def on_mount(self) -> None:
        """Load HOME independently of the invocation directory."""
        self.action_refresh_tree()
        self.query_one(Tree).focus()

    def action_refresh_tree(self) -> None:
        """Rescan and retain expansion and selection for surviving nodes."""
        tree: Tree[Entry] = self.query_one(Tree)
        states: dict[str, bool] = {}
        selected = tree.cursor_node
        selected_key = selected.data.key if selected and selected.data else None
        pending = [tree.root]
        while pending:
            node = pending.pop()
            if node.data:
                states[node.data.key] = node.is_expanded
            pending.extend(node.children)
        model = discover(Path.home())
        tree.clear()
        tree.root.set_label(Text(model.label))
        tree.root.data = model
        selection = tree.root

        def populate(node: TreeNode[Entry], entry: Entry) -> None:
            nonlocal selection
            if entry.key == selected_key:
                selection = node
            for child in entry.children:
                child_node = node.add(
                    Text(child.label),
                    data=child,
                    allow_expand=bool(child.children),
                )
                populate(child_node, child)
            if states.get(entry.key, entry.expanded):
                node.expand()
            else:
                node.collapse()

        populate(tree.root, model)
        self.call_after_refresh(tree.move_cursor, selection)


def main() -> None:
    """Launch the no-argument browser."""
    if len(sys.argv) != 1:
        sys.exit("canonicalization_ui takes no arguments")
    CanonicalizationUI().run()


if __name__ == "__main__":
    main()
