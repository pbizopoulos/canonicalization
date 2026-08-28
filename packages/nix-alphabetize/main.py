#!/usr/bin/env python3
# Copyright (c) 2026- Paschalis Bizopoulos
# ruff: noqa: D101, S101, S603
"""Canonicalize ordering and nesting in Nix expressions."""

from __future__ import annotations

import os
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from itertools import pairwise
from pathlib import Path
from typing import TYPE_CHECKING

import nix_syntax

if TYPE_CHECKING:
    from tree_sitter import Node


@dataclass(frozen=True)
class Binding:
    path: tuple[str, ...] | None
    value: str | None
    raw: str
    nested: tuple[Binding, ...] | None = None


def _replace_children(document: nix_syntax.Document, node: Node) -> str:
    source = document.source[node.start_byte : node.end_byte]
    edits = []
    for child in node.named_children:
        rendered = render_node(document, child).encode()
        original = document.source[child.start_byte : child.end_byte]
        if rendered != original:
            edits.append(
                (
                    child.start_byte - node.start_byte,
                    child.end_byte - node.start_byte,
                    rendered,
                ),
            )
    return nix_syntax.apply_edits(source, edits).decode()


def _binding(document: nix_syntax.Document, node: Node) -> Binding:
    rendered = _replace_children(document, node)
    if node.type != "binding":
        return Binding(None, None, rendered)
    attrpath = nix_syntax.field(node, "attrpath")
    expression = nix_syntax.field(node, "expression")
    if attrpath is None or expression is None:
        return Binding(None, None, rendered)
    path = nix_syntax.static_attrpath(document, attrpath)
    value = render_node(document, expression)
    nested = _nested_bindings(document, expression)
    return Binding(path, value, rendered, nested)


def _nested_bindings(
    document: nix_syntax.Document,
    expression: Node,
) -> tuple[Binding, ...] | None:
    if expression.type != "attrset_expression" or document.text(
        expression,
    ).lstrip().startswith("rec"):
        return None
    binding_set = next(
        (child for child in expression.named_children if child.type == "binding_set"),
        None,
    )
    if binding_set is None:
        return ()
    bindings = tuple(_binding(document, child) for child in binding_set.named_children)
    return bindings if all(binding.path is not None for binding in bindings) else None


def _paths_unambiguous(paths: list[tuple[str, ...]]) -> bool:
    ordered = sorted(paths)
    return all(right[: len(left)] != left for left, right in pairwise(ordered))


def _normalize(bindings: list[Binding]) -> list[Binding]:
    static = [binding for binding in bindings if binding.path]
    opaque = [binding for binding in bindings if not binding.path]
    groups: dict[str, list[Binding]] = {}
    for binding in static:
        assert binding.path is not None
        groups.setdefault(binding.path[0], []).append(binding)
    result = opaque
    for root in sorted(groups):
        group = groups[root]
        paths = [binding.path for binding in group if binding.path is not None]
        compatible = (
            len(group) == 1
            and (
                len(paths[0]) > 1
                or (group[0].nested is not None and bool(group[0].nested))
            )
        ) or (
            len(group) > 1
            and all(len(path) > 1 for path in paths)
            and _paths_unambiguous(paths)
        )
        if not compatible:
            result.extend(sorted(group, key=lambda item: item.path or ()))
            continue
        nested: list[Binding] = []
        for binding in group:
            assert binding.path is not None
            if len(binding.path) > 1:
                nested.append(
                    Binding(
                        binding.path[1:],
                        binding.value,
                        binding.raw,
                        binding.nested,
                    ),
                )
            elif binding.nested is not None:
                nested.extend(binding.nested)
        normalized = _normalize(nested)
        if len(normalized) == 1 and normalized[0].path:
            child = normalized[0]
            result.append(
                Binding((root, *child.path), child.value, child.raw, child.nested),
            )
        else:
            result.append(Binding((root,), _render_set(normalized), ""))
    return result


def _render_binding(binding: Binding) -> str:
    if binding.path is None or binding.value is None:
        return binding.raw.strip()
    keys = ".".join(_render_key(key) for key in binding.path)
    return f"{keys} = {binding.value.strip()};"


def _render_key(key: str) -> str:
    if (
        key
        and (key[0].isalpha() or key[0] == "_")
        and all(character.isalnum() or character in "_-'" for character in key)
    ):
        return key
    escaped = key.replace("\\", "\\\\").replace('"', '\\"').replace("${", "\\${")
    return f'"{escaped}"'


def _render_set(bindings: list[Binding]) -> str:
    return "{ " + " ".join(_render_binding(binding) for binding in bindings) + " }"


def render_node(document: nix_syntax.Document, node: Node) -> str:
    """Render a node after recursively applying canonical ordering."""
    if node.type == "binding_set":
        bindings = [_binding(document, child) for child in node.named_children]
        return " ".join(_render_binding(binding) for binding in _normalize(bindings))
    if node.type == "formals":
        formals = [child for child in node.named_children if child.type == "formal"]
        rendered = sorted(
            (_replace_children(document, child) for child in formals),
            key=lambda text: text.split("?", 1)[0].strip(),
        )
        if any(child.type == "ellipses" for child in node.named_children):
            rendered.append("...")
        return "{ " + ", ".join(rendered) + " }"
    if node.type == "list_expression":
        elements = [render_node(document, child) for child in node.named_children]
        contains_string = any("string" in child.type for child in node.named_children)
        if not contains_string:
            elements.sort()
        return "[ " + " ".join(elements) + " ]"
    return (
        _replace_children(document, node)
        if node.named_child_count
        else document.text(node)
    )


def format_text(source: str, path: str = "<expression>") -> str:
    """Sort a Nix expression and apply deterministic formatting."""
    document = nix_syntax.parse(source, path)
    transformed = render_node(document, document.root)
    return nix_syntax.format_source(transformed, path)


def format_file(path: Path) -> bool:
    """Format one file and report whether processing succeeded."""
    try:
        source = path.read_text(encoding="utf-8")
        formatted = format_text(source, str(path))
        nix_syntax.write_if_changed(path, formatted)
    except (OSError, UnicodeError, nix_syntax.NixSyntaxError, ValueError) as error:
        print(f"error: {path}: {error}", file=sys.stderr)  # noqa: T201
        return False
    return True


def main() -> None:
    """Format every supplied Nix file."""
    if not all(format_file(Path(argument)) for argument in sys.argv[1:]):
        raise SystemExit(1)


def test_preserves_string_order_and_sorts_other_constructs() -> None:
    """Canonicalizes formals, bindings, and non-string lists."""
    formatted = format_text(
        '{ z = [ 3 1 2 ]; strings = [ "c" "a" ]; f = { z, a }: z + a; }',
    )
    assert (
        formatted.index("f =") < formatted.index("strings =") < formatted.index("z =")
    )
    assert formatted.index("1") < formatted.index("2") < formatted.index("3")
    assert formatted.index('"c"') < formatted.index('"a"')


def test_dotted_bindings_collapse_safely() -> None:
    """Collapses compatible bindings without changing conflicting paths."""
    formatted = format_text("{ b.z = 1; b.x = 2; a = 1; }")
    assert "b = {" in formatted
    assert "x = 2;" in formatted
    assert "z = 1;" in formatted
    conflicting = format_text("{ a = 1; a.b = 2; }")
    assert "a = 1;" in conflicting
    assert "a.b = 2;" in conflicting


def test_installed_executable_formats_files() -> None:
    """Runs the installed command on explicit file paths."""
    executable = os.environ.get("PACKAGE_E2E_EXECUTABLE")
    if executable is None:
        return
    with tempfile.TemporaryDirectory() as directory:
        path = Path(directory) / "example.nix"
        path.write_text("{ b = 2; a = 1; }", encoding="utf-8")
        completed = subprocess.run(
            [executable, str(path)],
            capture_output=True,
            check=False,
            text=True,
        )
        assert completed.returncode == 0
        assert not completed.stdout
        assert not completed.stderr
        assert path.read_text(encoding="utf-8").index("a = 1") < path.read_text(
            encoding="utf-8",
        ).index("b = 2")


if __name__ == "__main__":
    main()
