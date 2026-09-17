# Copyright (c) 2026- Paschalis Bizopoulos
"""Tests for nix_alphabetize."""

from __future__ import annotations

import json
import os
import subprocess
import tempfile
from pathlib import Path
from typing import TYPE_CHECKING

import nix_syntax
from hypothesis import example, given
from hypothesis import strategies as st

from packages.nix_alphabetize.main import (
    format_text,
)

if TYPE_CHECKING:
    from tree_sitter import Node


def test_preserves_string_order_and_sorts_other_constructs() -> None:
    """Canonicalizes formals, bindings, and non-string lists."""
    formatted = format_text(
        '{ z = [ 3 1 2 ]; strings = [ "c" "a" ]; f = { z, a }: z + a; }',
    )
    if not (
        formatted.index("f =") < formatted.index("strings =") < formatted.index("z =")
    ):
        raise AssertionError
    if not (formatted.index("1") < formatted.index("2") < formatted.index("3")):
        raise AssertionError
    if not (formatted.index('"c"') < formatted.index('"a"')):
        raise AssertionError


def test_dotted_bindings_collapse_safely() -> None:
    """Collapses compatible bindings without changing conflicting paths."""
    formatted = format_text("{ b.z = 1; b.x = 2; a = 1; }")
    if "b = {" not in formatted:
        raise AssertionError
    if "x = 2;" not in formatted:
        raise AssertionError
    if "z = 1;" not in formatted:
        raise AssertionError
    conflicting = format_text("{ a = 1; a.b = 2; }")
    if "a = 1;" not in conflicting:
        raise AssertionError
    if "a.b = 2;" not in conflicting:
        raise AssertionError


def test_nested_plain_inherit_normalizes() -> None:
    """Expand plain inherited names and converge to the explicit binding form."""
    for source, expected in [
        ("{ passthru = { inherit python; }; }", "{ passthru.python = python; }"),
        ("{ a = { b = { inherit x; }; }; }", "{ a.b.x = x; }"),
        ("{ a = { inherit z x; }; }", "{ a = { x = x; z = z; }; }"),
        ("{ a = { inherit x; z = 1; }; }", "{ a = { x = x; z = 1; }; }"),
    ]:
        formatted = format_text(source)
        if formatted != expected:
            raise AssertionError(formatted)
        if format_text(formatted) != formatted:
            raise AssertionError


def test_opaque_inherit_is_preserved() -> None:
    """Retain recursive sets, sourced inherits, comments, and direct inherits."""
    for source in [
        "{ inherit python; }",
        "{ a = rec { inherit python; }; }",
        "{ a = { inherit (python) version; }; }",
        "{ a = { inherit /* keep */ python; }; }",
    ]:
        if format_text(source) != source:
            raise AssertionError


def test_installed_executable_formats_files() -> None:
    """Runs the installed command on explicit file paths."""
    executable = os.environ.get("PACKAGE_E2E_EXECUTABLE")
    if executable is None:
        return
    with tempfile.TemporaryDirectory() as directory:
        path = Path(directory) / "example.nix"
        path.write_text("{ b = 2; a = 1; }", encoding="utf-8")
        completed = subprocess.run(  # noqa: S603
            [executable, str(path)],
            capture_output=True,
            check=False,
            text=True,
        )
        if not (completed.returncode == 0):
            raise AssertionError
        if completed.stdout:
            raise AssertionError
        if completed.stderr:
            raise AssertionError
        if not (
            path.read_text(encoding="utf-8").index("a = 1")
            < path.read_text(
                encoding="utf-8",
            ).index("b = 2")
        ):
            raise AssertionError


_KEY = st.one_of(
    st.text(alphabet="abcxyz", min_size=1, max_size=5),
    st.sampled_from(["", "é", "中", "a b", "if", "or", "${literal}", "a.b", 'a"b']),
)
_BINDINGS = st.one_of(
    *(
        st.dictionaries(
            st.lists(_KEY, min_size=depth, max_size=depth).map(tuple),
            st.integers(0, 1000),
            max_size=10,
        )
        for depth in (1, 2, 3)
    ),
)


def _quoted_key(value: str) -> str:
    return json.dumps(value, ensure_ascii=False).replace("${", r"\${")


def _render_tree(entries: dict[tuple[str, ...], int]) -> str:
    groups: dict[str, dict[tuple[str, ...], int]] = {}
    for path, value in entries.items():
        groups.setdefault(path[0], {})[path[1:]] = value
    bindings = []
    for key, children in groups.items():
        rendered_value = str(children[()]) if () in children else _render_tree(children)
        bindings.append(f"{_quoted_key(key)} = {rendered_value};")
    return "{ " + " ".join(bindings) + " }"


def _integer_leaves(source: str) -> dict[tuple[str, ...], int]:
    document = nix_syntax.parse(source)
    leaves: dict[tuple[str, ...], int] = {}

    def visit(node: Node, prefix: tuple[str, ...]) -> None:
        for child in node.named_children:
            if child.type == "binding":
                attrpath = nix_syntax.field(child, "attrpath")
                value = nix_syntax.field(child, "expression")
                if attrpath is None or value is None:
                    msg = "missing binding fields"
                    raise AssertionError(msg)
                path = nix_syntax.static_attrpath(document, attrpath)
                if path is None:
                    msg = "static binding became dynamic"
                    raise AssertionError(msg)
                full_path = prefix + path
                if value.type == "attrset_expression":
                    visit(value, full_path)
                else:
                    if full_path in leaves:
                        msg = "formatter duplicated a binding"
                        raise AssertionError(msg)
                    leaves[full_path] = int(document.text(value))
            else:
                visit(child, prefix)

    visit(document.root, ())
    return leaves


@given(entries=_BINDINGS)
@example(entries={("b", "z"): 2, ("b", "a"): 1, ("a", "x"): 0})
@example(entries={("é",): 1})
@example(entries={("if",): 1})
@example(entries={})
def test_binding_normalization_preserves_the_model(
    entries: dict[tuple[str, ...], int],
) -> None:
    """Dotted, nested, and reordered bindings converge without losing leaves."""
    dotted = (
        "{ "
        + " ".join(
            f"{'.'.join(_quoted_key(part) for part in path)} = {value};"
            for path, value in entries.items()
        )
        + " }"
    )
    sources = (
        dotted,
        _render_tree(entries),
        _render_tree(dict(reversed(list(entries.items())))),
    )
    outputs = [format_text(source) for source in sources]
    for output in outputs:
        if _integer_leaves(output) != entries:
            msg = "normalization changed attribute paths or values"
            raise AssertionError(msg)
        if format_text(output) != output:
            msg = "binding normalization did not converge"
            raise AssertionError(msg)
    if len(set(outputs)) != 1:
        msg = "equivalent bindings have different canonical forms"
        raise AssertionError(msg)


@given(
    numbers=st.lists(st.integers(0, 1000), max_size=20),
    strings=st.lists(st.text(alphabet="abc é中", max_size=8), max_size=12),
)
@example(numbers=[2, 10, 2, 0], strings=["z", "a", "z"])
@example(numbers=[], strings=[])
def test_list_order_and_multiplicity(numbers: list[int], strings: list[str]) -> None:
    """Sort non-string lists lexically, but retain string-containing list order."""
    numeric = [str(number) for number in numbers]
    mixed = [*numeric, '"anchor"', *(_quoted_key(value) for value in strings)]
    for elements, expected in ((numeric, sorted(numeric)), (mixed, mixed)):
        output = format_text("[ " + " ".join(elements) + " ]")
        document = nix_syntax.parse(output)
        actual = [document.text(child) for child in document.root.named_children]
        if actual != expected or format_text(output) != output:
            msg = "list formatting changed order or multiplicity"
            raise AssertionError(msg)
