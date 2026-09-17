# Copyright (c) 2026- Paschalis Bizopoulos
"""Test declaration extraction, graph semantics, and the installed command."""

from __future__ import annotations

import os
import shutil
import subprocess
from typing import TYPE_CHECKING

import pytest

from packages.python_name_graph.main import graph_from_source

if TYPE_CHECKING:
    from pathlib import Path


def test_directed_unique_graph() -> None:
    """Connect only adjacent tokens, merging lemmas across declarations."""
    if graph_from_source(
        "user_account_names = 1\nuser_account_names = 2\nnames_users = 3\n",
    ) != {
        "nodes": ["account", "name", "user"],
        "edges": [
            {"source": "account", "target": "name"},
            {"source": "name", "target": "user"},
            {"source": "user", "target": "account"},
        ],
    }:
        message = "expected sorted unique directed adjacency"
        raise AssertionError(message)


def test_normalization_and_self_loops() -> None:
    """Handle empty tokens, irregular nouns, unknowns, and collapsed edges."""
    graph = graph_from_source(
        "__CHILDREN__geese_ = 0\ncats_cat = 0\nCamelCase2 = 0\nqzxv9 = 0\n_ = 0\n",
    )
    if graph != {
        "nodes": ["camelcase2", "cat", "child", "goose", "qzxv9"],
        "edges": [
            {"source": "cat", "target": "cat"},
            {"source": "child", "target": "goose"},
        ],
    }:
        message = "expected normalized nouns, isolated tokens, and self-loops"
        raise AssertionError(message)


def test_declaration_kinds() -> None:
    """Include bindings from Python declaration and assignment forms."""
    source = """
class Widget:
    pass
def build(first, /, second=0, *args, third=0, **kwargs):
    local: int
    left, *right = external
    local += 1
    for item in external:
        pass
    with external as context:
        pass
    try:
        pass
    except Exception as error:
        pass
    [value for value in external]
    (named := external)
    match external:
        case {"key": capture, **remaining}:
            pass
        case [head, *tail] as whole:
            pass
        case _:
            pass
async def fetch():
    async for entry in external:
        pass
    async with external as resource:
        pass
type Alias[Element, *Shape, **Options] = tuple[Element]
callback = lambda argument: argument
"""
    graph = graph_from_source(source)
    if set(graph["nodes"]) != {
        "widget",
        "build",
        "first",
        "second",
        "args",
        "third",
        "kwargs",
        "local",
        "left",
        "right",
        "item",
        "context",
        "error",
        "value",
        "named",
        "capture",
        "remaining",
        "head",
        "tail",
        "whole",
        "fetch",
        "entry",
        "resource",
        "alias",
        "element",
        "shape",
        "option",
        "callback",
        "argument",
    }:
        message = "expected all declaration kinds without reference names"
        raise AssertionError(message)
    if graph["edges"] != []:
        message = "separate single-token declarations must not connect"
        raise AssertionError(message)


def test_excluded_names_and_empty_input() -> None:
    """Ignore references, imports, attributes, comments, and strings."""
    source = """
import imported_module as imported_alias
from other_module import imported_name
external.attribute_name = reference_name
external[index_name] = reference_name
del deleted_name
global global_name
"string_name"
# comment_name
match external:
    case _:
        pass
try:
    external()
except:
    pass
"""
    if graph_from_source(source) != {"nodes": [], "edges": []}:
        message = "excluded identifiers must not contribute tokens"
        raise AssertionError(message)
    if graph_from_source("") != {"nodes": [], "edges": []}:
        message = "empty source must produce an empty graph"
        raise AssertionError(message)


def test_syntax_error() -> None:
    """Expose syntax errors to Python API callers."""
    with pytest.raises(SyntaxError):
        graph_from_source("def broken(")


def test_installed_cli(tmp_path: Path) -> None:
    """Read encoded source without executing or modifying it, offline."""
    source = tmp_path / "source.py"
    contents = (
        b"# coding: latin-1\nuser_account_names = 1\n"
        b"cats_cat = 0\ngraph = 0\n\xe9 = 0\n"
        b"raise RuntimeError('must not execute')\n"
    )
    source.write_bytes(contents)
    environment = os.environ.copy()
    environment["HOME"] = str(tmp_path)
    environment["NLTK_DATA"] = str(tmp_path / "absent")
    completed = subprocess.run(  # noqa: S603
        [os.environ["PACKAGE_E2E_EXECUTABLE"], str(source)],
        capture_output=True,
        text=True,
        check=False,
        env=environment,
    )
    if completed.returncode != 0:
        message = f"installed CLI failed: {completed.stderr}"
        raise AssertionError(message)
    if completed.stderr != "":
        message = "successful CLI must not write diagnostics"
        raise AssertionError(message)
    expected = (
        'digraph {\n  "account";\n  "cat";\n  "graph";\n  "name";\n'
        '  "user";\n  "é";\n  "account" -> "name";\n'
        '  "cat" -> "cat";\n  "user" -> "account";\n}\n'
    )
    if completed.stdout != expected:
        message = "CLI must emit sorted DOT with isolated nodes and self-loops"
        raise AssertionError(message)
    if source.read_bytes() != contents:
        message = "source must remain unchanged"
        raise AssertionError(message)
    dot = shutil.which("dot")
    if dot is None:
        message = "Graphviz must be available in the test environment"
        raise AssertionError(message)
    rendered = subprocess.run(  # noqa: S603
        [dot, "-Tsvg"],
        input=completed.stdout,
        capture_output=True,
        text=True,
        check=False,
    )
    if rendered.returncode != 0 or "<svg" not in rendered.stdout:
        message = f"Graphviz must render CLI output directly: {rendered.stderr}"
        raise AssertionError(message)


def test_cli_empty_graph(tmp_path: Path) -> None:
    """Emit a valid DOT document even when the file has no declarations."""
    source = tmp_path / "empty.py"
    source.write_text("", encoding="utf-8")
    completed = subprocess.run(  # noqa: S603
        [os.environ["PACKAGE_E2E_EXECUTABLE"], str(source)],
        capture_output=True,
        text=True,
        check=False,
    )
    if completed.returncode != 0 or completed.stdout != "digraph {\n}\n":
        message = "empty input must produce an empty DOT graph"
        raise AssertionError(message)


def test_cli_errors(tmp_path: Path) -> None:
    """Report missing, malformed, and undecodable files without partial DOT."""
    for contents in [None, b"def broken(", b"# coding: utf-8\n\xff"]:
        source = tmp_path / "source.py"
        if contents is not None:
            source.write_bytes(contents)
        completed = subprocess.run(  # noqa: S603
            [os.environ["PACKAGE_E2E_EXECUTABLE"], str(source)],
            capture_output=True,
            text=True,
            check=False,
        )
        if completed.returncode == 0:
            message = "invalid input must fail"
            raise AssertionError(message)
        if completed.stdout != "":
            message = "invalid input must not emit a partial graph"
            raise AssertionError(message)
        if not (completed.stderr):
            message = "invalid input must report a diagnostic"
            raise AssertionError(message)
