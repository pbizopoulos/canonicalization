#!/usr/bin/env python3
# Copyright (c) 2026- Paschalis Bizopoulos
"""Build Graphviz DOT graphs from English noun lemmas in Python declarations."""

from __future__ import annotations

import argparse
import ast
import itertools
import json
import sys
import tokenize
from importlib.resources import files
from typing import TypedDict

import nltk.data
from nltk.stem import WordNetLemmatizer

nltk.data.path.insert(0, str(files("python_name_graph_data")))


class Edge(TypedDict):
    """One directed connection between token lemmas."""

    source: str
    target: str


class Graph(TypedDict):
    """Sorted unique token nodes and directed edges."""

    nodes: list[str]
    edges: list[Edge]


def graph_from_source(source: str) -> Graph:
    """Parse declarations without executing source and return their token graph."""
    tree = ast.parse(source)
    names: set[str] = set()
    for node in ast.walk(tree):
        if isinstance(
            node,
            (
                ast.FunctionDef,
                ast.AsyncFunctionDef,
                ast.ClassDef,
                ast.ExceptHandler,
                ast.MatchAs,
                ast.MatchStar,
                ast.TypeVar,
                ast.ParamSpec,
                ast.TypeVarTuple,
            ),
        ):
            if node.name is not None:
                names.add(node.name)
        elif isinstance(node, ast.arg):
            names.add(node.arg)
        elif isinstance(node, ast.Name) and isinstance(node.ctx, ast.Store):
            names.add(node.id)
        elif isinstance(node, ast.MatchMapping) and node.rest is not None:
            names.add(node.rest)
    lemmatizer = WordNetLemmatizer()
    lemmas: dict[str, str] = {}
    nodes: set[str] = set()
    edges: set[tuple[str, str]] = set()
    for name in sorted(names):
        tokens = []
        for token in filter(None, name.lower().split("_")):
            if token not in lemmas:
                lemmas[token] = lemmatizer.lemmatize(token, pos="n")
            tokens.append(lemmas[token])
        nodes.update(tokens)
        edges.update(itertools.pairwise(tokens))
    return {
        "nodes": sorted(nodes),
        "edges": [
            {"source": source, "target": target} for source, target in sorted(edges)
        ],
    }


def main() -> None:
    """Print the declaration graph for one Python source file as Graphviz DOT."""
    parser = argparse.ArgumentParser(
        description=__doc__,
        epilog=(
            "Split declared names on underscores and lowercase nonempty tokens. "
            "Connect adjacent noun lemmas in order, merging duplicate nodes and "
            "edges while retaining isolated nodes and self-loops. "
            "Unknown tokens are unchanged. Imports, references, and attribute "
            "assignments are excluded. Source is never executed or modified."
        ),
    )
    parser.add_argument("file", help="Python source file to analyze without executing")
    options = parser.parse_args()
    try:
        with tokenize.open(options.file) as source_file:
            graph = graph_from_source(source_file.read())
    except (OSError, UnicodeError, SyntaxError) as error:
        sys.stderr.write(f"{parser.prog}: {error}\n")
        raise SystemExit(1) from error
    lines = ["digraph {"]
    lines.extend(
        f"  {json.dumps(node, ensure_ascii=False)};" for node in graph["nodes"]
    )
    for edge in graph["edges"]:
        source = json.dumps(edge["source"], ensure_ascii=False)
        target = json.dumps(edge["target"], ensure_ascii=False)
        lines.append(f"  {source} -> {target};")
    lines.append("}")
    sys.stdout.write("\n".join(lines) + "\n")


if __name__ == "__main__":
    main()
