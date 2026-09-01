#!/usr/bin/env python3
# Copyright (c) 2026- Paschalis Bizopoulos
# ruff: noqa: ANN401, C901, E501, PLR0911, PLR2004, S101, S603, S607
"""Remove literal NixOS and treefmt assignments equal to option defaults."""

from __future__ import annotations

import contextlib
import json
import math
import os
import subprocess
import sys
from pathlib import Path
from typing import TYPE_CHECKING, Any

import nix_syntax

if TYPE_CHECKING:
    from tree_sitter import Node
SKIPPED_DIRECTORIES = {".agents", ".codex", ".git", "prm", "result", "target", "tmp"}
Literal = None | bool | int | float | str | list["Literal"] | dict[str, "Literal"]
Candidate = tuple[tuple[str, ...], Literal]


def find_flake_root(path: Path) -> Path | None:
    """Find the closest parent containing flake.nix."""
    current = path.resolve()
    while True:
        if (current / "flake.nix").is_file():
            return current
        if current.parent == current:
            return None
        current = current.parent


def find_nix_files(root: Path) -> list[Path]:
    """Find regular Nix files without following ignored trees or links."""
    result: list[Path] = []
    for directory, names, files in os.walk(root, followlinks=False):
        names[:] = sorted(
            name
            for name in names
            if name not in SKIPPED_DIRECTORIES
            and not (Path(directory) / name).is_symlink()
        )
        result.extend(
            path
            for name in sorted(files)
            if name.endswith(".nix")
            and not (path := Path(directory) / name).is_symlink()
            and path.is_file()
        )
    return sorted(result)


def literal(document: nix_syntax.Document, node: Node) -> Literal:
    """Decode a context-free Nix literal or raise ValueError."""
    text = document.text(node)
    if node.type == "variable_expression" and text in {"null", "true", "false"}:
        return {"null": None, "true": True, "false": False}[text]
    if node.type == "integer_expression":
        return int(text)
    if node.type == "float_expression":
        value = float(text)
        if not math.isfinite(value):
            msg = "non-finite float"
            raise ValueError(msg)
        return value
    if node.type == "string_expression" and "${" not in text:
        return json.loads(text)
    if node.type == "indented_string_expression" and "${" not in text:
        return text[2:-2]
    if node.type == "list_expression":
        return [literal(document, child) for child in node.named_children]
    if node.type == "attrset_expression" and not text.lstrip().startswith("rec"):
        binding_set = next(
            (child for child in node.named_children if child.type == "binding_set"),
            None,
        )
        values: dict[str, Literal] = {}
        for binding in [] if binding_set is None else binding_set.named_children:
            attrpath = nix_syntax.field(binding, "attrpath")
            expression = nix_syntax.field(binding, "expression")
            path = (
                None
                if attrpath is None
                else nix_syntax.static_attrpath(document, attrpath)
            )
            if (
                path is None
                or len(path) != 1
                or expression is None
                or path[0] in values
            ):
                msg = "not a literal set"
                raise ValueError(msg)
            values[path[0]] = literal(document, expression)
        return values
    msg = "not a literal"
    raise ValueError(msg)


def _returned_expression(node: Node) -> Node:
    if node.type == "function_expression":
        return _returned_expression(nix_syntax.field(node, "body") or node)
    if node.type == "let_expression":
        return _returned_expression(nix_syntax.field(node, "body") or node)
    return node


def collect_candidates(
    document: nix_syntax.Document,
    root: Node | None = None,
) -> list[Candidate]:
    """Collect literal option assignments from a module expression."""
    expression = _returned_expression(root or document.root)
    if expression.type != "attrset_expression":
        return []
    result: list[Candidate] = []

    def collect_set(set_node: Node, prefix: tuple[str, ...]) -> None:
        binding_set = next(
            (child for child in set_node.named_children if child.type == "binding_set"),
            None,
        )
        for binding in [] if binding_set is None else binding_set.named_children:
            attrpath = nix_syntax.field(binding, "attrpath")
            value = nix_syntax.field(binding, "expression")
            path = (
                None
                if attrpath is None
                else nix_syntax.static_attrpath(document, attrpath)
            )
            if path is None or value is None:
                continue
            effective = (*prefix, *path)
            if not prefix and effective[:1] == ("config",):
                effective = effective[1:]
            with contextlib.suppress(ValueError):
                result.append((effective, literal(document, value)))
            if value.type == "attrset_expression":
                collect_set(value, effective)

    collect_set(expression, ())
    return result


def treefmt_arguments(document: nix_syntax.Document) -> list[Node]:
    """Find attribute-set arguments passed to treefmt evalModule."""
    result: list[Node] = []
    for node in nix_syntax.walk(document.root):
        if node.type != "apply_expression":
            continue
        argument = nix_syntax.field(node, "argument")
        function = nix_syntax.field(node, "function")
        if (
            argument is not None
            and function is not None
            and argument.type == "attrset_expression"
            and "treefmt-nix.lib.evalModule"
            in nix_syntax.compact(document.text(function))
        ):
            result.append(argument)
    return result


def _nix_string(value: str) -> str:
    return json.dumps(value, ensure_ascii=False)


def _render_literal(value: Literal) -> str:
    if value is None:
        return "null"
    if value is True:
        return "true"
    if value is False:
        return "false"
    if isinstance(value, str):
        return _nix_string(value)
    if isinstance(value, list):
        return "[ " + " ".join(_render_literal(item) for item in value) + " ]"
    if isinstance(value, dict):
        return (
            "{ "
            + " ".join(
                f"{_nix_string(key)} = {_render_literal(item)};"
                for key, item in sorted(value.items())
            )
            + " }"
        )
    return repr(value)


def _path_list(path: tuple[str, ...]) -> str:
    return "[ " + " ".join(_nix_string(part) for part in path) + " ]"


def _run_nix(expression: str) -> Any:
    completed = subprocess.run(
        ["nix", "eval", "--impure", "--json", "--expr", expression],
        capture_output=True,
        check=False,
        text=True,
    )
    if completed.returncode != 0:
        raise RuntimeError("nix evaluation failed: " + completed.stderr)
    try:
        return json.loads(completed.stdout)
    except json.JSONDecodeError as error:
        raise RuntimeError("cannot decode Nix JSON output: " + str(error)) from error


def treefmt_defaults(
    root: Path,
    paths: set[tuple[str, ...]],
) -> dict[tuple[str, ...], Literal]:
    """Evaluate treefmt option defaults for candidate paths."""
    if not paths:
        return {}
    expression = (
        f"let flake = builtins.getFlake (toString (/. + {_nix_string(str(root))})); "
        "pkgs = import flake.inputs.nixpkgs { system = builtins.currentSystem; }; "
        "evaluated = flake.inputs.treefmt-nix.lib.evalModule pkgs {}; "
        f"paths = [ {' '.join(_path_list(path) for path in sorted(paths))} ]; "
        "at = value: path: if path == [] then { success = true; inherit value; } else let key = builtins.head path; rest = builtins.tail path; in if builtins.isAttrs value && builtins.hasAttr key value then at value.${key} rest else { success = false; }; "
        'one = path: let attempt = at evaluated.options path; raw = if attempt.success && builtins.isAttrs attempt.value && attempt.value ? default then attempt.value.default else throw "missing option default"; tried = builtins.tryEval (builtins.deepSeq raw raw); in if tried.success then [{ inherit path; default = tried.value; }] else []; '
        "in builtins.concatMap one paths"
    )
    return {tuple(record["path"]): record["default"] for record in _run_nix(expression)}


def nixos_removals(
    root: Path,
    candidates: list[Candidate],
) -> dict[Path, set[tuple[str, ...]]]:
    """Resolve matching NixOS defaults and their defining source files."""
    if not candidates:
        return {}
    configurations = _run_nix(
        f"let f = builtins.getFlake (toString (/. + {_nix_string(str(root))})); in builtins.attrNames (f.nixosConfigurations or {{}})",
    )
    if not configurations:
        return {}
    unique_candidates = {
        (path, json.dumps(value, sort_keys=True, ensure_ascii=False))
        for path, value in candidates
    }
    rendered = (
        "[ "
        + " ".join(
            f"{{ path = {_path_list(path)}; value = {_render_literal(json.loads(value))}; }}"
            for path, value in sorted(unique_candidates)
        )
        + " ]"
    )
    expression = (
        f"let f = builtins.getFlake (toString (/. + {_nix_string(str(root))})); cs = f.nixosConfigurations or {{}}; names = {json.dumps(configurations)}; candidates = {rendered}; "
        "at = value: path: if path == [] then { success = true; inherit value; } else let key = builtins.head path; rest = builtins.tail path; in if builtins.isAttrs value && builtins.hasAttr key value then at value.${key} rest else { success = false; }; "
        "eq = a: b: builtins.toJSON a == builtins.toJSON b; files = name: candidate: let option = at (builtins.getAttr name cs).options candidate.path; raw = if option.success && option.value ? default && eq candidate.value option.value.default then builtins.concatMap (d: if builtins.isAttrs d && d ? file then [d.file] else []) (option.value.definitionsWithLocations or []) else []; tried = builtins.tryEval (builtins.deepSeq raw raw); in if tried.success then tried.value else []; "
        "one = candidate: { inherit (candidate) path; files = builtins.concatMap (name: files name candidate) names; }; in builtins.map one candidates"
    )
    removals: dict[Path, set[tuple[str, ...]]] = {}
    for record in _run_nix(expression):
        for source in record["files"]:
            marker = source.find("-source/")
            relative = source[marker + 8 :] if marker >= 0 else None
            local = root / relative if relative is not None else Path(source)
            removals.setdefault(local.resolve(), set()).add(tuple(record["path"]))
    return removals


def rewrite(
    document: nix_syntax.Document,
    removals: set[tuple[str, ...]],
    root: Node | None = None,
) -> str:
    """Remove selected paths and parents that become empty."""
    target = _returned_expression(root or document.root)

    def rewrite_set(node: Node, prefix: tuple[str, ...]) -> tuple[str, bool]:
        binding_set = next(
            (child for child in node.named_children if child.type == "binding_set"),
            None,
        )
        if binding_set is None:
            return document.text(node), True
        rendered: list[str] = []
        for binding in binding_set.named_children:
            attrpath = nix_syntax.field(binding, "attrpath")
            value = nix_syntax.field(binding, "expression")
            path = (
                None
                if attrpath is None
                else nix_syntax.static_attrpath(document, attrpath)
            )
            if path is None or value is None:
                rendered.append(document.text(binding))
                continue
            effective = (*prefix, *path)
            if not prefix and effective[:1] == ("config",):
                effective = effective[1:]
            if effective in removals:
                continue
            value_text = document.text(value)
            if value.type == "attrset_expression":
                value_text, empty = rewrite_set(value, effective)
                if empty:
                    continue
            rendered.append(f"{document.text(attrpath)} = {value_text};")
        prefix_text = "rec " if document.text(node).lstrip().startswith("rec") else ""
        return prefix_text + "{ " + " ".join(rendered) + " }", not rendered

    transformed, _ = rewrite_set(target, ())
    source = document.source
    replaced = nix_syntax.apply_edits(
        source,
        [(target.start_byte, target.end_byte, transformed.encode())],
    ).decode()
    nix_syntax.parse(replaced, document.path)
    return replaced


def process_repository(root: Path) -> None:
    """Parse, resolve defaults, and safely rewrite a flake repository."""
    documents = {
        path: nix_syntax.parse(path.read_bytes(), str(path))
        for path in find_nix_files(root)
    }
    candidates = {
        path: collect_candidates(document) for path, document in documents.items()
    }
    nixos = nixos_removals(
        root,
        [candidate for values in candidates.values() for candidate in values],
    )
    treefmt_nodes = {
        path: treefmt_arguments(document) for path, document in documents.items()
    }
    treefmt_candidates = [
        candidate
        for path, nodes in treefmt_nodes.items()
        for node in nodes
        for candidate in collect_candidates(documents[path], node)
    ]
    defaults = treefmt_defaults(root, {path for path, _ in treefmt_candidates})
    for path, document in documents.items():
        removals = set(nixos.get(path.resolve(), set()))
        for node in treefmt_nodes[path]:
            removals.update(
                candidate_path
                for candidate_path, value in collect_candidates(document, node)
                if defaults.get(candidate_path, object()) == value
            )
        if removals:
            nix_syntax.write_if_changed(path, rewrite(document, removals))


def main() -> None:
    """Process the current or explicitly selected flake repository."""
    if len(sys.argv) > 2:
        raise SystemExit(1)
    argument = Path(sys.argv[1] if len(sys.argv) == 2 else ".")
    if not argument.is_dir():
        print(f"error: no such flake/repository directory: {argument}", file=sys.stderr)  # noqa: T201
        raise SystemExit(1)
    root = find_flake_root(argument)
    if root is None:
        raise SystemExit(1)
    try:
        process_repository(root)
    except (OSError, ValueError, RuntimeError, nix_syntax.NixSyntaxError) as error:
        print(f"error: {error}", file=sys.stderr)  # noqa: T201
        raise SystemExit(1) from error


def test_literal_candidates_and_rewrite() -> None:
    """Collects literal options and removes empty structural parents."""
    document = nix_syntax.parse("{ services = { demo.enable = false; }; keep = true; }")
    assert (("services", "demo", "enable"), False) in collect_candidates(document)
    output = rewrite(document, {("services", "demo", "enable")})
    assert "services" not in output
    assert "keep = true;" in output


if __name__ == "__main__":
    main()
