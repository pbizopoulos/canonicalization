# Copyright (c) 2026- Paschalis Bizopoulos
"""Tests for nix_remove_defaults."""

from __future__ import annotations

import json

import nix_syntax
from hypothesis import example, given
from hypothesis import strategies as st

from packages.nix_remove_defaults.main import (
    collect_candidates,
    rewrite,
)


def test_literal_candidates_and_rewrite() -> None:
    """Collects literal options and removes empty structural parents."""
    document = nix_syntax.parse("{ services = { demo.enable = false; }; keep = true; }")
    if (("services", "demo", "enable"), False) not in collect_candidates(document):
        raise AssertionError
    output = rewrite(document, {("services", "demo", "enable")})
    if not ("services" not in output):
        raise AssertionError
    if "keep = true;" not in output:
        raise AssertionError


_OPTION = st.text(alphabet="abcxyz", min_size=1, max_size=5)


@given(
    entries=st.dictionaries(
        st.tuples(_OPTION, _OPTION),
        st.tuples(st.integers(0, 1000), st.booleans()),
        max_size=12,
    ),
    nested=st.booleans(),
    config=st.booleans(),
    wrapper=st.sampled_from(["", "{ lib, ... }: ", "let unrelated = 1; in "]),
)
@example(
    entries={("a", "x"): (0, True), ("a", "y"): (1, False), ("b", "z"): (2, True)},
    nested=True,
    config=True,
    wrapper="{ lib, ... }: ",
)
@example(entries={}, nested=False, config=False, wrapper="")
def test_rewrite_removes_only_selected_options(
    entries: dict[tuple[str, str], tuple[int, bool]],
    *,
    nested: bool,
    config: bool,
    wrapper: str,
) -> None:
    """Remove selected leaves and newly empty parents, preserving other values."""
    groups: dict[str, list[str]] = {}
    bindings = []
    for (parent, child), (value, _) in entries.items():
        groups.setdefault(parent, []).append(f"{json.dumps(child)} = {value};")
        bindings.append(f"{json.dumps(parent)}.{json.dumps(child)} = {value};")
    if nested:
        bindings = [
            f"{json.dumps(parent)} = {{ {' '.join(children)} }};"
            for parent, children in groups.items()
        ]
    body = (
        "{ empty = {}; untouched = { empty = {}; }; dynamic = builtins.currentSystem; "
        + " ".join(bindings)
        + " }"
    )
    source = wrapper + ("{ config = " + body + "; }" if config else body)
    removals: set[tuple[str, ...]] = {
        path for path, (_, remove) in entries.items() if remove
    }
    expected = {path: value for path, (value, remove) in entries.items() if not remove}
    before = dict(collect_candidates(nix_syntax.parse(source)))
    if {path: before.get(path) for path in entries} != {
        path: value for path, (value, _) in entries.items()
    }:
        msg = "candidate collection lost a generated option"
        raise AssertionError(msg)
    output = rewrite(nix_syntax.parse(source), removals)
    after = dict(collect_candidates(nix_syntax.parse(output)))
    actual = {path: value for path, value in after.items() if isinstance(value, int)}
    if actual != expected:
        msg = "rewrite removed an unselected option or retained a selected one"
        raise AssertionError(msg)
    if after.get(("empty",)) != {} or after.get(("untouched", "empty")) != {}:
        msg = "rewrite removed a pre-existing empty set"
        raise AssertionError(msg)
    for parent in groups:
        if not any(path[0] == parent for path in expected) and (parent,) in after:
            msg = "rewrite retained a newly empty parent"
            raise AssertionError(msg)
    if "dynamic = builtins.currentSystem;" not in output or not output.startswith(
        wrapper,
    ):
        msg = "rewrite changed unrelated expressions"
        raise AssertionError(msg)
    if rewrite(nix_syntax.parse(output), removals) != output:
        msg = "removing the same options twice changed the result"
        raise AssertionError(msg)
