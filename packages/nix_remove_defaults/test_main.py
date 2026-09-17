# Copyright (c) 2026- Paschalis Bizopoulos
"""Tests for nix_remove_defaults."""

from __future__ import annotations

import nix_syntax

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
