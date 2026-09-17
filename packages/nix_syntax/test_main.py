# Copyright (c) 2026- Paschalis Bizopoulos
"""Tests for nix_syntax."""

from __future__ import annotations

from packages.nix_syntax.main import (
    NixSyntaxError,
    compact,
    parse,
    static_attrpath,
    walk,
)


def test_parse_extracts_static_paths_and_rejects_errors() -> None:
    """Parses bindings and rejects malformed source."""
    document = parse('{ "a".b = 1; }')
    attrpath = next(node for node in walk(document.root) if node.type == "attrpath")
    if not (static_attrpath(document, attrpath) == ("a", "b")):
        raise AssertionError
    try:
        parse("{ invalid = ; }")
    except NixSyntaxError:
        pass
    else:
        msg = "malformed source parsed successfully"
        raise AssertionError(msg)


def test_static_paths_decode_escapes_and_reject_interpolation() -> None:
    """Static paths decode Nix escapes and distinguish literal interpolation."""
    cases = [
        (r'"a\"b"."c\\d"', ('a"b', "c\\d")),
        (r'"a\nb\rc\td"', ("a\nb\rc\td",)),
        (r'"\${literal}"', ("${literal}",)),
        (r'"\q"', ("q",)),
        ('"${variable}"', None),
        (r'"\\${variable}"', None),
        ("${variable}", None),
    ]
    for source, expected in cases:
        document = parse("{ " + source + " = 1; }")
        attrpath = next(node for node in walk(document.root) if node.type == "attrpath")
        if not (static_attrpath(document, attrpath) == expected):
            raise AssertionError


def test_compact_preserves_literal_and_comment_whitespace() -> None:
    """Comparisons ignore layout but retain significant source whitespace."""
    if not (compact("{\n  a = 1;\n}") == compact("{ a = 1; }")):
        raise AssertionError
    for left, right in [
        ('"a  b"', '"a b"'),
        ("''a  b''", "''a b''"),
        ('"a\nb"', '"a b"'),
        ('"${ "a  b" }"', '"${ "a b" }"'),
        ("{ /* a  b */ a = 1; }", "{ /* a b */ a = 1; }"),
        ("{ # comment\n a = 1;\n}", "{ # comment a = 1;\n}"),
    ]:
        if not (compact(left) != compact(right)):
            raise AssertionError
