# Copyright (c) 2026- Paschalis Bizopoulos
"""Tests for nix_syntax."""

from __future__ import annotations

import json

import pytest
from hypothesis import example, given
from hypothesis import strategies as st

from packages.nix_syntax.main import (
    NixSyntaxError,
    apply_edits,
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


@given(
    chunks=st.lists(
        st.tuples(
            st.binary(max_size=12),
            st.binary(min_size=1, max_size=12),
            st.binary(max_size=12),
        ),
        max_size=12,
    ),
    tail=st.binary(max_size=12),
    insertion=st.binary(max_size=12),
)
@example(
    chunks=[(b"\xc3\xa9", b"old", b"new"), (b"", b"!", b"")],
    tail=b"end",
    insertion=b"!",
)
@example(chunks=[], tail=b"", insertion=b"new")
def test_edits_preserve_untouched_bytes(
    chunks: list[tuple[bytes, bytes, bytes]],
    tail: bytes,
    insertion: bytes,
) -> None:
    """Edits use original byte coordinates and are independent of input order."""
    original = bytearray()
    expected = bytearray()
    edits = []
    for unchanged, removed, replacement in chunks:
        original.extend(unchanged)
        start = len(original)
        original.extend(removed)
        edits.append((start, len(original), replacement))
        expected.extend(unchanged + replacement)
    original.extend(tail)
    edits.append((len(original), len(original), insertion))
    expected.extend(tail + insertion)
    for ordered in (edits, list(reversed(edits))):
        if apply_edits(bytes(original), ordered) != bytes(expected):
            msg = "edits changed bytes outside their source spans"
            raise AssertionError(msg)


@given(source=st.binary(min_size=1, max_size=100), replacement=st.binary(max_size=20))
@example(source=b"abc", replacement=b"")
def test_invalid_edit_ranges_are_rejected(source: bytes, replacement: bytes) -> None:
    """Reject out-of-bounds, reversed, and overlapping source ranges."""
    for edits in (
        [(-1, 0, replacement)],
        [(0, len(source) + 1, replacement)],
        [(1, 0, replacement)],
        [(0, len(source), replacement), (0, 1, replacement)],
    ):
        with pytest.raises(ValueError, match="overlapping or invalid"):
            apply_edits(source, edits)


_QUOTED_TEXT = st.text(alphabet='abcXYZ09 é中\\"${}\n\r\t', max_size=20)


@given(parts=st.lists(_QUOTED_TEXT, min_size=1, max_size=4))
@example(parts=["é", "${literal}", 'a"b\\c\n\t'])
def test_quoted_attribute_paths_round_trip(parts: list[str]) -> None:
    """UTF-8 byte offsets and Nix escapes preserve every static path component."""
    quoted = [
        json.dumps(part, ensure_ascii=False).replace("${", r"\${") for part in parts
    ]
    document = parse("{ " + ".".join(quoted) + " = 1; }")
    attrpath = next(node for node in walk(document.root) if node.type == "attrpath")
    if static_attrpath(document, attrpath) != tuple(parts):
        msg = "quoted attribute path did not round-trip"
        raise AssertionError(msg)


@given(
    value=_QUOTED_TEXT,
    comment=st.text(alphabet="abc é中 \t", max_size=20),
    layout=st.sampled_from([" ", "\n", "\t  ", "\r\n"]),
)
@example(value="é ${literal}\n  text", comment=" keep  spacing", layout="\n")
def test_compaction_changes_only_layout(value: str, comment: str, layout: str) -> None:
    """Layout variations converge without changing literal or comment tokens."""
    literal = json.dumps(value, ensure_ascii=False).replace("${", r"\${")
    source = (
        f"{{{layout}# {comment}\n{layout}value{layout}={layout}{literal};{layout}}}"
    )
    expected = f"{{ # {comment}\nvalue = {literal}; }}"
    compacted = compact(source)
    if compacted != expected or compact(compacted) != compacted:
        msg = "layout compaction changed significant text or failed to converge"
        raise AssertionError(msg)
    parse(compacted)
