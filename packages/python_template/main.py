#!/usr/bin/env python3
"""Canonicalize labels into a stable, human-readable summary."""

from __future__ import annotations

import contextlib
import io
import re

from hypothesis import given
from hypothesis import strategies as st

DEFAULT_LABELS = [
    "Hello, World!",
    "hello_world",
    "HELLO   WORLD",
    "Python-Template",
    "python template",
]


def canonicalize_label(label: str) -> str:
    """Collapse arbitrary label spellings to lowercase kebab-case."""
    normalized = re.sub(r"[^0-9A-Za-z]+", "-", label.casefold()).strip("-")
    return re.sub(r"-{2,}", "-", normalized)


def unique_canonical_labels(labels: list[str]) -> list[str]:
    """Keep first occurrences after canonicalization."""
    seen: set[str] = set()
    canonical_labels: list[str] = []
    for label in labels:
        canonical = canonicalize_label(label)
        if canonical and canonical not in seen:
            seen.add(canonical)
            canonical_labels.append(canonical)
    return canonical_labels


def render_message(labels: list[str] | None = None) -> str:
    """Return a deterministic summary of canonical labels."""
    source_labels = DEFAULT_LABELS if labels is None else labels
    canonical_labels = unique_canonical_labels(source_labels)
    if not canonical_labels:
        return "No canonical labels"
    return ", ".join(canonical_labels)


def main() -> None:
    """Run main."""
    print(render_message())  # noqa: T201


def test_canonicalize_label_examples() -> None:
    """Equivalent spellings should collapse to the same label."""
    examples = [
        "Hello, World!",
        "hello_world",
        "HELLO   WORLD",
    ]
    canonical = [canonicalize_label(example) for example in examples]
    if canonical != ["hello-world", "hello-world", "hello-world"]:
        msg = "canonical spellings should match"
        raise AssertionError(msg)


def test_render_message_uses_canonical_unique_labels() -> None:
    """The default message should summarize unique canonical labels."""
    if render_message() != "hello-world, python-template":
        msg = "default message should summarize unique canonical labels"
        raise AssertionError(msg)


def test_render_message_reports_empty_result_when_no_labels_survive() -> None:
    """Empty canonical labels should produce the fallback message."""
    if render_message(["...", "   ", "---"]) != "No canonical labels":
        msg = "empty canonical labels should produce the fallback message"
        raise AssertionError(msg)


def test_unique_canonical_labels_keeps_first_surviving_occurrence() -> None:
    """Deduplication should keep first surviving canonical labels in order."""
    labels = [
        "Hello, World!",
        "python template",
        "---",
        "hello_world",
        "Python-Template",
        "HELLO   WORLD",
    ]
    if unique_canonical_labels(labels) != ["hello-world", "python-template"]:
        msg = "deduplication should keep first surviving canonical labels in order"
        raise AssertionError(
            msg,
        )


def test_main_prints_message() -> None:
    """main() should emit the canonical label summary."""
    output = io.StringIO()
    with contextlib.redirect_stdout(output):
        main()
    if output.getvalue().strip() != "hello-world, python-template":
        msg = "main() should emit the canonical label summary"
        raise AssertionError(msg)


@given(st.text())  # type: ignore[untyped-decorator]
def test_property_canonicalization_is_idempotent(label: str) -> None:
    """Canonicalizing twice should not change the result."""
    canonical = canonicalize_label(label)
    if canonicalize_label(canonical) != canonical:
        msg = "canonicalization should be idempotent"
        raise AssertionError(msg)


@given(st.text())  # type: ignore[untyped-decorator]
def test_property_canonicalization_uses_restricted_character_set(label: str) -> None:
    """Canonical labels should only contain lowercase ASCII, digits, and hyphens."""
    canonical = canonicalize_label(label)
    if canonical and re.fullmatch(r"[a-z0-9]+(?:-[a-z0-9]+)*", canonical) is None:
        msg = "canonical labels must use lowercase ASCII, digits, and hyphens"
        raise AssertionError(
            msg,
        )


@given(st.lists(st.text(), max_size=25))  # type: ignore[untyped-decorator]
def test_property_unique_canonical_labels_is_idempotent(labels: list[str]) -> None:
    """Deduplicating canonical labels twice should be stable."""
    canonical_labels = unique_canonical_labels(labels)
    if unique_canonical_labels(canonical_labels) != canonical_labels:
        msg = "deduplicating canonical labels twice should be stable"
        raise AssertionError(msg)


if __name__ == "__main__":
    main()
