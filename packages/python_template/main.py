#!/usr/bin/env python3
"""Canonicalize labels into a stable, human-readable summary."""

from __future__ import annotations

import contextlib
import io
import re
import unittest

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


class MainTests(unittest.TestCase):
    """Unit tests for CLI behavior and deterministic examples."""

    def test_canonicalize_label_examples(self) -> None:
        """Equivalent spellings should collapse to the same label."""
        examples = [
            "Hello, World!",
            "hello_world",
            "HELLO   WORLD",
        ]
        canonical = [canonicalize_label(example) for example in examples]
        if canonical != ["hello-world", "hello-world", "hello-world"]:
            msg = "equivalent labels should canonicalize to 'hello-world'"
            raise AssertionError(msg)

    def test_render_message_uses_canonical_unique_labels(self) -> None:
        """The default message should summarize unique canonical labels."""
        message = render_message()
        if message != "hello-world, python-template":
            msg = "render_message() should summarize canonical default labels"
            raise AssertionError(msg)

    def test_render_message_reports_empty_result_when_no_labels_survive(self) -> None:
        """Empty canonical labels should produce the fallback message."""
        message = render_message(["...", "   ", "---"])
        if message != "No canonical labels":
            msg = "render_message() should report when no canonical labels remain"
            raise AssertionError(msg)

    def test_unique_canonical_labels_keeps_first_surviving_occurrence(self) -> None:
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
            msg = (
                "unique_canonical_labels() should keep first surviving canonical labels"
            )
            raise AssertionError(msg)

    def test_main_prints_message(self) -> None:
        """main() should emit the canonical label summary."""
        output = io.StringIO()
        with contextlib.redirect_stdout(output):
            main()
        if output.getvalue().strip() != "hello-world, python-template":
            msg = "main() should print the canonical label summary"
            raise AssertionError(msg)


class PropertyTests(unittest.TestCase):
    """Property-based tests for canonicalization invariants."""

    @given(st.text())  # type: ignore[untyped-decorator]
    def test_canonicalization_is_idempotent(self, label: str) -> None:
        """Canonicalizing twice should not change the result."""
        canonical = canonicalize_label(label)
        if canonicalize_label(canonical) != canonical:
            msg = "canonicalize_label() must be idempotent"
            raise AssertionError(msg)

    @given(st.text())  # type: ignore[untyped-decorator]
    def test_canonicalization_uses_restricted_character_set(self, label: str) -> None:
        """Canonical labels should only contain lowercase ASCII, digits, and hyphens."""
        canonical = canonicalize_label(label)
        if canonical and re.fullmatch(r"[a-z0-9]+(?:-[a-z0-9]+)*", canonical) is None:
            msg = "canonicalize_label() produced characters outside kebab-case"
            raise AssertionError(msg)

    @given(st.lists(st.text(), max_size=25))  # type: ignore[untyped-decorator]
    def test_unique_canonical_labels_is_idempotent(self, labels: list[str]) -> None:
        """Deduplicating canonical labels twice should be stable."""
        canonical_labels = unique_canonical_labels(labels)
        if unique_canonical_labels(canonical_labels) != canonical_labels:
            msg = "unique_canonical_labels() must be idempotent"
            raise AssertionError(msg)


if __name__ == "__main__":
    main()
