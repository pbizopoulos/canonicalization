#!/usr/bin/env python3
"""Canonicalize labels into a stable, human-readable summary."""

from __future__ import annotations

import contextlib
import io
import os
import re
import unittest
from unittest import mock

from hypothesis import given
from hypothesis import strategies as st

MODULE_NAME = __name__
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

    def test_main_prints_message_when_not_debug(self) -> None:
        """main() should emit the summary when DEBUG is not enabled."""
        output = io.StringIO()
        with (
            mock.patch.dict(os.environ, {"DEBUG": "0"}),
            contextlib.redirect_stdout(output),
        ):
            main()
        if output.getvalue().strip() != "hello-world, python-template":
            msg = "main() should print the canonical label summary when DEBUG != 1"
            raise AssertionError(msg)

    def test_main_calls_run_tests_when_debug(self) -> None:
        """main() should execute run_tests() when DEBUG is enabled."""
        with (
            mock.patch.dict(os.environ, {"DEBUG": "1"}),
            mock.patch(f"{MODULE_NAME}.run_tests") as run_tests_mock,
        ):
            main()
        if run_tests_mock.call_count != 1:
            msg = "main() should call run_tests() exactly once when DEBUG=1"
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

    @given(st.lists(st.text(), max_size=25))  # type: ignore[untyped-decorator]
    def test_unique_canonical_labels_preserves_first_occurrence_order(
        self,
        labels: list[str],
    ) -> None:
        """Deduplication should preserve first-occurrence ordering."""
        canonical_labels = unique_canonical_labels(labels)
        expected: list[str] = []
        seen: set[str] = set()
        for label in labels:
            canonical = canonicalize_label(label)
            if canonical and canonical not in seen:
                seen.add(canonical)
                expected.append(canonical)
        if canonical_labels != expected:
            msg = "unique_canonical_labels() changed first-occurrence ordering"
            raise AssertionError(msg)


def run_tests() -> None:
    """Run this module's unittest suite, including property tests."""
    suite = unittest.TestSuite(
        [
            unittest.defaultTestLoader.loadTestsFromTestCase(MainTests),
            unittest.defaultTestLoader.loadTestsFromTestCase(PropertyTests),
        ],
    )
    result = unittest.TextTestRunner(verbosity=2).run(suite)
    if not result.wasSuccessful():
        raise SystemExit(1)


def main() -> None:
    """Run main."""
    if os.getenv("DEBUG") == "1":
        run_tests()
    else:
        print(render_message())  # noqa: T201


if __name__ == "__main__":
    main()
