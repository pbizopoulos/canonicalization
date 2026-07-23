#!/usr/bin/env python3
"""Canonicalize labels into a stable, human-readable summary."""

from __future__ import annotations

import contextlib
import io
import re
from dataclasses import dataclass
from typing import TYPE_CHECKING

from hypothesis import given
from hypothesis import strategies as st

if TYPE_CHECKING:
    from collections.abc import Iterable
CANONICAL_PARTS = re.compile(r"[a-z0-9]+")


def canonicalize_label(label: str) -> str:
    """Return a non-empty lowercase ASCII kebab-case label."""
    canonical = "-".join(CANONICAL_PARTS.findall(label.casefold()))
    if not canonical:
        msg = "label must contain an ASCII letter or digit"
        raise ValueError(msg)
    return canonical


@dataclass(frozen=True, init=False, slots=True)
class CanonicalLabels:
    """An immutable, non-empty sequence of unique canonical labels."""

    values: tuple[str, ...]

    def __init__(self, labels: Iterable[str]) -> None:
        """Canonicalize labels and preserve first-occurrence order."""
        values = tuple(dict.fromkeys(map(canonicalize_label, labels)))
        if not values:
            msg = "labels must not be empty"
            raise ValueError(msg)
        object.__setattr__(self, "values", values)

    def __str__(self) -> str:
        """Join the labels for display."""
        return ", ".join(self.values)


DEFAULT_LABELS = CanonicalLabels(("hello-world", "python-template"))


def render_message(labels: CanonicalLabels = DEFAULT_LABELS) -> str:
    """Return a deterministic canonical-label summary."""
    return str(labels)


def main() -> None:
    """Print the default canonical-label summary."""
    print(render_message())  # noqa: T201


def test_canonical_label_normalizes_equivalent_spellings() -> None:
    """Equivalent spellings should produce equal canonical labels."""
    labels = {
        canonicalize_label("Hello, World!"),
        canonicalize_label("hello_world"),
        canonicalize_label("HELLO   WORLD"),
    }
    if labels != {"hello-world"}:
        msg = "canonical spellings should match"
        raise AssertionError(msg)


def test_canonical_label_rejects_empty_results() -> None:
    """A canonical label must contain an ASCII letter or digit."""
    try:
        canonicalize_label("...")
    except ValueError:
        return
    msg = "empty canonical labels must be rejected"
    raise AssertionError(msg)


def test_canonical_labels_rejects_empty_sequences() -> None:
    """A canonical-label sequence must contain at least one label."""
    try:
        CanonicalLabels(())
    except ValueError:
        return
    msg = "empty canonical-label sequences must be rejected"
    raise AssertionError(msg)


def test_render_message_uses_unique_labels() -> None:
    """The default message should summarize unique canonical labels."""
    if render_message() != "hello-world, python-template":
        msg = "default message should summarize unique canonical labels"
        raise AssertionError(msg)


def test_canonical_labels_keeps_first_occurrence() -> None:
    """Deduplication should keep first canonical labels in order."""
    labels = [
        "Hello, World!",
        "python template",
        "hello_world",
        "Python-Template",
        "HELLO   WORLD",
    ]
    canonical = CanonicalLabels(labels).values
    if canonical != ("hello-world", "python-template"):
        msg = "deduplication should keep first canonical labels in order"
        raise AssertionError(msg)


def test_main_prints_message() -> None:
    """main() should emit the canonical-label summary."""
    output = io.StringIO()
    with contextlib.redirect_stdout(output):
        main()
    if output.getvalue() != "hello-world, python-template\n":
        msg = "main() should emit the canonical-label summary"
        raise AssertionError(msg)


@given(  # type: ignore[untyped-decorator]
    st.text().filter(
        lambda label: CANONICAL_PARTS.search(label.casefold()) is not None,
    ),
)
def test_property_canonicalization_is_idempotent(label: str) -> None:
    """Canonicalizing twice should not change the result."""
    canonical = canonicalize_label(label)
    if canonicalize_label(canonical) != canonical:
        msg = "canonicalization should be idempotent"
        raise AssertionError(msg)


@given(  # type: ignore[untyped-decorator]
    st.text().filter(
        lambda label: CANONICAL_PARTS.search(label.casefold()) is not None,
    ),
)
def test_property_canonicalization_has_canonical_form(label: str) -> None:
    """Canonical labels should contain lowercase ASCII words and hyphens."""
    canonical = canonicalize_label(label)
    if re.fullmatch(r"[a-z0-9]+(?:-[a-z0-9]+)*", canonical) is None:
        msg = "canonical labels must have canonical form"
        raise AssertionError(msg)


@given(
    st.lists(
        st.from_regex(r"[a-z0-9]+(?:-[a-z0-9]+)*", fullmatch=True),
        min_size=1,
        max_size=25,
    ),
)  # type: ignore[untyped-decorator]
def test_property_unique_canonical_labels_is_idempotent(labels: list[str]) -> None:
    """Deduplicating canonical labels twice should be stable."""
    canonical_labels = CanonicalLabels(labels)
    canonical_again = CanonicalLabels(canonical_labels.values)
    if canonical_again != canonical_labels:
        msg = "deduplicating canonical labels twice should be stable"
        raise AssertionError(msg)


if __name__ == "__main__":
    main()
