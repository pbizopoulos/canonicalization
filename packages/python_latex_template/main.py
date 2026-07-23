#!/usr/bin/env python3
"""Generate Python-produced artifacts for a LaTeX build."""

from __future__ import annotations

import math
import re
import tempfile
from dataclasses import dataclass
from pathlib import Path

import matplotlib as mpl
from hypothesis import given, settings
from hypothesis import strategies as st

mpl.use("Agg")
from typing import TYPE_CHECKING

import matplotlib.pyplot as plt

if TYPE_CHECKING:
    from collections.abc import Iterable
LATEX_ESCAPES = {
    "\\": r"\textbackslash{}",
    "&": r"\&",
    "%": r"\%",
    "$": r"\$",
    "#": r"\#",
    "_": r"\_",
    "{": r"\{",
    "}": r"\}",
    "~": r"\textasciitilde{}",
    "^": r"\textasciicircum{}",
}


@dataclass(frozen=True, init=False, slots=True)
class Samples:
    """An immutable, non-empty sequence with finite summary statistics."""

    values: tuple[float, ...]

    def __init__(self, values: Iterable[float]) -> None:
        """Freeze values after validating every summary operation."""
        normalized = tuple(values)
        if not normalized:
            msg = "samples must not be empty"
            raise ValueError(msg)
        if not all(map(math.isfinite, normalized)):
            msg = "samples must be finite"
            raise ValueError(msg)
        total = math.fsum(normalized)
        if not math.isfinite(total):
            msg = "sample total must be finite"
            raise ValueError(msg)
        object.__setattr__(self, "values", normalized)

    def summary(self) -> tuple[tuple[str, float], ...]:
        """Return summary rows in display order."""
        total = math.fsum(self.values)
        return (
            ("count", float(len(self.values))),
            ("total", total),
            ("mean", total / len(self.values)),
            ("min", min(self.values)),
            ("max", max(self.values)),
        )


DEFAULT_SAMPLES = Samples((2.0, 3.5, 5.0, 9.5, 12.0))


def latex_escape(text: str) -> str:
    """Escape LaTeX-special characters."""
    return "".join(LATEX_ESCAPES.get(character, character) for character in text)


def render_table(samples: Samples) -> str:
    """Render summary statistics as a LaTeX tabular."""
    lines = [
        "\\begin{tabular}{lr}",
        "\\toprule",
        "Metric & Value \\\\",
        "\\midrule",
        *(
            f"{latex_escape(metric)} & {value:.2f} \\\\"
            for metric, value in samples.summary()
        ),
        "\\bottomrule",
        "\\end{tabular}",
        "",
    ]
    return "\n".join(lines)


def create_figure(path: Path, samples: Samples) -> None:
    """Create a deterministic figure for the LaTeX document."""
    figure, axis = plt.subplots(figsize=(5, 3))
    axis.plot(
        range(1, len(samples.values) + 1),
        samples.values,
        color="#1f77b4",
        linewidth=2.5,
        marker="o",
    )
    axis.set_xlabel("Sample index")
    axis.set_ylabel("Value")
    axis.set_title("Python-generated figure")
    axis.grid(alpha=0.3)
    figure.tight_layout()
    figure.savefig(path, dpi=200)
    plt.close(figure)


def create_workspace_artifacts(
    workspace: Path,
    samples: Samples = DEFAULT_SAMPLES,
) -> None:
    """Generate all artifacts required by the LaTeX document."""
    workspace.mkdir(parents=True, exist_ok=True)
    create_figure(workspace / "figure.png", samples)
    (workspace / "table.tex").write_text(render_table(samples), encoding="utf-8")


def main() -> None:
    """Generate the LaTeX build workspace artifacts."""
    create_workspace_artifacts(Path.cwd().resolve() / "tmp")


def test_samples_contains_expected_summary() -> None:
    """Summary statistics should remain stable for the default dataset."""
    expected = (
        ("count", 5.0),
        ("total", 32.0),
        ("mean", 6.4),
        ("min", 2.0),
        ("max", 12.0),
    )
    if DEFAULT_SAMPLES.summary() != expected:
        msg = "summary statistics should remain stable"
        raise AssertionError(msg)


def test_samples_rejects_invalid_values() -> None:
    """Samples should reject empty and non-finite sequences."""
    for values in ((), (math.nan,), (math.inf,)):
        try:
            Samples(values)
        except ValueError:
            continue
        msg = "invalid samples must be rejected"
        raise AssertionError(msg)


def test_render_table_contains_expected_metrics() -> None:
    """Table output should expose the expected summary metrics and values."""
    table = render_table(DEFAULT_SAMPLES)
    expected_fragments = (
        "\\begin{tabular}{lr}",
        "Metric & Value \\\\",
        "count & 5.00 \\\\",
        "total & 32.00 \\\\",
        "mean & 6.40 \\\\",
        "min & 2.00 \\\\",
        "max & 12.00 \\\\",
        "\\end{tabular}",
    )
    if not all(fragment in table for fragment in expected_fragments):
        msg = "table output should contain expected fragments"
        raise AssertionError(msg)


def test_create_figure_writes_non_empty_png() -> None:
    """Figure generation should create a non-empty PNG."""
    with tempfile.TemporaryDirectory() as tmpdir:
        figure_path = Path(tmpdir) / "figure.png"
        create_figure(figure_path, DEFAULT_SAMPLES)
        if not figure_path.is_file() or figure_path.stat().st_size == 0:
            msg = "figure generation should create a non-empty PNG"
            raise AssertionError(msg)


@given(  # type: ignore[untyped-decorator]
    st.lists(
        st.floats(
            min_value=-1_000,
            max_value=1_000,
            allow_nan=False,
            allow_infinity=False,
        ),
        min_size=1,
        max_size=25,
    ),
)
def test_property_summary_is_permutation_invariant(values: list[float]) -> None:
    """Aggregate statistics should not depend on sample ordering."""
    forward = Samples(values).summary()
    backward = Samples(reversed(values)).summary()
    for left, right in zip(forward, backward, strict=True):
        if left[0] != right[0] or not math.isclose(
            left[1],
            right[1],
            rel_tol=1e-9,
            abs_tol=1e-9,
        ):
            msg = "summary should be permutation invariant"
            raise AssertionError(msg)


@given(st.text())  # type: ignore[untyped-decorator]
def test_property_latex_escape_escapes_special_characters(text: str) -> None:
    """Escaped output should contain no bare LaTeX-special characters."""
    escaped = latex_escape(text)
    for character in "&%$#_":
        if re.search(rf"(?<!\\){re.escape(character)}", escaped) is not None:
            msg = "special characters should be escaped"
            raise AssertionError(msg)


@given(  # type: ignore[untyped-decorator]
    st.lists(
        st.text(
            alphabet="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-",
            min_size=1,
            max_size=8,
        ),
        min_size=1,
        max_size=5,
    ),
)
@settings(deadline=None, max_examples=25)  # type: ignore[untyped-decorator]
def test_property_workspace_artifacts_created_for_nested_paths(
    path_segments: list[str],
) -> None:
    """Artifact generation should work for nested workspace paths."""
    with tempfile.TemporaryDirectory() as tmpdir:
        workspace = Path(tmpdir).joinpath(*path_segments)
        create_workspace_artifacts(workspace)
        figure_path = workspace / "figure.png"
        table_path = workspace / "table.tex"
        if (
            not figure_path.is_file()
            or figure_path.stat().st_size == 0
            or not table_path.is_file()
        ):
            msg = "workspace artifacts should be complete"
            raise AssertionError(msg)


if __name__ == "__main__":
    main()
