#!/usr/bin/env python3
"""Generate Python-produced artifacts for a LaTeX build."""

from __future__ import annotations

import math
import re
import tempfile
from pathlib import Path
from unittest import mock

import matplotlib as mpl
from hypothesis import given, settings
from hypothesis import strategies as st

mpl.use("Agg")
import matplotlib.pyplot as plt
import pandas as pd

DEFAULT_SAMPLES = [2.0, 3.5, 5.0, 9.5, 12.0]
LATEX_ESCAPES = {
    "\\": r"\textbackslash{}",
    "&": r"\&",
    "%": r"\%",
    "$": r"\$",
    "#": r"\#",
    "_": r"\_",
    "{": r"\{",
    "}": r"\}",
}


def latex_escape(text: str) -> str:
    """Escape a narrow, deterministic subset of LaTeX-special characters."""
    return "".join(LATEX_ESCAPES.get(character, character) for character in text)


def summarize_samples(samples: list[float]) -> list[tuple[str, float]]:
    """Return stable summary statistics for a non-empty sample list."""
    if not samples:
        msg = "samples must not be empty"
        raise ValueError(msg)
    total = float(sum(samples))
    count = float(len(samples))
    mean = total / count
    return [
        ("count", count),
        ("total", total),
        ("mean", mean),
        ("min", float(min(samples))),
        ("max", float(max(samples))),
    ]


def build_table_frame(samples: list[float]) -> pd.DataFrame:
    """Represent summary statistics as a DataFrame for LaTeX emission."""
    rows = summarize_samples(samples)
    return pd.DataFrame(rows, columns=["Metric", "Value"])


def render_table(samples: list[float]) -> str:
    """Render a LaTeX tabular with escaped metric names and fixed decimals."""
    frame = build_table_frame(samples)
    lines = [
        "\\begin{tabular}{lr}",
        "\\toprule",
        "Metric & Value \\\\",
        "\\midrule",
    ]
    lines.extend(
        f"{latex_escape(str(row.Metric))} & {row.Value:.2f} \\\\"
        for row in frame.itertuples(index=False)
    )
    lines.extend(["\\bottomrule", "\\end{tabular}", ""])
    return "\n".join(lines)


def create_figure(path: Path, samples: list[float]) -> None:
    """Create a deterministic figure for the LaTeX document."""
    figure, axis = plt.subplots(figsize=(5, 3))
    x_values = list(range(1, len(samples) + 1))
    axis.plot(x_values, samples, color="#1f77b4", linewidth=2.5, marker="o")
    axis.set_xlabel("Sample index")
    axis.set_ylabel("Value")
    axis.set_title("Python-generated figure")
    axis.grid(alpha=0.3)
    figure.tight_layout()
    figure.savefig(path, dpi=200)
    plt.close(figure)


def create_table(path: Path, samples: list[float]) -> None:
    """Create a LaTeX table with pandas-backed summary statistics."""
    path.write_text(render_table(samples), encoding="utf-8")


def create_workspace_artifacts(
    workspace: Path,
    samples: list[float] | None = None,
) -> None:
    """Generate all artifacts required by the LaTeX document."""
    resolved_samples = DEFAULT_SAMPLES if samples is None else samples
    workspace.mkdir(parents=True, exist_ok=True)
    create_figure(workspace / "figure.png", resolved_samples)
    create_table(workspace / "table.tex", resolved_samples)


def main() -> None:
    """Generate the build workspace artifacts for LaTeX compilation."""
    workspace = Path.cwd().resolve() / "tmp"
    create_workspace_artifacts(workspace)


def test_summarize_samples_contains_expected_metrics() -> None:
    """Summary statistics should remain stable for the default dataset."""
    summary = dict(summarize_samples(DEFAULT_SAMPLES))
    expected = {
        "count": 5.0,
        "total": 32.0,
        "mean": 6.4,
        "min": 2.0,
        "max": 12.0,
    }
    if summary != expected:
        msg = "summary statistics should remain stable"
        raise AssertionError(msg)


def test_create_table_contains_expected_metrics() -> None:
    """Table output should expose the expected summary metrics and values."""
    with tempfile.TemporaryDirectory() as tmpdir:
        table_path = Path(tmpdir) / "table.tex"
        create_table(table_path, DEFAULT_SAMPLES)
        table = table_path.read_text(encoding="utf-8")
        expected_fragments = [
            "\\begin{tabular}{lr}",
            "Metric & Value \\\\",
            "count & 5.00 \\\\",
            "total & 32.00 \\\\",
            "mean & 6.40 \\\\",
            "min & 2.00 \\\\",
            "max & 12.00 \\\\",
            "\\end{tabular}",
        ]
        for fragment in expected_fragments:
            if fragment not in table:
                msg = "table output should contain expected fragments"
                raise AssertionError(msg)


def test_create_figure_writes_non_empty_png() -> None:
    """Figure generation should create a real PNG file."""
    with tempfile.TemporaryDirectory() as tmpdir:
        figure_path = Path(tmpdir) / "figure.png"
        create_figure(figure_path, DEFAULT_SAMPLES)
        if not figure_path.exists():
            msg = "figure generation should create a PNG"
            raise AssertionError(msg)
        if figure_path.stat().st_size <= 0:
            msg = "PNG should not be empty"
            raise AssertionError(msg)


def test_main_generates_workspace_artifacts_in_current_directory() -> None:
    """main() should write artifacts into <cwd>/tmp."""
    with tempfile.TemporaryDirectory() as tmpdir:
        fake_cwd = Path(tmpdir)
        with mock.patch("pathlib.Path.cwd", return_value=fake_cwd):
            main()
        workspace = fake_cwd.resolve() / "tmp"
        if not (workspace / "figure.png").exists():
            msg = "figure.png should exist"
            raise AssertionError(msg)
        if not (workspace / "table.tex").exists():
            msg = "table.tex should exist"
            raise AssertionError(msg)


@given(
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
@settings(deadline=None)
def test_property_summary_is_permutation_invariant(samples: list[float]) -> None:
    """Aggregate statistics should not depend on sample ordering."""
    forward = summarize_samples(samples)
    backward = summarize_samples(list(reversed(samples)))
    if len(forward) != len(backward):
        msg = "summary lengths should match"
        raise AssertionError(msg)
    for (left_metric, left_value), (right_metric, right_value) in zip(
        forward,
        backward,
        strict=True,
    ):
        if left_metric != right_metric:
            msg = "metrics should match"
            raise AssertionError(msg)
        if not math.isclose(left_value, right_value, rel_tol=1e-9, abs_tol=1e-9):
            msg = "summary values should match within tolerance"
            raise AssertionError(msg)


@given(st.text())
def test_property_latex_escape_prefixes_special_characters(text: str) -> None:
    """Escaped output should prefix special characters that need escaping."""
    escaped = latex_escape(text)
    for character in "&%$#_":
        if re.search(rf"(?<!\\){re.escape(character)}", escaped) is not None:
            msg = "special characters should be escaped"
            raise AssertionError(msg)


@given(
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
@settings(deadline=None, max_examples=25)
def test_property_workspace_artifacts_created_for_nested_paths(
    path_segments: list[str],
) -> None:
    """Artifact generation should work for nested workspace paths."""
    with tempfile.TemporaryDirectory() as tmpdir:
        workspace = Path(tmpdir).joinpath(*path_segments)
        create_workspace_artifacts(workspace)
        figure_path = workspace / "figure.png"
        table_path = workspace / "table.tex"
        if not figure_path.exists():
            msg = "figure.png should exist"
            raise AssertionError(msg)
        if figure_path.stat().st_size <= 0:
            msg = "PNG should not be empty"
            raise AssertionError(msg)
        if not table_path.exists():
            msg = "table.tex should exist"
            raise AssertionError(msg)


if __name__ == "__main__":
    main()
