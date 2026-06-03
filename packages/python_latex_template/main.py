#!/usr/bin/env python3
"""Generate Python-produced artifacts for a LaTeX build."""

from __future__ import annotations

import math
import os
import re
import tempfile
import unittest
from pathlib import Path
from unittest import mock

import matplotlib as mpl
from hypothesis import given, settings
from hypothesis import strategies as st

mpl.use("Agg")
import matplotlib.pyplot as plt
import pandas as pd

MODULE_NAME = __name__
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
    if os.getenv("DEBUG") == "1":
        axis.set_title("Python-generated figure (in DEBUG mode)")
    else:
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


class ArtifactTests(unittest.TestCase):
    """Unit tests for deterministic artifact generation."""

    def test_summarize_samples_contains_expected_metrics(self) -> None:
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
            msg = "summarize_samples() changed expected default statistics"
            raise AssertionError(msg)

    def test_create_table_contains_expected_metrics(self) -> None:
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
                    msg = f"table.tex is missing expected content: {fragment!r}"
                    raise AssertionError(msg)

    def test_create_figure_writes_non_empty_png(self) -> None:
        """Figure generation should create a real PNG file."""
        with tempfile.TemporaryDirectory() as tmpdir:
            figure_path = Path(tmpdir) / "figure.png"
            create_figure(figure_path, DEFAULT_SAMPLES)
            if not figure_path.exists():
                msg = "figure.png was not created"
                raise AssertionError(msg)
            if figure_path.stat().st_size <= 0:
                msg = "figure.png is empty"
                raise AssertionError(msg)

    def test_main_generates_workspace_artifacts_in_current_directory(self) -> None:
        """main() should write artifacts into <cwd>/tmp."""
        with tempfile.TemporaryDirectory() as tmpdir:
            fake_cwd = Path(tmpdir)
            with (
                mock.patch.dict(os.environ, {"DEBUG": "0"}),
                mock.patch("pathlib.Path.cwd", return_value=fake_cwd),
            ):
                main()
            workspace = fake_cwd.resolve() / "tmp"
            if not (workspace / "figure.png").exists():
                msg = "main() should generate figure.png in <cwd>/tmp"
                raise AssertionError(msg)
            if not (workspace / "table.tex").exists():
                msg = "main() should generate table.tex in <cwd>/tmp"
                raise AssertionError(msg)

    def test_main_debug_still_writes_artifacts_in_current_directory(self) -> None:
        """DEBUG mode should still generate artifacts into <cwd>/tmp."""
        with tempfile.TemporaryDirectory() as tmpdir:
            fake_cwd = Path(tmpdir)
            with (
                mock.patch.dict(os.environ, {"DEBUG": "1"}),
                mock.patch(f"{MODULE_NAME}.run_tests"),
                mock.patch("pathlib.Path.cwd", return_value=fake_cwd),
            ):
                main()
            workspace = fake_cwd.resolve() / "tmp"
            if not (workspace / "figure.png").exists():
                msg = "main() should generate figure.png in <cwd>/tmp when DEBUG=1"
                raise AssertionError(msg)
            if not (workspace / "table.tex").exists():
                msg = "main() should generate table.tex in <cwd>/tmp when DEBUG=1"
                raise AssertionError(msg)


class PropertyTests(unittest.TestCase):
    """Property-based tests for summary and rendering invariants."""

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
    @settings(deadline=None)  # type: ignore[untyped-decorator]
    def test_summary_is_permutation_invariant(self, samples: list[float]) -> None:
        """Aggregate statistics should not depend on sample ordering."""
        forward = summarize_samples(samples)
        backward = summarize_samples(list(reversed(samples)))
        if len(forward) != len(backward):
            msg = "summary length changed across permutations"
            raise AssertionError(msg)
        for (left_metric, left_value), (right_metric, right_value) in zip(
            forward,
            backward,
            strict=True,
        ):
            if left_metric != right_metric:
                msg = "summary metric ordering changed across permutations"
                raise AssertionError(msg)
            if not math.isclose(left_value, right_value, rel_tol=1e-9, abs_tol=1e-9):
                msg = f"summary metric {left_metric!r} changed across permutations"
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
    @settings(deadline=None)  # type: ignore[untyped-decorator]
    def test_rendered_table_row_count_tracks_summary_metrics(
        self,
        samples: list[float],
    ) -> None:
        """Each summary metric should produce exactly one LaTeX body row."""
        rendered = render_table(samples)
        row_lines = [
            line
            for line in rendered.splitlines()
            if line.endswith("\\\\") and not line.startswith("Metric &")
        ]
        if len(row_lines) != len(summarize_samples(samples)):
            msg = "render_table() row count does not match summary metric count"
            raise AssertionError(msg)

    @given(st.text())  # type: ignore[untyped-decorator]
    def test_latex_escape_prefixes_special_characters(self, text: str) -> None:
        """Escaped output should prefix special characters that need escaping."""
        escaped = latex_escape(text)
        for character in "&%$#_":
            if re.search(rf"(?<!\\){re.escape(character)}", escaped) is not None:
                msg = f"latex_escape() left unescaped {character!r} in output"
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
    def test_workspace_artifacts_created_for_nested_paths(
        self,
        path_segments: list[str],
    ) -> None:
        """Artifact generation should work for nested workspace paths."""
        with tempfile.TemporaryDirectory() as tmpdir:
            workspace = Path(tmpdir).joinpath(*path_segments)
            create_workspace_artifacts(workspace)
            figure_path = workspace / "figure.png"
            table_path = workspace / "table.tex"
            if not figure_path.exists() or figure_path.stat().st_size <= 0:
                msg = "create_workspace_artifacts() must produce a non-empty figure.png"
                raise AssertionError(msg)
            if not table_path.exists():
                msg = "create_workspace_artifacts() must produce table.tex"
                raise AssertionError(msg)


def run_tests() -> None:
    """Run this module's unittest suite, including property tests."""
    suite = unittest.TestSuite(
        [
            unittest.defaultTestLoader.loadTestsFromTestCase(ArtifactTests),
            unittest.defaultTestLoader.loadTestsFromTestCase(PropertyTests),
        ],
    )
    result = unittest.TextTestRunner(verbosity=2).run(suite)
    if not result.wasSuccessful():
        raise SystemExit(1)


def main() -> None:
    """Generate the build workspace artifacts for LaTeX compilation."""
    if os.getenv("DEBUG") == "1":
        run_tests()
    workspace = Path.cwd().resolve() / "tmp"
    create_workspace_artifacts(workspace)


if __name__ == "__main__":
    main()
