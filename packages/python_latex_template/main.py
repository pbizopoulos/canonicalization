#!/usr/bin/env python3
"""Generate Python-produced artifacts for a LaTeX build."""

from __future__ import annotations

import os
import tempfile
import unittest
from pathlib import Path
from unittest import mock

import matplotlib as mpl
from hypothesis import given
from hypothesis import strategies as st

mpl.use("Agg")
import matplotlib.pyplot as plt
import pandas as pd


def create_figure(path: Path) -> None:
    """Create a small deterministic figure for the LaTeX document."""
    figure, axis = plt.subplots(figsize=(5, 3))
    x_values = [1, 2, 3, 4]
    y_values = [1, 4, 9, 16]
    axis.plot(x_values, y_values, color="#1f77b4", linewidth=2.5, marker="o")
    axis.set_xlabel("Input")
    axis.set_ylabel("Squared output")
    if os.getenv("DEBUG"):
        axis.set_title("Python-generated figure (in DEBUG mode)")
    else:
        axis.set_title("Python-generated figure")
    axis.grid(alpha=0.3)
    figure.tight_layout()
    figure.savefig(path, dpi=200)
    plt.close(figure)


def create_table(path: Path) -> None:
    """Create a LaTeX table with pandas."""
    frame = pd.DataFrame(
        {
            "Metric": ["mean", "median", "max"],
            "Value": [7.50, 6.50, 16.00],
        },
    )
    lines = [
        "\\begin{tabular}{lr}",
        "\\toprule",
        "Metric & Value \\\\",
        "\\midrule",
    ]
    lines.extend(
        f"{row.Metric} & {row.Value:.2f} \\\\" for row in frame.itertuples(index=False)
    )
    lines.extend(["\\bottomrule", "\\end{tabular}", ""])
    latex = "\n".join(lines)
    path.write_text(latex, encoding="utf-8")


def create_workspace_artifacts(workspace: Path) -> None:
    """Generate all artifacts required by the LaTeX document."""
    workspace.mkdir(parents=True, exist_ok=True)
    create_figure(workspace / "figure.png")
    create_table(workspace / "table.tex")


class ArtifactTests(unittest.TestCase):
    """Unit tests for deterministic artifact generation."""

    def test_create_table_contains_expected_rows(self) -> None:
        """Table output should contain known metrics and values."""
        with tempfile.TemporaryDirectory() as tmpdir:
            table_path = Path(tmpdir) / "table.tex"
            create_table(table_path)
            table = table_path.read_text(encoding="utf-8")
            if "\\begin{tabular}{lr}" not in table:
                msg = "missing LaTeX tabular header"
                raise AssertionError(msg)
            if "mean & 7.50 \\\\" not in table:
                msg = "missing mean row"
                raise AssertionError(msg)
            if "median & 6.50 \\\\" not in table:
                msg = "missing median row"
                raise AssertionError(msg)
            if "max & 16.00 \\\\" not in table:
                msg = "missing max row"
                raise AssertionError(msg)
            if "\\end{tabular}" not in table:
                msg = "missing LaTeX tabular footer"
                raise AssertionError(msg)
            expected_lines = [
                "\\begin{tabular}{lr}",
                "\\toprule",
                "Metric & Value \\\\",
                "\\midrule",
                "mean & 7.50 \\\\",
                "median & 6.50 \\\\",
                "max & 16.00 \\\\",
                "\\bottomrule",
                "\\end{tabular}",
            ]
            actual_lines = [line for line in table.splitlines() if line]
            if actual_lines != expected_lines:
                msg = "table.tex lines differ from expected deterministic layout"
                raise AssertionError(msg)

    def test_create_figure_writes_non_empty_png(self) -> None:
        """Figure generation should create a real PNG file."""
        with tempfile.TemporaryDirectory() as tmpdir:
            figure_path = Path(tmpdir) / "figure.png"
            create_figure(figure_path)
            if not figure_path.exists():
                msg = "figure.png was not created"
                raise AssertionError(msg)
            if figure_path.stat().st_size <= 0:
                msg = "figure.png is empty"
                raise AssertionError(msg)

    def test_create_workspace_artifacts_writes_both_outputs(self) -> None:
        """Workspace generation should always produce figure and table files."""
        with tempfile.TemporaryDirectory() as tmpdir:
            workspace = Path(tmpdir) / "tmp"
            create_workspace_artifacts(workspace)
            if not (workspace / "figure.png").exists():
                msg = "workspace is missing figure.png"
                raise AssertionError(msg)
            if not (workspace / "table.tex").exists():
                msg = "workspace is missing table.tex"
                raise AssertionError(msg)

    def test_main_debug_runs_tests_and_generates_workspace(self) -> None:
        """DEBUG mode should run tests and still generate artifacts."""
        with tempfile.TemporaryDirectory() as tmpdir:
            fake_cwd = Path(tmpdir)
            with (
                mock.patch.dict(os.environ, {"DEBUG": "1"}),
                mock.patch("__main__.run_tests") as run_tests_mock,
                mock.patch("__main__.create_workspace_artifacts") as create_mock,
                mock.patch("pathlib.Path.cwd", return_value=fake_cwd),
            ):
                main()
            if run_tests_mock.call_count != 1:
                msg = "main() should call run_tests() exactly once when DEBUG=1"
                raise AssertionError(msg)
            if create_mock.call_count != 1:
                msg = "main() should always call create_workspace_artifacts()"
                raise AssertionError(msg)
            called_workspace = create_mock.call_args.args[0]
            if called_workspace != fake_cwd.resolve() / "tmp":
                msg = "main() should generate artifacts in <cwd>/tmp"
                raise AssertionError(msg)


class PropertyTests(unittest.TestCase):
    """Property-based tests for table generation invariants."""

    @given(  # type: ignore[untyped-decorator]
        st.text(
            alphabet=st.characters(min_codepoint=97, max_codepoint=122),
            min_size=1,
            max_size=32,
        ),
    )
    def test_create_table_has_required_structure(self, generated_text: str) -> None:
        """create_table() should always emit required deterministic markers."""
        _ = generated_text
        with tempfile.TemporaryDirectory() as tmpdir:
            table_path = Path(tmpdir) / "table.tex"
            create_table(table_path)
            table = table_path.read_text(encoding="utf-8")
            if "\\begin{tabular}{lr}" not in table:
                msg = "missing LaTeX tabular header"
                raise AssertionError(msg)
            if "\\end{tabular}" not in table:
                msg = "missing LaTeX tabular footer"
                raise AssertionError(msg)
            if "mean & 7.50 \\\\" not in table:
                msg = "missing deterministic mean row"
                raise AssertionError(msg)


def run_tests() -> None:
    """Run this module's unittest suite."""
    suite = unittest.defaultTestLoader.loadTestsFromTestCase(ArtifactTests)
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
