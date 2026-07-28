#!/usr/bin/env python3
"""Generate Python-produced artifacts for a LaTeX build."""

from __future__ import annotations

import math
import os
import subprocess
import tempfile
from dataclasses import dataclass
from pathlib import Path

import matplotlib as mpl

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
    create_workspace_artifacts(Path.cwd() / "tmp")


def test_samples_rejects_invalid_values() -> None:
    """Rejects empty and non-finite sample sequences."""
    for values in ((), (math.nan,), (math.inf,)):
        try:
            Samples(values)
        except ValueError:
            continue
        msg = "invalid samples must be rejected"
        raise AssertionError(msg)


def test_latex_escape_handles_special_characters() -> None:
    """Escapes LaTeX-special characters in generated text."""
    escaped = latex_escape(r"value_#1 & 50%")
    if escaped != r"value\_\#1 \& 50\%":
        msg = "LaTeX-special characters should be escaped"
        raise AssertionError(msg)


def test_main_creates_latex_workspace_artifacts() -> None:
    """Creates the LaTeX workspace artifacts from the executable."""
    with tempfile.TemporaryDirectory() as temporary_directory:
        working_directory = Path(temporary_directory)
        completed = subprocess.run(  # noqa: S603
            [os.environ["PACKAGE_E2E_EXECUTABLE"]],
            cwd=working_directory,
            check=False,
            capture_output=True,
            text=True,
        )
        if completed.returncode != 0 or completed.stdout or completed.stderr:
            msg = "the executable should succeed without console output"
            raise AssertionError(msg)
        workspace = working_directory / "tmp"
        figure_path = workspace / "figure.png"
        table_path = workspace / "table.tex"
        if (
            not figure_path.is_file()
            or figure_path.stat().st_size == 0
            or not table_path.is_file()
        ):
            msg = "workspace artifacts should be complete"
            raise AssertionError(msg)
        table = table_path.read_text(encoding="utf-8")
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
            msg = "generated table should contain the expected summary"
            raise AssertionError(msg)


if __name__ == "__main__":
    main()
