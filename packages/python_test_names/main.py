#!/usr/bin/env python3
# Copyright (c) 2026- Paschalis Bizopoulos
"""Print test names as sentences without importing or executing test source."""

from __future__ import annotations

import argparse
import ast
import shlex
import subprocess
import sys
from pathlib import Path
from tempfile import TemporaryDirectory


def qualified_name(node: ast.expr) -> str:
    """Read a dotted Python name without evaluating it."""
    if isinstance(node, ast.Name):
        return node.id
    if isinstance(node, ast.Attribute):
        return qualified_name(node.value) + "." + node.attr
    return ""


def unittest_classes(module: ast.Module) -> set[str]:
    """Recognize unittest subclasses regardless of their class names."""
    bases: set[str] = set()
    case_types = {"TestCase", "IsolatedAsyncioTestCase"}
    for node in module.body:
        if isinstance(node, ast.Import):
            bases.update(
                (alias.asname or alias.name) + "." + case
                for alias in node.names
                if alias.name == "unittest"
                for case in case_types
            )
        elif isinstance(node, ast.ImportFrom) and node.module == "unittest":
            bases.update(
                alias.asname or alias.name
                for alias in node.names
                if alias.name in case_types
            )
    classes = [node for node in module.body if isinstance(node, ast.ClassDef)]
    found: set[str] = set()
    while additions := {
        node.name
        for node in classes
        if node.name not in found
        and any(qualified_name(base) in bases | found for base in node.bases)
    }:
        found.update(additions)
    return found


def read_test_names(path: Path, *, source_order: bool = False) -> list[str]:
    """Read top-level test functions and methods in recognized test classes."""
    if path.is_symlink():
        message = f"linked test file: {path}"
        raise ValueError(message)
    names = source_test_names(path.read_bytes(), str(path))
    return names if source_order else sorted(names)


def source_test_names(source: bytes, filename: str) -> list[str]:
    """Convert Python source into sentences in definition order without executing it."""
    module = ast.parse(source, filename=filename)
    case_classes = unittest_classes(module)
    definitions = []
    for node in module.body:
        if isinstance(node, ast.ClassDef) and (
            node.name.startswith("Test") or node.name in case_classes
        ):
            definitions.extend(node.body)
        else:
            definitions.append(node)
    return [
        node.name.replace("_", " ")
        for node in definitions
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef))
        and node.name.startswith("test_")
    ]


def print_package(package: Path) -> None:
    """Validate a canonical Python package and print its test sentences."""
    if (
        package.parent.name != "packages"
        or not (package.parent.parent / "flake.nix").is_file()
        or not package.name.isidentifier()
        or not all(
            (package / name).is_file()
            for name in ("default.nix", "main.py", "test_main.py")
        )
    ):
        message = (
            "expected a canonical packages/NAME with default.nix, main.py "
            "and test_main.py inside a flake"
        )
        raise ValueError(message)
    for name in read_test_names(package / "test_main.py"):
        sys.stdout.write(name + "\n")


def print_repository(root: Path) -> bool:
    """List packages sequentially and continue after individual parse failures."""
    directory = root / "packages"
    packages = (
        sorted(
            path
            for path in directory.iterdir()
            if path.is_dir() and not path.is_symlink() and (path / "main.py").is_file()
        )
        if directory.is_dir()
        else []
    )
    if not packages:
        message = f"no Python packages found under {directory}"
        raise ValueError(message)
    success = True
    for package in packages:
        if not (package / "test_main.py").exists():
            sys.stderr.write(f"Skipping {package.name}: no test_main.py\n")
            continue
        sys.stdout.write(f"packages/{package.name}:\n")
        try:
            print_package(package)
        except (OSError, SyntaxError, UnicodeError, ValueError) as error:
            success = False
            sys.stderr.write(f"python_test_names: {package.name}: {error}\n")
    return success


def git_output(arguments: list[str], *, data: bytes | None = None) -> bytes:
    """Read Git output while preserving its diagnostics and failures."""
    return subprocess.run(  # noqa: S603
        ["git", *arguments],  # noqa: S607
        input=data,
        stdout=subprocess.PIPE,
        check=True,
    ).stdout


def git_arguments(arguments: list[str]) -> tuple[list[str], list[str]]:
    """Separate Git options/revisions from explicit path filters."""
    separator = arguments.index("--") if "--" in arguments else len(arguments)
    options = arguments[:separator]
    paths = arguments[separator + 1 :]
    for option in options:
        if option in {
            "--no-index",
            "--no-textconv",
            "--ext-diff",
            "--check",
        } or option.startswith(
            ("--output", "--textconv=", "-L"),
        ):
            message = f"unsupported test-name diff option: {option}"
            raise ValueError(message)
    return options, paths


def check_diff_attributes(configuration: list[str], paths: bytes) -> None:
    """Refuse attribute overrides that would expose unconverted source."""
    if not paths:
        return
    attributes = git_output(
        [*configuration, "check-attr", "-z", "--stdin", "diff"],
        data=paths,
    ).split(b"\0")
    for index in range(0, len(attributes) - 1, 3):
        path, _, driver = attributes[index : index + 3]
        if driver != b"python-test-names":
            message = f"conflicting diff attribute for {path.decode(errors='replace')}"
            raise ValueError(message)


def diff_paths(raw: bytes) -> bytes:
    """Collect raw diff paths and reject modes that bypass Git's textconv."""
    paths = []
    for field in raw.split(b"\0"):
        if not field:
            continue
        header = field.lstrip(b"\n")
        if header.startswith(b":"):
            parents = len(header) - len(header.lstrip(b":"))
            modes = header.lstrip(b":").split()[: parents + 1]
            if any(mode not in {b"000000", b"100644", b"100755"} for mode in modes):
                message = (
                    "test-name diffs require regular files, not symlinks or submodules"
                )
                raise ValueError(message)
        else:
            paths.append(field)
    return b"\0".join(paths) + (b"\0" if paths else b"")


def print_git(command: str, arguments: list[str]) -> int:
    """Let Git compare test sentences using an invocation-local textconv driver."""
    options, paths = git_arguments(arguments)
    if command == "show":
        revisions = git_output(["rev-parse", "--revs-only", "--no-flags", *options])
        for revision in revisions.decode().splitlines():
            git_output(["rev-parse", "--verify", revision.lstrip("^") + "^{commit}"])
    root = git_output(["rev-parse", "--show-toplevel"]).decode().rstrip("\n")
    converter = shlex.join([sys.executable, str(Path(__file__).resolve()), "_textconv"])
    with TemporaryDirectory(prefix="python-test-names-") as directory:
        attributes = Path(directory) / "attributes"
        attributes.write_text(
            "/packages/*/test_main.py diff=python-test-names python-test-names\n",
        )
        configuration = [
            "-c",
            f"core.attributesFile={attributes}",
            "-c",
            f"diff.python-test-names.textconv={converter}",
            "-c",
            "diff.python-test-names.cachetextconv=false",
        ]
        filters = [*paths, ":(top,exclude,attr:!python-test-names)**"]
        discovery = git_output(
            [
                *configuration,
                command,
                *options,
                "--no-patch",
                "--raw",
                "-z",
                "--no-relative",
                "--no-renames",
                "--no-ext-diff",
                "--textconv",
                "--no-quiet",
                "--no-exit-code",
                *(["--format="] if command == "show" else []),
                "--",
                *filters,
            ],
        )
        check_diff_attributes(["-C", root, *configuration], diff_paths(discovery))
        return subprocess.run(  # noqa: S603
            [  # noqa: S607
                "git",
                *configuration,
                command,
                *options,
                "--no-ext-diff",
                "--textconv",
                "--",
                *filters,
            ],
            check=False,
        ).returncode


def run() -> int:
    """Dispatch Git views separately from the original listing interface."""
    if len(sys.argv) > 1 and sys.argv[1] in {"diff", "show"}:
        return print_git(sys.argv[1], sys.argv[2:])
    if sys.argv[1:2] == ["_textconv"]:
        converter_parser = argparse.ArgumentParser(prog="python_test_names _textconv")
        converter_parser.add_argument("file", type=Path)
        for name in read_test_names(
            converter_parser.parse_args(sys.argv[2:]).file,
            source_order=True,
        ):
            sys.stdout.write(name + "\n")
        return 0
    parser = argparse.ArgumentParser(
        description=__doc__,
        epilog=(
            "Repository targets list Python packages sequentially and skip packages "
            "without test_main.py. Test source is parsed, never executed. "
            "Git views: diff [Git options/revisions] [-- paths...] or "
            "show [Git options/revisions] [-- paths...]. Examples: diff; "
            "diff --staged; diff HEAD; diff HEAD~1 HEAD; show HEAD. "
            "Only packages/*/test_main.py sentences are compared. Git supplies "
            "formatting, commit metadata and exit codes. Body-only edits have "
            "no sentence hunks. These review diffs cannot be applied as source "
            "patches. Show requires commits; --no-index, --no-textconv, "
            "--ext-diff, --check, --output and -L are unsupported."
        ),
    )
    parser.add_argument(
        "target",
        type=Path,
        nargs="?",
        default=Path(),
        help=(
            "canonical packages/NAME directory or flake repository root "
            "(default: current directory)"
        ),
    )
    args = parser.parse_args()
    target = args.target.resolve()
    if (target / "flake.nix").is_file():
        return 0 if print_repository(target) else 1
    print_package(target)
    return 0


def main() -> None:
    """Report errors consistently for listing, conversion and Git commands."""
    try:
        status = run()
    except subprocess.CalledProcessError as error:
        sys.exit(error.returncode)
    except (OSError, SyntaxError, UnicodeError, ValueError) as error:
        sys.stderr.write(f"python_test_names: {error}\n")
        sys.exit(1)
    except KeyboardInterrupt:
        sys.stderr.write("python_test_names: interrupted\n")
        sys.exit(130)
    sys.exit(status)


if __name__ == "__main__":
    main()
