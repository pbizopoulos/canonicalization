#!/usr/bin/env python3
# Copyright (c) 2026- Paschalis Bizopoulos
"""Print test names as sentences without importing or executing test source."""

from __future__ import annotations

import argparse
import ast
import sys
from pathlib import Path


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


def read_test_names(path: Path) -> list[str]:
    """Read top-level test functions and methods in recognized test classes."""
    if path.is_symlink():
        message = f"linked test file: {path}"
        raise ValueError(message)
    module = ast.parse(path.read_bytes(), filename=str(path))
    case_classes = unittest_classes(module)
    definitions = []
    for node in module.body:
        if isinstance(node, ast.ClassDef) and (
            node.name.startswith("Test") or node.name in case_classes
        ):
            definitions.extend(node.body)
        else:
            definitions.append(node)
    return sorted(
        node.name.replace("_", " ")
        for node in definitions
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef))
        and node.name.startswith("test_")
    )


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


def main() -> None:
    """Print test sentences for the selected package or repository."""
    parser = argparse.ArgumentParser(
        description=__doc__,
        epilog=(
            "Repository targets list Python packages sequentially and skip packages "
            "without test_main.py. Test source is parsed, never executed."
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
    try:
        target = args.target.resolve()
        if (target / "flake.nix").is_file():
            success = print_repository(target)
        else:
            print_package(target)
            success = True
    except (OSError, SyntaxError, UnicodeError, ValueError) as error:
        sys.stderr.write(f"python_test_names: {error}\n")
        sys.exit(1)
    except KeyboardInterrupt:
        sys.stderr.write("python_test_names: interrupted\n")
        sys.exit(130)
    sys.exit(0 if success else 1)


if __name__ == "__main__":
    main()
