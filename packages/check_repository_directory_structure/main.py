#!/usr/bin/env python3
"""Check repository directory structure."""

import os
import re
import shutil
import sys
import unittest
from pathlib import Path

import fire
import git
from fqdn import FQDN

names_allowed = {
    r"\.env\.age",
    r"\.git(/.*)?",
    r"\.github/workflows/workflow\.yml",
    r"\.gitignore",
    r"CITATION\.bib",
    r"LICENSE",
    r"README",
    r"checks/[^/]+/default\.nix",
    r"flake\.lock",
    r"flake\.nix",
    r"formatter\.nix",
    r"prm(/.*)?",
    r"secrets\.nix",
    r"tmp(/.*)?",
}
file_dependencies = {
    r"hosts/[^/]+/configuration\.nix": {
        r"hosts/[^/]+/\.terraform\.lock\.hcl",
        r"hosts/[^/]+/\.terraform(/.*)?",
        r"hosts/[^/]+/deploy-requirements\.sh",
        r"hosts/[^/]+/deploy\.sh",
        r"hosts/[^/]+/hardware-configuration\.nix",
        r"hosts/[^/]+/main\.tf",
    },
    r"packages/[^/]+/default\.nix": {
        r"packages/[^/]+/prm(/.*)?",
        r"packages/[^/]+/tmp(/.*)?",
    },
    r"packages/[^/]+/package\.json": {
        r"packages/[^/]+/app(/.*)?",
        r"packages/[^/]+/default\.nix",
        r"packages/[^/]+/next\.config\.mjs",
        r"packages/[^/]+/package-lock\.json",
        r"packages/[^/]+/postcss\.config\.mjs",
        r"packages/[^/]+/tailwind\.config\.ts",
        r"packages/[^/]+/tsconfig\.json",
    },
    r"packages/[^/]+/index\.html": {
        r"CNAME",
        r"packages/[^/]+/default\.nix",
        r"packages/[^/]+/script\.js",
        r"packages/[^/]+/style\.css",
    },
    r"packages/[^/]+/main\.c": {
        r"packages/[^/]+/default\.nix",
    },
    r"packages/[^/]+/ms\.tex": {
        r"packages/[^/]+/default\.nix",
        r"packages/[^/]+/ms\.bib",
    },
    r"packages/[^/]+/Main\.hs": {
        r"packages/[^/]+/default\.nix",
        r"packages/[^/]+/main\.cabal",
    },
    r"packages/[^/]+/main\.py": {
        r"packages/[^/]+/\.env\.age",
        r"packages/[^/]+/default\.nix",
        r"packages/[^/]+/static(/.*)?",
        r"packages/[^/]+/templates(/.*)?",
    },
    r"packages/[^/]+/main\.sh": {
        r"packages/[^/]+/default\.nix",
    },
}
compiled_names_allowed = [re.compile(p) for p in names_allowed]
compiled_file_dependencies = {
    re.compile(trigger): {re.compile(pat) for pat in patterns}
    for trigger, patterns in file_dependencies.items()
}
_trigger_by_dir = {}  # type: ignore[var-annotated]
for trigger_regex in compiled_file_dependencies:
    parts = trigger_regex.pattern.split("/")
    top_dir = parts[0]
    _trigger_by_dir.setdefault(top_dir, []).append(trigger_regex)


class _TestCase(unittest.TestCase):
    def test_check_repository_directory_structure_file_input(self) -> None:
        with self.assertRaises(SystemExit) as cm:  # noqa: PT027
            check_repository_directory_structure()
        if cm.exception.code != 0:
            raise AssertionError


def _get_additional_allowed_patterns(path_set: set[Path]) -> set[re.Pattern]:  # type: ignore[type-arg]
    additional_patterns = set()
    for path in path_set:
        path_str = path.as_posix()
        top_dir = path.parts[0] if path.parts else ""
        for trigger_pattern in _trigger_by_dir.get(top_dir, []):
            if trigger_pattern.fullmatch(path_str):
                additional_patterns.update(compiled_file_dependencies[trigger_pattern])
                additional_patterns.add(trigger_pattern)
    return additional_patterns


def check_repository_directory_structure(  # noqa: C901, PLR0912
    dir_name: str = ".",
    fix: bool = False,  # noqa: FBT001,FBT002
) -> None:
    """Check git."""
    warnings = []
    dir_path = Path(dir_name).resolve()
    if not dir_path.is_dir():
        dir_path = dir_path.parent
    git_repo = git.Repo(dir_path, search_parent_directories=True)
    dir_path = Path(git_repo.working_dir)
    if fix:
        git_log = git_repo.git.execute(["git", "clean", "-df"])
        if git_log:
            sys.stdout.write(f"{git_log}\n")
    else:
        for untracked_file in git_repo.untracked_files:
            warnings.append(f"{dir_path}/{untracked_file}: is untracked")  # noqa: PERF401
    if git_repo.active_branch.name != "main":
        warnings.append(f"{dir_path}: should have 'main' as the active branch")
    if len(git_repo.branches) != 1:
        warnings.append(f"{dir_path}: should have only one branch")
    if (
        dir_path.name == dir_path.name.lower()
        and not FQDN(dir_path.name).is_valid
        and not re.match(r"^[a-z0-9]+([-.][a-z0-9]+)*$", dir_path.name)
    ):
        warnings.append(
            f"{dir_path}: should be lower-case and valid FQDN or in dash-case",
        )
    paths = [
        path
        for path in Path(dir_path).rglob("*")
        if not any(part in ("tmp", "prm") for part in path.parts[:-1])
    ]
    paths.sort()
    dir_and_file_names = set()
    for path in paths:
        rel_path = path.relative_to(dir_path)
        if path.is_file() or not any(child for child in paths if child.parent == path):
            dir_and_file_names.add(rel_path)
    compiled_allowed_patterns = compiled_names_allowed + list(
        _get_additional_allowed_patterns(dir_and_file_names),
    )
    dir_and_file_names = {
        name
        for name in dir_and_file_names
        if not any(p.fullmatch(name.as_posix()) for p in compiled_allowed_patterns)
    }
    for dir_and_file_name in sorted(dir_and_file_names):
        if fix:
            if dir_and_file_name.is_file():
                dir_and_file_name.unlink()
            elif dir_and_file_name.is_dir():
                shutil.rmtree(dir_and_file_name)
            sys.stdout.write(f"{dir_path}/{dir_and_file_name}: removed\n")
        else:
            warnings.append(f"{dir_path}/{dir_and_file_name}: is not allowed")
    if warnings:
        sys.stdout.write("\n".join(warnings) + "\n")
        sys.exit(1)
    else:
        sys.exit(0)


def main() -> None:
    """Launch check_repository_directory_structure using the Fire module."""
    fire.Fire(check_repository_directory_structure)


if __name__ == "__main__":
    if os.getenv("DEBUG"):
        unittest.main()
    else:
        main()
