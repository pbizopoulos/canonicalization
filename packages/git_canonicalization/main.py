#!/usr/bin/env python3
# Copyright (c) 2026- Paschalis Bizopoulos
# ruff: noqa: C901, D101, E501, FBT001, FBT003, PLR2004, S101, S603, S607, TRY301
"""Check canonical home repositories and manage canonical flake repositories."""

from __future__ import annotations

import argparse
import ast
import contextlib
import json
import os
import re
import shutil
import stat
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import TYPE_CHECKING, Any, cast
from urllib.parse import urlparse

import nix_syntax
import tomllib

if TYPE_CHECKING:
    from tree_sitter import Node
PACKAGE_KINDS = ("html", "latex", "nix", "python")
KIND_MARKERS = {
    "python": ("main.py",),
    "html": ("index.html",),
    "latex": ("ms.tex",),
}
KIND_SEPARATOR = dict.fromkeys(PACKAGE_KINDS, "_")
ROOT_FILES = {
    ".github/workflows/workflow.yml",
    ".gitignore",
    "LICENSE",
    "README",
    "flake.lock",
    "flake.nix",
    "formatter.nix",
}
OPAQUE_NAME = "prm"
SCRATCH_NAME = "tmp"


class CommandError(RuntimeError):
    """A user-facing command failure."""


@dataclass(frozen=True)
class Package:
    name: str
    kind: str
    root: Path


def run(
    arguments: list[str],
    cwd: Path | None = None,
    *,
    input_text: str | None = None,
    quiet: bool = False,
) -> str:
    """Run a process and preserve its diagnostic and exit status."""
    completed = subprocess.run(
        arguments,
        cwd=cwd,
        input=input_text,
        capture_output=True,
        check=False,
        text=True,
    )
    if completed.returncode != 0:
        if completed.stdout:
            print(completed.stdout, end="")  # noqa: T201
        if completed.stderr:
            print(completed.stderr, end="", file=sys.stderr)  # noqa: T201
        raise SystemExit(completed.returncode)
    if not quiet:
        if completed.stdout:
            print(completed.stdout, end="")  # noqa: T201
        if completed.stderr:
            print(completed.stderr, end="", file=sys.stderr)  # noqa: T201
    return completed.stdout


def git(
    root: Path,
    arguments: list[str],
    *,
    check: bool = True,
) -> subprocess.CompletedProcess[str]:
    """Run Git in a selected repository."""
    completed = subprocess.run(
        ["git", "-C", str(root), *arguments],
        capture_output=True,
        check=False,
        text=True,
    )
    if check and completed.returncode != 0:
        if completed.stdout:
            print(completed.stdout, end="")  # noqa: T201
        if completed.stderr:
            print(completed.stderr, end="", file=sys.stderr)  # noqa: T201
        raise SystemExit(completed.returncode)
    return completed


def repository_root(path: Path = Path()) -> Path:
    """Discover the current Git worktree root."""
    completed = git(
        path,
        ["rev-parse", "--path-format=absolute", "--show-toplevel"],
        check=False,
    )
    if completed.returncode != 0:
        msg = "not inside a Git repository"
        raise CommandError(msg)
    return Path(completed.stdout.strip())


def profile(root: Path, default: str | None = None) -> str:
    """Detect home/submodule and flake repository layouts."""
    flake = any(
        (root / marker).exists()
        for marker in ("flake.nix", "flake.lock", "packages", "checks", "hosts")
    )
    gitignore = _read_regular(root / ".gitignore") or ""
    home = (root / ".gitmodules").exists() or "!/.gitmodules" in gitignore.splitlines()
    if home and not flake:
        return "home"
    if flake and not home:
        return "flake"
    if not home and not flake and default is not None:
        return default
    if home and flake:
        msg = "repository contains markers for both home and flake layouts"
        raise CommandError(
            msg,
        )
    msg = (
        f"cannot determine the repository type; run 'git canonicalization init {root}'"
    )
    raise CommandError(
        msg,
    )


def _read_regular(path: Path) -> str | None:
    try:
        mode = path.lstat().st_mode
    except FileNotFoundError:
        return None
    if not stat.S_ISREG(mode):
        msg = f"{path}: must be a regular file"
        raise CommandError(msg)
    return path.read_text(encoding="utf-8")


def _change(message: str, *, dry_run: bool) -> None:
    """Report one deterministic convergence action."""
    print(("would " if dry_run else "") + message)  # noqa: T201


def _write_managed(
    root: Path,
    relative: Path,
    source: str,
    *,
    dry_run: bool,
    executable: bool = False,
) -> bool:
    """Write and stage one managed file when its contents or mode differ."""
    path = root / relative
    current = _read_regular(path) if path.exists() and not path.is_symlink() else None
    current_mode = path.lstat().st_mode if path.exists() or path.is_symlink() else 0
    mode_matches = bool(current_mode & 0o111) == executable
    if current == source and mode_matches:
        return False
    _change(f"write '{relative}'", dry_run=dry_run)
    if dry_run:
        return True
    path.parent.mkdir(parents=True, exist_ok=True)
    if path.is_symlink():
        path.unlink()
    path.write_text(source, encoding="utf-8")
    path.chmod(0o755 if executable else 0o644)
    git(root, ["add", "--", str(relative)])
    return True


def _tracked_paths(root: Path) -> set[Path]:
    """Return paths represented in the index."""
    completed = git(root, ["ls-files", "-z"])
    return {Path(item) for item in completed.stdout.split("\0") if item}


def _clean_arguments(*, dry_run: bool) -> list[str]:
    """Build the single-force cleanup command with scratch exclusions."""
    return [
        "clean",
        "-ndx" if dry_run else "-fdx",
        "-e",
        f"/{SCRATCH_NAME}/",
        "-e",
        f"/packages/*/{SCRATCH_NAME}/",
    ]


def _clean_output(source: str) -> str:
    """Normalize Git clean actions to the canonical message style."""
    return re.sub(
        r"(?m)^(?:Would remove|Removing) ",
        lambda match: (
            "would remove " if match.group().startswith("Would") else "remove "
        ),
        source,
    )


def hosted_remote(remote: str) -> tuple[str, str]:
    """Parse URL- and SCP-style hosted Git remotes."""
    parsed = urlparse(remote)
    if (
        parsed.scheme in {"http", "https", "ssh", "git+ssh", "git"}
        and parsed.hostname
        and parsed.path.strip("/")
    ):
        return parsed.hostname.lower(), parsed.path.strip("/")
    match = re.fullmatch(r"(?:[^/@:]+@)?([^/:]+):(.+)", remote)
    if match:
        return match.group(1).lower(), match.group(2).rstrip("/")
    msg = f"remote URL has no canonical host and repository path: {remote}"
    raise CommandError(
        msg,
    )


def canonical_remote_path(remote: str) -> Path:
    """Map a hosted remote to its canonical home-relative path."""
    host, remote_path = hosted_remote(remote)
    remote_path = remote_path.removesuffix(".git")
    components = [host, *remote_path.split("/")]
    if len(components) < 3 or any(
        not re.fullmatch(r"[A-Za-z0-9._-]+", component) or component in {".", ".."}
        for component in components
    ):
        msg = "repository path components must contain only ASCII letters, digits, '.', '-', or '_'"
        raise CommandError(
            msg,
        )
    return Path(*components)


def home_repositories(root: Path) -> list[dict[str, str]]:
    """Read submodule records using Git's configuration parser."""
    modules = root / ".gitmodules"
    if not modules.exists():
        return []
    _read_regular(modules)
    completed = git(
        root,
        [
            "config",
            "get",
            "--file",
            str(modules),
            "--null",
            "--show-names",
            "--all",
            "--regexp",
            r"^submodule\..*",
        ],
        check=False,
    )
    if completed.returncode == 1 and not completed.stdout and not completed.stderr:
        return []
    if completed.returncode != 0:
        msg = f"could not read {modules}: {completed.stderr.strip()}"
        raise CommandError(msg)
    grouped: dict[str, dict[str, str]] = {}
    for record in completed.stdout.split("\0"):
        if not record:
            continue
        key, separator, value = record.partition("\n")
        match = re.fullmatch(r"submodule\.(.+)\.(path|url)", key)
        if not separator or match is None:
            msg = "malformed .gitmodules field"
            raise CommandError(msg)
        grouped.setdefault(match.group(1), {})[match.group(2)] = value
    repositories = []
    for name, fields in sorted(grouped.items()):
        if set(fields) != {"path", "url"}:
            msg = f'submodule "{name}": must have exactly one path and one URL'
            raise CommandError(
                msg,
            )
        repositories.append({"name": name, **fields})
    return repositories


def _converge_home_ignore(root: Path, *, dry_run: bool) -> bool:
    """Converge the canonical home whitelist."""
    changed = False
    gitignore_path = root / ".gitignore"
    source = _read_regular(gitignore_path)
    required = ["!/.gitignore", "!/.gitmodules"]
    if source is None:
        source = "*\n" + "\n".join(required) + "\n"
        changed |= _write_managed(root, Path(".gitignore"), source, dry_run=dry_run)
    lines = source.splitlines()
    if (
        not lines
        or lines[0] != "*"
        or any(not line.startswith("!/") for line in lines[1:])
    ):
        msg = f"{gitignore_path}: must start with * and subsequent lines must start with !/"
        raise CommandError(
            msg,
        )
    missing = [line for line in required if line not in lines]
    if missing:
        source = source.rstrip("\n") + "\n" + "\n".join(missing) + "\n"
        changed |= _write_managed(
            root,
            Path(".gitignore"),
            source,
            dry_run=dry_run,
        )
    return changed


def _converge_home_repository(
    root: Path,
    repository: dict[str, str],
    expected: Path,
    *,
    dry_run: bool,
) -> bool:
    """Converge one home submodule record and checkout."""
    actual = Path(repository["path"])
    changed = False
    if repository["name"] != expected.as_posix():
        _change(
            f"rename submodule '{repository['name']}' to '{expected.as_posix()}'",
            dry_run=dry_run,
        )
        changed = True
        if not dry_run:
            git(
                root,
                [
                    "config",
                    "--file",
                    ".gitmodules",
                    "--rename-section",
                    f"submodule.{repository['name']}",
                    f"submodule.{expected.as_posix()}",
                ],
            )
    if actual != expected:
        _change(f"move '{actual}' to '{expected}'", dry_run=dry_run)
        changed = True
        if not dry_run:
            (root / expected).parent.mkdir(parents=True, exist_ok=True)
            git(root, ["mv", "--", str(actual), str(expected)])
    checkout = root / (actual if dry_run and actual != expected else expected)
    if not (checkout / ".git").exists():
        _change(f"initialize submodule '{expected}'", dry_run=dry_run)
        changed = True
        if not dry_run:
            git(root, ["submodule", "update", "--init", "--", str(expected)])
    if (checkout / ".git").exists():
        origin = git(checkout, ["remote", "get-url", "origin"], check=False)
        if origin.returncode != 0 or origin.stdout.strip() != repository["url"]:
            msg = f"{expected}: origin does not match .gitmodules URL"
            raise CommandError(msg)
    return changed


def check_home(root: Path, dry_run: bool) -> list[dict[str, str]]:
    """Converge a canonical home repository."""
    changed = _converge_home_ignore(root, dry_run=dry_run)
    repositories = home_repositories(root)
    actual_paths = [Path(repository["path"]) for repository in repositories]
    expected_paths = [
        canonical_remote_path(repository["url"]) for repository in repositories
    ]
    if len(set(expected_paths)) != len(expected_paths):
        msg = "duplicate canonical repository path"
        raise CommandError(msg)
    if len(set(actual_paths)) != len(actual_paths):
        msg = "duplicate configured repository path"
        raise CommandError(msg)
    for repository, expected in zip(repositories, expected_paths, strict=True):
        changed |= _converge_home_repository(
            root,
            repository,
            expected,
            dry_run=dry_run,
        )
    if not dry_run and repositories:
        git(root, ["add", "--", ".gitmodules"])
    clean = git(root, _clean_arguments(dry_run=dry_run), check=False)
    if clean.returncode != 0:
        raise CommandError(clean.stderr.strip() or "git clean failed")
    if clean.stdout:
        print(_clean_output(clean.stdout), end="")  # noqa: T201
        changed = True
    if dry_run and changed:
        msg_0 = "home repository would change"
        raise CommandError(msg_0)
    return repositories


def detect_packages(root: Path) -> list[Package]:
    """Detect supported packages from unambiguous marker files."""
    packages_root = root / "packages"
    if not packages_root.is_dir():
        return []
    result: list[Package] = []
    for package_root in sorted(
        path
        for path in packages_root.iterdir()
        if path.is_dir() and not path.is_symlink()
    ):
        matches = [
            kind
            for kind, markers in KIND_MARKERS.items()
            if all((package_root / marker).is_file() for marker in markers)
        ]
        if (package_root / "main.py").exists():
            matches = [kind for kind in matches if kind != "latex"]
        if len(matches) > 1:
            msg = f"{package_root.relative_to(root)}: has ambiguous project markers: {', '.join(matches)}"
            raise CommandError(
                msg,
            )
        result.append(
            Package(package_root.name, matches[0] if matches else "nix", package_root),
        )
    return result


def validate_name(kind: str, name: str) -> None:
    """Enforce package naming conventions."""
    separator = KIND_SEPARATOR[kind]
    if not re.fullmatch(rf"[a-z0-9]+(?:{re.escape(separator)}[a-z0-9]+)*", name):
        msg = f"package name must use snake_case: {name}"
        raise CommandError(msg)


def package_files(package: Package) -> set[Path]:
    """Return permitted regular files for a package kind."""
    relative = Path("packages") / package.name
    kind_files = {
        "python": {"main.py"},
        "html": {"index.html", "script.js", "style.css"},
        "latex": {"ms.tex", "ms.bib"},
        "nix": set(),
    }[package.kind]
    return {relative / "default.nix", *(relative / item for item in kind_files)}


def required_package_files(package: Package) -> set[Path]:
    """Return regular files required for a package kind."""
    optional = {
        "html": {"script.js", "style.css"},
        "latex": set(),
        "nix": set(),
        "python": set(),
    }[package.kind]
    return {path for path in package_files(package) if path.name not in optional}


def allowed_paths(root: Path, packages: list[Package]) -> set[Path]:
    """Compute the repository whitelist represented by .gitignore."""
    allowed = {Path(item) for item in ROOT_FILES if (root / item).exists()}
    for directory, filename in (
        ("checks", "default.nix"),
        ("hosts", "configuration.nix"),
    ):
        base = root / directory
        if base.is_dir():
            for child in base.iterdir():
                if child.is_dir() and (child / filename).exists():
                    allowed.add(Path(directory) / child.name / filename)
                    hardware = (
                        Path(directory) / child.name / "hardware-configuration.nix"
                    )
                    if (root / hardware).exists():
                        allowed.add(hardware)
    for package in packages:
        allowed.update(
            path for path in package_files(package) if (root / path).exists()
        )
    return allowed


def opaque_trees(root: Path) -> set[Path]:
    """Return existing repository trees whose contents are unrestricted."""
    candidates = {Path("prm")}
    for parent in ("hosts", "packages"):
        base = root / parent
        if base.is_dir():
            for child in base.iterdir():
                if child.is_dir():
                    candidates.add(Path(parent) / child.name / OPAQUE_NAME)
    return {path for path in candidates if (root / path).is_dir()}


def scratch_trees(root: Path) -> set[Path]:
    """Return permitted untracked scratch trees."""
    candidates = {Path(SCRATCH_NAME)}
    packages = root / "packages"
    if packages.is_dir():
        candidates.update(
            Path("packages") / child.name / SCRATCH_NAME
            for child in packages.iterdir()
            if child.is_dir()
        )
    return {
        path
        for path in candidates
        if (root / path).is_dir() and not (root / path).is_symlink()
    }


def beneath(path: Path, trees: set[Path]) -> bool:
    """Return whether path is a tree or lies beneath one."""
    return any(path == tree or tree in path.parents for tree in trees)


def render_gitignore(paths: set[Path], trees: set[Path] | None = None) -> str:
    """Render a minimal whitelist Git ignore file."""
    trees = trees or set()
    directories: set[Path] = set()
    for path in paths | trees:
        directories.update(path.parents)
    directories.discard(Path())
    patterns = {f"!/{directory.as_posix()}/" for directory in directories}
    patterns.update(f"!/{path.as_posix()}" for path in paths)
    for tree in trees:
        patterns.update((f"!/{tree.as_posix()}/", f"!/{tree.as_posix()}/**"))
    return "\n".join(["*", *sorted(patterns)]) + "\n"


def inspect_structure(root: Path) -> tuple[list[Package], list[str]]:
    """Validate the declared repository subset."""
    packages = detect_packages(root)
    allowed = allowed_paths(root, packages)
    issues: list[str] = []
    for package in packages:
        validate_name(package.kind, package.name)
        for relative in sorted(required_package_files(package)):
            if not (root / relative).is_file():
                issues.extend([f"{relative}: missing required regular file"])
    opaque = opaque_trees(root)
    scratch = scratch_trees(root)
    for path in sorted(root.rglob("*")):
        relative = path.relative_to(root)
        if relative.parts[0] == ".git" or beneath(relative, opaque | scratch):
            continue
        if path.is_symlink():
            issues.append(
                f"{relative}: expected regular file or directory, found symbolic link",
            )
        elif path.is_file() and relative not in allowed:
            issues.append(
                f"{relative}: unsupported by the canonical flake layout; "
                "move unrestricted project files under prm/ "
                f"(for example, prm/{relative.name})",
            )
    return packages, issues


def python_tests(path: Path) -> list[str]:
    """Discover pytest-style tests without executing package source."""
    try:
        module = ast.parse(path.read_text(encoding="utf-8"), filename=str(path))
    except (OSError, SyntaxError, UnicodeError) as error:
        msg = f"{path}: Python source could not be parsed: {error}"
        raise CommandError(
            msg,
        ) from error
    identifiers = []
    for node in module.body:
        if isinstance(
            node,
            (ast.FunctionDef, ast.AsyncFunctionDef),
        ) and node.name.startswith("test_"):
            identifiers.append(node.name)
        if isinstance(node, ast.ClassDef) and node.name.startswith("Test"):
            identifiers.extend(
                child.name
                for child in node.body
                if isinstance(child, (ast.FunctionDef, ast.AsyncFunctionDef))
                and child.name.startswith("test_")
            )
    return sorted({_humanize(identifier) for identifier in identifiers})


def _humanize(identifier: str) -> str:
    words = identifier.removeprefix("test_").replace("_", " ")
    replacements = {
        "cli": "CLI",
        "gitignore": ".gitignore",
        "gitmodules": ".gitmodules",
        "url": "URL",
        "utf8": "UTF-8",
    }
    rendered = " ".join(replacements.get(word, word) for word in words.split())
    return (
        rendered[:1].upper()
        + rendered[1:]
        + ("" if rendered.endswith((".", "!", "?")) else ".")
    )


def package_description(package: Package) -> str | None:
    """Extract declared package metadata where supported."""
    pyproject = package.root / "pyproject.toml"
    if pyproject.is_file():
        try:
            description = (
                tomllib.loads(pyproject.read_text(encoding="utf-8"))
                .get("project", {})
                .get("description")
            )
            if isinstance(description, str):
                return description
        except (OSError, tomllib.TOMLDecodeError):
            pass
    default = _read_regular(package.root / "default.nix")
    if default is None:
        return None
    with contextlib.suppress(json.JSONDecodeError, nix_syntax.NixSyntaxError):
        description = _meta_description(default)
        if description is not None:
            return description
    return None


def _meta_description(source: str) -> str | None:
    """Return a literal meta.description through the Nix syntax tree."""
    document, expression = _meta_description_expression(source)
    if expression is None or expression.type != "string_expression":
        return None
    if any(node.type == "interpolation" for node in nix_syntax.walk(expression)):
        return None
    decoded = json.loads(document.text(expression).replace(r"\${", "${"))
    return cast("str", decoded)


def _nix_string(value: str) -> str:
    """Encode a non-interpolating Nix string literal."""
    return json.dumps(value).replace("${", r"\${")


def _replace_meta_description(source: str, description: str) -> str:
    """Replace a literal meta.description while retaining surrounding source."""
    _document, expression = _meta_description_expression(source)
    if expression is None or expression.type != "string_expression":
        msg = "Python package default.nix must declare exactly one literal meta.description"
        raise CommandError(msg)
    encoded = source.encode("utf-8")
    replacement = _nix_string(description).encode()
    return (
        encoded[: expression.start_byte] + replacement + encoded[expression.end_byte :]
    ).decode("utf-8")


def _meta_description_expression(
    source: str,
) -> tuple[nix_syntax.Document, Node | None]:
    """Find the meta description value in either supported metadata form."""
    document = nix_syntax.parse(source)
    matches = []
    for binding in (
        node for node in nix_syntax.walk(document.root) if node.type == "binding"
    ):
        attrpath = nix_syntax.field(binding, "attrpath")
        expression = nix_syntax.field(binding, "expression")
        if attrpath is None or expression is None:
            continue
        path = nix_syntax.static_attrpath(document, attrpath)
        if path == ("meta", "description"):
            matches.append(expression)
        elif path == ("meta",):
            matches.extend(_attrset_expression(document, expression, ("description",)))
    return document, matches[0] if len(matches) == 1 else None


def _attrset_expression(
    document: nix_syntax.Document,
    expression: Node,
    path: tuple[str, ...],
) -> list[Node]:
    """Find direct static bindings beneath an attribute-set expression."""
    if expression.type != "attrset_expression":
        return []
    binding_set = next(
        (child for child in expression.named_children if child.type == "binding_set"),
        None,
    )
    return [
        value
        for binding in ([] if binding_set is None else binding_set.named_children)
        if binding.type == "binding"
        and (attrpath := nix_syntax.field(binding, "attrpath")) is not None
        and nix_syntax.static_attrpath(document, attrpath) == path
        and (value := nix_syntax.field(binding, "expression")) is not None
    ]


def _compact_nix(source: str) -> str:
    """Normalize insignificant whitespace for template comparisons."""
    return " ".join(source.split())


def _without_dependency_lists(source: str) -> str:
    """Replace package-specific bindings with their template defaults."""
    source = re.sub(
        r"\{\s*inputs,\s*pkgs,\s*\.\.\.\s*\}:",
        "{ pkgs, ... }:",
        source,
        count=1,
    )
    for dependency_name in ("nativeDeps", "pythonDeps"):
        source, count = re.subn(
            rf"(?s)(\b{dependency_name}\s*=\s*)\[.*?\](\s*;)",
            r"\1[ ]\2",
            source,
            count=1,
        )
        if count != 1:
            msg = f"Python package default.nix must declare exactly one {dependency_name} list"
            raise CommandError(msg)
    source, count = re.subn(
        r"(?s)(\bshellHook\s*=\s*)(?:\".*?\"|''.*?'')(\s*;)",
        r'\1""\2',
        source,
        count=1,
    )
    if count != 1:
        msg = "Python package default.nix must declare exactly one shellHook string"
        raise CommandError(msg)
    return _replace_meta_description(source, "A Python package.")


def _check_python_default(package: Package) -> None:
    """Ensure generated Python definitions only customize approved package fields."""
    actual = _read_regular(package.root / "default.nix")
    if actual is None:
        return
    expected = scaffold("python", package.name, None)[
        Path("packages") / package.name / "default.nix"
    ]
    if _compact_nix(_without_dependency_lists(actual)) != _compact_nix(
        _without_dependency_lists(expected),
    ):
        msg = (
            f"packages/{package.name}/default.nix: does not match the canonical "
            "Python package template; permitted customizations are nativeDeps, "
            "pythonDeps, shellHook, and meta.description"
        )
        raise CommandError(msg)


def _check_coverage_default(root: Path, package: Package) -> None:
    """Ensure generated Python coverage checks retain their static definition."""
    check = root / "checks" / f"{package.name}_coverage" / "default.nix"
    if not check.is_file():
        return
    actual = _read_regular(check)
    assert actual is not None
    expected = _python_coverage_source(package.name)
    current = _current_python_coverage_source()
    if _compact_nix(actual) not in {_compact_nix(expected), _compact_nix(current)}:
        msg = (
            f"{check.relative_to(root)}: differs from the canonical coverage "
            "check template"
        )
        raise CommandError(msg)


def _current_python_coverage_source() -> str:
    """Render the current canonical coverage-check definition."""
    return """{ inputs, pkgs, ... }:
let
  checkName = baseNameOf ./.;
  packageDrv = inputs.self.packages.${pkgs.stdenv.system}.${packageName};
  packageName = pkgs.lib.removeSuffix "_coverage" checkName;
  pythonEnv = packageDrv.python.withPackages (
    _:
    packageDrv.propagatedBuildInputs
    ++ [
      packageDrv.python.pkgs.pytest
      packageDrv.python.pkgs.pytest-cov
    ]
  );
in
pkgs.runCommand checkName
  {
    nativeBuildInputs =
      packageDrv.nativeBuildInputs ++ packageDrv.propagatedBuildInputs ++ [ pythonEnv ];
    src = ../.. + "/packages/${packageName}";
  }
  ''
    export HOME="$(mktemp -d)"
    mkdir -p "$out/html"
    cd "$out"
    PACKAGE_E2E_EXECUTABLE="${packageDrv}/bin/${packageName}" python -m pytest -p no:cacheprovider --cov="$src" --cov-report "html:$out/html" "$src/main.py"
  ''
"""


def _binding_value(source: str, name: str, kind: str) -> str | None:
    """ExtractReturn one permitted template binding expression."""
    patterns = {
        "list": rf"(?s)\b{name}\s*=\s*(\[.*?\])\s*;",
        "string": rf"""(?s)\b{name}\s*=\s*((?:"(?:\\.|[^"\\])*"|''.*?''))\s*;""",
    }
    match = re.search(patterns[kind], source)
    return match.group(1) if match else None


def _replace_binding(source: str, name: str, value: str) -> str:
    """Replace one binding expression in a generated template."""
    return re.sub(
        rf"(?s)(\b{name}\s*=\s*)(?:\[.*?\]|\"(?:\\.|[^\"\\])*\"|''.*?'')(\s*;)",
        lambda match: match.group(1) + value + match.group(2),
        source,
        count=1,
    )


def canonical_typed_default(package: Package) -> str | None:
    """Render a typed definition while retaining its permitted fields."""
    if package.kind == "nix":
        return None
    source = _read_regular(package.root / "default.nix")
    if source is None:
        return scaffold(package.kind, package.name, None)[
            Path("packages") / package.name / "default.nix"
        ]
    nix_syntax.parse(source, str(package.root / "default.nix"))
    description = package_description(package)
    rendered = scaffold(package.kind, package.name, description)[
        Path("packages") / package.name / "default.nix"
    ]
    fields = {
        "html": (("runtimeDeps", "list"),),
        "latex": (("nativeDeps", "list"),),
        "python": (
            ("nativeDeps", "list"),
            ("pythonDeps", "list"),
            ("shellHook", "string"),
        ),
    }[package.kind]
    for name, kind in fields:
        value = _binding_value(source, name, kind)
        if value is not None:
            rendered = _replace_binding(rendered, name, value)
    if package.kind == "python" and not re.match(
        r"\s*\{\s*inputs\s*,",
        source,
    ):
        rendered = rendered.replace(
            "{ inputs, pkgs, ... }:",
            "{ pkgs, ... }:",
            1,
        )
    return rendered


def _write_managed_nix(
    root: Path,
    relative: Path,
    source: str,
    *,
    dry_run: bool,
) -> bool:
    """Write a Nix template only when its formatted structure differs."""
    current = _read_regular(root / relative)
    if current is not None and _compact_nix(current) == _compact_nix(source):
        source = current
    return _write_managed(root, relative, source, dry_run=dry_run)


def _converge_packages(root: Path, packages: list[Package], dry_run: bool) -> bool:
    """Converge package templates, generated checks, and file modes."""
    changed = False
    for package in packages:
        expected_default = canonical_typed_default(package)
        if expected_default is not None:
            relative = Path("packages") / package.name / "default.nix"
            changed |= _write_managed_nix(
                root,
                relative,
                expected_default,
                dry_run=dry_run,
            )
        expected_files = scaffold(
            package.kind,
            package.name,
            package_description(package),
        )
        for relative, source in expected_files.items():
            if (
                relative.name == "default.nix"
                or relative not in required_package_files(package)
                or (root / relative).exists()
            ):
                continue
            changed |= _write_managed(
                root,
                relative,
                source,
                dry_run=dry_run,
                executable=relative.name == "main.py",
            )
        if package.kind == "python" and (package.root / "main.py").is_file():
            tests = python_tests(package.root / "main.py")
            check = Path("checks") / f"{package.name}_coverage" / "default.nix"
            if tests:
                changed |= _write_managed_nix(
                    root,
                    check,
                    _current_python_coverage_source(),
                    dry_run=dry_run,
                )
            elif (root / check).exists():
                _change(f"remove '{check.parent}'", dry_run=dry_run)
                changed = True
                if not dry_run:
                    git(root, ["rm", "-rf", "--", str(check.parent)])
        for relative in package_files(package):
            path = root / relative
            if not path.exists() or path.is_symlink():
                continue
            executable = package.kind == "python" and relative.name == "main.py"
            mode_matches = bool(path.stat().st_mode & 0o111) == executable
            if not mode_matches:
                _change(f"set mode on '{relative}'", dry_run=dry_run)
                changed = True
                if not dry_run:
                    path.chmod(0o755 if executable else 0o644)
                    git(root, ["add", "--", str(relative)])
    return changed


def _converge_allowed_files(
    root: Path,
    allowed: set[Path],
    tracked: set[Path],
    python_entrypoints: set[Path],
    *,
    dry_run: bool,
) -> bool:
    """Stage declared files and normalize their executable bits."""
    changed = False
    for relative in sorted(allowed - tracked):
        path = root / relative
        if not path.is_file() or path.is_symlink():
            continue
        _change(f"stage '{relative}'", dry_run=dry_run)
        changed = True
        if not dry_run:
            git(root, ["add", "--", str(relative)])
            tracked.add(relative)
    for relative in sorted(allowed):
        path = root / relative
        if not path.is_file() or path.is_symlink():
            continue
        executable = relative in python_entrypoints
        if bool(path.stat().st_mode & 0o111) != executable:
            _change(f"set mode on '{relative}'", dry_run=dry_run)
            changed = True
            if not dry_run:
                path.chmod(0o755 if executable else 0o644)
                git(root, ["add", "--", str(relative)])
    return changed


def _remove_unsupported_tracked(
    root: Path,
    tracked: set[Path],
    scratch: set[Path],
    protected: set[Path],
    *,
    dry_run: bool,
) -> bool:
    """Untrack scratch content and delete unsupported tracked paths."""
    changed = False
    for relative in sorted(path for path in tracked if beneath(path, scratch)):
        _change(f"untrack '{relative}'", dry_run=dry_run)
        changed = True
        if not dry_run:
            git(root, ["rm", "--cached", "-r", "--", str(relative)])
    for relative in sorted(tracked):
        if beneath(relative, protected):
            continue
        _change(f"remove '{relative}'", dry_run=dry_run)
        changed = True
        if not dry_run:
            git(root, ["rm", "-rf", "--", str(relative)])
    return changed


def _cleanup_flake(root: Path, packages: list[Package], dry_run: bool) -> bool:
    """Remove undeclared files while preserving permitted scratch trees."""
    allowed = allowed_paths(root, packages)
    opaque = opaque_trees(root)
    scratch = scratch_trees(root)
    tracked = _tracked_paths(root)
    python_entrypoints = {
        Path("packages") / package.name / "main.py"
        for package in packages
        if package.kind == "python"
    }
    changed = _converge_allowed_files(
        root,
        allowed,
        tracked,
        python_entrypoints,
        dry_run=dry_run,
    )
    changed |= _remove_unsupported_tracked(
        root,
        tracked,
        scratch,
        opaque | scratch | allowed,
        dry_run=dry_run,
    )
    clean_arguments = _clean_arguments(dry_run=dry_run)
    if dry_run:
        for relative in sorted(allowed):
            if (root / relative).exists():
                clean_arguments.extend(("-e", f"/{relative.as_posix()}"))
    clean = git(root, clean_arguments, check=False)
    if clean.returncode != 0:
        raise CommandError(clean.stderr.strip() or "git clean failed")
    if clean.stdout:
        print(_clean_output(clean.stdout), end="")  # noqa: T201
        changed = True
    return changed


def check_flake(root: Path, dry_run: bool) -> list[Package]:
    """Converge required files, structure, templates, and root whitelist."""
    missing = [
        name
        for name in (".gitignore", "flake.nix", "flake.lock")
        if not (root / name).is_file()
    ]
    if missing:
        raise CommandError("missing required file: " + missing[0])
    packages = detect_packages(root)
    changed = _converge_packages(root, packages, dry_run)
    expected = render_gitignore(allowed_paths(root, packages), opaque_trees(root))
    actual = _read_regular(root / ".gitignore")
    if actual != expected:
        changed |= _write_managed(
            root,
            Path(".gitignore"),
            expected,
            dry_run=dry_run,
        )
    changed |= _cleanup_flake(root, packages, dry_run)
    if dry_run and changed:
        msg = "flake repository would change"
        raise CommandError(msg)
    return validate_flake_source(root)


def validate_flake_source(root: Path) -> list[Package]:
    """Validate a Git-filtered flake source without requiring Git metadata."""
    packages, issues = inspect_structure(root)
    for required in (".gitignore", "README", "flake.lock", "flake.nix"):
        if not (root / required).is_file():
            issues.append(f"{required}: missing required regular file")
    expected_ignore = render_gitignore(
        allowed_paths(root, packages),
        opaque_trees(root),
    )
    if _read_regular(root / ".gitignore") != expected_ignore:
        issues.append(".gitignore: does not match the canonical source whitelist")
    for package in packages:
        issues.extend(_source_package_issues(root, package))
    if issues:
        formatted_issues = "\n".join(f"  - {issue}" for issue in issues)
        msg = f"repository layout validation failed:\n{formatted_issues}"
        raise CommandError(msg)
    for package in packages:
        default = package.root / "default.nix"
        if default.is_file():
            nix_syntax.parse(default.read_bytes(), str(default))
        if package.kind == "python":
            _check_coverage_default(root, package)
    checks_root = root / "checks"
    if checks_root.is_dir():
        for check in checks_root.iterdir():
            default = check / "default.nix"
            if default.is_file():
                nix_syntax.parse(default.read_bytes(), str(default))
    return packages


def _source_package_issues(root: Path, package: Package) -> list[str]:
    """Return source-only typed-template and generated-check issues."""
    issues: list[str] = []
    relative = Path("packages") / package.name / "default.nix"
    actual = _read_regular(root / relative)
    expected = canonical_typed_default(package)
    if (
        expected is not None
        and actual is not None
        and _compact_nix(actual) != _compact_nix(expected)
    ):
        issues.append(f"{relative}: differs from its canonical typed template")
    main = package.root / "main.py"
    if package.kind != "python" or not main.is_file():
        return issues
    check = root / "checks" / f"{package.name}_coverage" / "default.nix"
    tests = python_tests(main)
    if tests and not check.is_file():
        issues.append(f"{check.relative_to(root)}: missing for tested package")
    if not tests and check.exists():
        issues.append(f"{check.relative_to(root)}: exists for untested package")
    return issues


def scaffold(
    kind: str,
    name: str,
    description: str | None,
    tests: list[str] | None = None,
) -> dict[Path, str]:
    """Render one supported package and its optional coverage check."""
    description = (
        description
        or {
            "python": "A Python package.",
            "html": "An HTML package.",
            "latex": "A LaTeX package.",
            "nix": "A Nix package.",
        }[kind]
    )
    description_literal = _nix_string(description)
    root = Path("packages") / name
    defaults = {
        "python": """{ inputs, pkgs, ... }:
let
  nativeDeps = [ ];
  pname = baseNameOf ./.;
  python = pkgs.python3;
  pythonDeps = [ ];
  shellHook = "";
in
python.pkgs.buildPythonPackage {
  inherit pname;
  inherit shellHook;
  installPhase = ''
    install -Dm644 main.py "$out/${python.sitePackages}/$pname.py"
    install -Dm755 main.py "$out/bin/$pname"
    if [ -d prm ]; then
      cp -R prm/ "$out/${python.sitePackages}/"
      cp -R prm/ "$out/bin/"
    fi
  '';
  meta = {
    description = __DESCRIPTION__;
    mainProgram = pname;
  };
  nativeBuildInputs = nativeDeps;
  passthru.python = python;
  propagatedBuildInputs = pythonDeps;
  pyproject = false;
  src = ./.;
  strictDeps = true;
  version = "0.0.0";
}
""",
        "html": """{ pkgs, ... }:
let
  pname = baseNameOf ./.;
  runtimeDeps = [ ];
in
pkgs.writeShellApplication {
  meta.description = __DESCRIPTION__;
  name = pname;
  runtimeInputs = runtimeDeps ++ [ pkgs.http-server ];
  text = ''
    exec http-server ${./.} "$@"
  '';
}
""",
        "latex": """{ pkgs, ... }:
let
  nativeDeps = [ ];
  pname = baseNameOf ./.;
in
pkgs.stdenv.mkDerivation {
  inherit pname;
  buildPhase = ''
    latexmk -pdf ms.tex
  '';
  installPhase = ''
    install -Dm644 ms.pdf "$out/ms.pdf"
  '';
  meta.description = __DESCRIPTION__;
  nativeBuildInputs = nativeDeps ++ [ pkgs.texliveFull ];
  src = ./.;
  strictDeps = true;
  version = "0.0.0";
}
""",
        "nix": """{ pkgs, ... }:
pkgs.writeTextFile {
  name = baseNameOf ./.;
  text = "";
  meta.description = __DESCRIPTION__;
}
""",
    }
    default = defaults[kind].replace("__DESCRIPTION__", description_literal)
    files: dict[Path, str] = {root / "default.nix": default}
    if kind == "python":
        test_source = "".join(
            f"\n\ndef test_{_identifier(test)}() -> None:\n"
            f"    {test!r}\n"
            f"    raise AssertionError({'not implemented: ' + test!r})\n"
            for test in tests or []
        )
        files[root / "main.py"] = (
            f'''#!/usr/bin/env python3\n{description!r}\n\ndef main() -> None:\n    """Run {name}."""\n{test_source}\nif __name__ == "__main__":\n    main()\n'''
        )
        if tests:
            check = Path("checks") / f"{name}_coverage" / "default.nix"
            files[check] = _current_python_coverage_source()
    elif kind == "html":
        files.update(
            {
                root / "index.html": "<!doctype html><html><body></body></html>\n",
                root / "script.js": (
                    'document.documentElement.dataset.javascript = "enabled";\n'
                ),
                root / "style.css": "",
            },
        )
    elif kind == "latex":
        files.update(
            {
                root
                / "ms.tex": "\\documentclass{article}\n\\begin{document}\n\\end{document}\n",
                root / "ms.bib": "",
            },
        )
    return files


def _identifier(description: str) -> str:
    """Render a human behavior name as a stable Python identifier."""
    rendered = re.sub(r"[^a-z0-9]+", "_", description.lower()).strip("_")
    if not rendered:
        return "not_implemented"
    return f"behavior_{rendered}" if rendered[0].isdigit() else rendered


def _python_coverage_source(name: str) -> str:
    return f"""{{ inputs, pkgs, ... }}:\nlet\n  packageDrv = inputs.self.packages.${{pkgs.stdenv.system}}.{name};\n  pythonEnv = packageDrv.python.withPackages (_: packageDrv.propagatedBuildInputs ++ [ packageDrv.python.pkgs.pytest packageDrv.python.pkgs.pytest-cov ]);\nin pkgs.runCommand "{name}_coverage" {{ nativeBuildInputs = [ pythonEnv ]; src = ../../packages/{name}; }} ''\n  mkdir -p "$out/html"\n  cd "$out"\n  PACKAGE_E2E_EXECUTABLE="${{packageDrv}}/bin/{name}" python -m pytest -p no:cacheprovider --cov="$src" --cov-report "html:$out/html" "$src/main.py"\n''\n"""


def add_package(root: Path, kind: str, name: str, description: str | None) -> None:
    """Create a package transactionally and stage its managed files."""
    if kind not in PACKAGE_KINDS:
        msg = f"unsupported package type: {kind}\nhint: supported package types: {', '.join(PACKAGE_KINDS)}"
        raise CommandError(
            msg,
        )
    validate_name(kind, name)
    files = scaffold(kind, name, description)
    if any((root / path).exists() for path in files):
        msg = f"package or generated check already exists: {name}"
        raise CommandError(msg)
    created: list[Path] = []
    try:
        for relative, source in files.items():
            path = root / relative
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(source, encoding="utf-8")
            path.chmod(0o755 if path.name == "main.py" else 0o644)
            created.append(path)
        packages = detect_packages(root)
        nix_syntax.write_if_changed(
            root / ".gitignore",
            render_gitignore(allowed_paths(root, packages), opaque_trees(root)),
        )
        generated = [str(path.relative_to(root)) for path in created] + [".gitignore"]
        completed = git(root, ["add", "--", *generated], check=False)
        if completed.returncode != 0:
            raise CommandError(completed.stderr.strip() or "git add failed")
    except BaseException:
        for path in reversed(created):
            path.unlink(missing_ok=True)
            with contextlib.suppress(OSError):
                path.parent.rmdir()
        raise


def remove_package(root: Path, name: str, dry_run: bool) -> None:
    """Remove a package and generated coverage check safely."""
    package_root = root / "packages" / name
    if not package_root.is_dir() or package_root.is_symlink():
        msg = f"package does not exist: {name}"
        raise CommandError(msg)
    check_root = root / "checks" / f"{name}_coverage"
    targets = [package_root, *([check_root] if check_root.exists() else [])]
    target_relatives = [str(target.relative_to(root)) for target in targets]
    if dry_run:
        for target in targets:
            print(f"rm '{target.relative_to(root)}'")  # noqa: T201
        print("update '.gitignore'")  # noqa: T201
        return
    for target in targets:
        shutil.rmtree(target)
    packages = detect_packages(root)
    nix_syntax.write_if_changed(
        root / ".gitignore",
        render_gitignore(allowed_paths(root, packages), opaque_trees(root)),
    )
    git(
        root,
        [
            "add",
            "--all",
            "--",
            *target_relatives,
            ".gitignore",
        ],
    )


def _imported_status(
    source: str,
) -> tuple[str, list[dict[str, Any]], list[str]]:
    """Validate package and host resources imported from a status document."""
    imported = json.loads(source)
    if not isinstance(imported, dict):
        msg = "status document must be a JSON object"
        raise CommandError(msg)
    packages = imported.get("packages", [])
    hosts = imported.get("hosts", [])
    readme = imported.get("readme")
    if not isinstance(readme, str):
        msg = "status document readme must be a string"
        raise CommandError(msg)
    if not isinstance(packages, list) or not isinstance(hosts, list):
        msg = "status document packages and hosts must be arrays"
        raise CommandError(msg)
    validated_packages: list[dict[str, Any]] = []
    for item in packages:
        if not isinstance(item, dict):
            msg = "status document packages must contain objects"
            raise CommandError(msg)
        kind = item.get("type")
        name = item.get("name")
        description = item.get("description")
        tests = item.get("tests", [])
        if not isinstance(kind, str) or kind not in PACKAGE_KINDS:
            msg = f"unsupported package type: {kind}"
            raise CommandError(msg)
        if not isinstance(name, str):
            msg = "status document package name must be a string"
            raise CommandError(msg)
        if description is not None and not isinstance(description, str):
            msg = "status document package description must be a string or null"
            raise CommandError(msg)
        if not isinstance(tests, list) or not all(
            isinstance(test, str) and test.strip() for test in tests
        ):
            msg = "status document package tests must contain nonempty strings"
            raise CommandError(msg)
        identifiers = [_identifier(test) for test in tests]
        if len(identifiers) != len(set(identifiers)):
            msg = f"status document package tests collide after normalization: {name}"
            raise CommandError(msg)
        validate_name(kind, name)
        validated_packages.append(
            {
                "type": kind,
                "name": name,
                "description": description,
                "tests": tests,
            },
        )
    validated_hosts: list[str] = []
    for host in hosts:
        if not isinstance(host, str) or not re.fullmatch(
            r"[A-Za-z0-9][A-Za-z0-9._-]*",
            host,
        ):
            msg = f"invalid host name: {host}"
            raise CommandError(msg)
        validated_hosts.append(host)
    return readme, validated_packages, validated_hosts


def initialize_home() -> None:
    """Initialize and stage the canonical home policy without cleaning."""
    root = Path.home()
    if not (root / ".git").exists():
        run(["git", "init", str(root)], quiet=True)
    if profile(root, "home") != "home":
        msg = "cannot initialize a flake repository as a home repository"
        raise CommandError(msg)
    path = root / ".gitignore"
    source = _read_regular(path) or "*\n!/.gitignore\n!/.gitmodules\n"
    lines = source.splitlines()
    if not lines or lines[0] != "*":
        msg = f"{path}: must start with *"
        raise CommandError(msg)
    for required in ("!/.gitignore", "!/.gitmodules"):
        if required not in lines:
            lines.append(required)
    _write_managed(root, Path(".gitignore"), "\n".join(lines) + "\n", dry_run=False)


def _remote_is_empty(remote: str) -> bool:
    """Return whether a hosted remote advertises no heads."""
    completed = subprocess.run(
        ["git", "ls-remote", remote],
        capture_output=True,
        check=False,
        text=True,
    )
    if completed.returncode != 0:
        raise CommandError(completed.stderr.strip() or "could not read remote")
    return not completed.stdout.strip()


def initialize_flake(remote: str, status_path: str | None) -> None:
    """Create a canonical flake at its remote-derived home path."""
    relative = canonical_remote_path(remote)
    home = Path.home()
    if repository_root(home) != home or profile(home) != "home":
        msg = "$HOME must be an initialized canonical home repository"
        raise CommandError(msg)
    if not _remote_is_empty(remote):
        msg = "init flake requires an empty remote"
        raise CommandError(msg)
    directory = home / relative
    if directory.exists():
        msg = f"target already exists: {directory}"
        raise CommandError(msg)
    directory.parent.mkdir(parents=True, exist_ok=True)
    run(["git", "clone", remote, str(directory)], quiet=True)
    flake = directory / "flake.nix"
    flake.write_text(
        '{ inputs.canonicalization.url = "github:pbizopoulos/canonicalization"; outputs = inputs: inputs.canonicalization.blueprint { inherit inputs; }; }\n',
        encoding="utf-8",
    )
    readme = f"# {directory.name}\n"
    if status_path is not None:
        source = (
            sys.stdin.read()
            if status_path == "-"
            else Path(status_path).read_text(encoding="utf-8")
        )
        readme, package_specs, hosts = _imported_status(source)
        (directory / "README").write_text(readme, encoding="utf-8")
        for item in package_specs:
            files = scaffold(
                item["type"],
                item["name"],
                item.get("description"),
                item.get("tests"),
            )
            for path, contents in files.items():
                target = directory / path
                target.parent.mkdir(parents=True, exist_ok=True)
                target.write_text(contents, encoding="utf-8")
                if target.name == "main.py":
                    target.chmod(0o755)
        for host in hosts:
            path = directory / "hosts" / host / "configuration.nix"
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text("{ ... }: { }\n", encoding="utf-8")
    else:
        (directory / "README").write_text(readme, encoding="utf-8")
    run(
        [os.environ.get("GIT_CANONICALIZATION_NIX", "nix"), "flake", "lock"],
        cwd=directory,
        quiet=True,
    )
    detected_packages = detect_packages(directory)
    (directory / ".gitignore").write_text(
        render_gitignore(
            allowed_paths(directory, detected_packages),
            opaque_trees(directory),
        ),
        encoding="utf-8",
    )
    run(
        [os.environ.get("GIT_CANONICALIZATION_NIX", "nix"), "fmt"],
        cwd=directory,
        quiet=True,
    )
    git(directory, ["add", "--all"])
    git(directory, ["branch", "-M", "main"])
    git(directory, ["commit", "-m", "Initialize repository"])
    git(
        home,
        [
            "submodule",
            "add",
            "--force",
            "--name",
            relative.as_posix(),
            remote,
            str(relative),
        ],
    )


def status(root: Path) -> dict[str, Any]:
    """Build the stable repository status payload."""
    current_profile = profile(root)
    if current_profile == "home":
        msg = "home repositories are not compatible with status"
        raise CommandError(msg)
    packages = check_flake(root, True)
    run(
        [
            os.environ.get("GIT_CANONICALIZATION_NIX", "nix"),
            "build",
            ".#checks.x86_64-linux.pkgs-formatter-check",
            "--no-link",
        ],
        cwd=root,
        quiet=True,
    )
    return {
        "readme": _read_regular(root / "README"),
        "packages": [
            {
                "name": package.name,
                "type": package.kind,
                "description": package_description(package),
                "tests": python_tests(package.root / "main.py")
                if package.kind == "python"
                else [],
            }
            for package in packages
        ],
        "hosts": sorted(path.name for path in (root / "hosts").iterdir())
        if (root / "hosts").is_dir()
        else [],
    }


def parser() -> argparse.ArgumentParser:
    """Construct the public command-line parser."""
    result = argparse.ArgumentParser(
        prog="git canonicalization",
        description="Check canonical home repositories and manage Nix flake repositories.",
    )
    commands = result.add_subparsers(dest="command", required=True)
    init = commands.add_parser("init", help="Initialize a canonical repository.")
    init.add_argument("profile", choices=("flake", "home"))
    init.add_argument("remote", nargs="?")
    init.add_argument("--from-status", metavar="FILE|-")
    commands.add_parser("status", help="Write repository status as JSON.")
    add = commands.add_parser("add", help="Scaffold a package.")
    add.add_argument("type")
    add.add_argument("name")
    add.add_argument("description", nargs="*")
    remove = commands.add_parser("rm", help="Remove a package and its generated check.")
    remove.add_argument("name")
    remove.add_argument("-n", "--dry-run", action="store_true")
    check = commands.add_parser("check", help="Converge the selected repository.")
    check.add_argument("-n", "--dry-run", action="store_true")
    check.add_argument("--source", type=Path, help=argparse.SUPPRESS)
    return result


def _dispatch_init(options: argparse.Namespace) -> bool:
    """Dispatch initialization and report whether it handled the command."""
    if options.command != "init":
        return False
    if options.profile == "home":
        if options.remote is not None or options.from_status is not None:
            msg = "init home does not accept a remote or status document"
            raise CommandError(msg)
        initialize_home()
    else:
        if options.remote is None:
            msg = "init flake requires REMOTE"
            raise CommandError(msg)
        initialize_flake(options.remote, options.from_status)
    return True


def main() -> None:
    """Dispatch the git canonicalization CLI."""
    arguments = sys.argv[1:]
    if arguments[:1] == ["help"]:
        arguments = [arguments[1], "--help"] if len(arguments) > 1 else ["--help"]
    try:
        options = parser().parse_args(arguments)
        if _dispatch_init(options):
            return
        if options.command == "check" and options.source is not None:
            validate_flake_source(options.source.resolve())
            return
        root = repository_root()
        current_profile = profile(root)
        if options.command in {"add", "rm"} and current_profile != "flake":
            msg = f"{current_profile} repositories do not support package resources"
            raise CommandError(
                msg,
            )
        if options.command == "status":
            print(  # noqa: T201
                json.dumps(
                    status(root),
                    ensure_ascii=False,
                    separators=(",", ":"),
                    sort_keys=True,
                ),
            )
        elif options.command == "check":
            check_home(
                root,
                options.dry_run,
            ) if current_profile == "home" else check_flake(
                root,
                options.dry_run,
            )
        elif options.command == "add":
            add_package(
                root,
                options.type,
                options.name,
                " ".join(options.description) or None,
            )
        elif options.command == "rm":
            remove_package(root, options.name, options.dry_run)
    except (
        CommandError,
        OSError,
        UnicodeError,
        json.JSONDecodeError,
        tomllib.TOMLDecodeError,
        nix_syntax.NixSyntaxError,
    ) as error:
        print(f"error: {error}", file=sys.stderr)  # noqa: T201
        raise SystemExit(1) from error


def test_removed_package_kinds_are_not_supported() -> None:
    """Reject package kinds replaced by typed or unconstrained Nix packages."""
    assert {"haskell", "opentofu", "python-latex"}.isdisjoint(PACKAGE_KINDS)


def test_python_package_allows_latex_resources_in_prm() -> None:
    """Classify LaTeX resources under prm as an opaque Python implementation detail."""
    with tempfile.TemporaryDirectory() as temporary_directory:
        root = Path(temporary_directory)
        package = root / "packages" / "report"
        (package / "prm").mkdir(parents=True)
        (package / "default.nix").write_text("{ }: { }\n", encoding="utf-8")
        (package / "main.py").write_text("", encoding="utf-8")
        (package / "prm" / "ms.tex").write_text("", encoding="utf-8")
        assert detect_packages(root) == [Package("report", "python", package)]


def test_domain_resources_in_prm_remain_an_unconstrained_nix_package() -> None:
    """Treat an OpenTofu implementation under prm as opaque Nix package data."""
    with tempfile.TemporaryDirectory() as temporary_directory:
        root = Path(temporary_directory)
        package = root / "packages" / "deployment"
        (package / "prm").mkdir(parents=True)
        (package / "default.nix").write_text("{ pkgs, ... }: pkgs.emptyFile\n")
        (package / "prm" / "main.tf").write_text("terraform {}\n")
        (package / "prm" / ".terraform.lock.hcl").write_text("")
        detected = Package("deployment", "nix", package)
        assert detect_packages(root) == [detected]
        assert package_files(detected) == {Path("packages/deployment/default.nix")}


def test_html_styles_and_scripts_are_optional() -> None:
    """Allow HTML packages without standalone CSS or JavaScript assets."""
    with tempfile.TemporaryDirectory() as temporary_directory:
        root = Path(temporary_directory)
        package_root = root / "packages" / "cv"
        package_root.mkdir(parents=True)
        files = scaffold("html", "cv", None)
        for name in ("default.nix", "index.html"):
            relative = Path("packages/cv") / name
            (root / relative).write_text(files[relative], encoding="utf-8")
        package = Package("cv", "html", package_root)
        assert required_package_files(package) == {
            Path("packages/cv/default.nix"),
            Path("packages/cv/index.html"),
        }
        assert inspect_structure(root) == ([package], [])
        assert not _converge_packages(root, [package], False)
        assert not (package_root / "script.js").exists()
        assert not (package_root / "style.css").exists()


def test_repository_layout_error_explains_how_to_place_unrestricted_files() -> None:
    """Report unsupported paths together with an actionable prm location."""
    with tempfile.TemporaryDirectory() as temporary_directory:
        root = Path(temporary_directory)
        for required in (".gitignore", "flake.lock", "flake.nix"):
            (root / required).write_text("", encoding="utf-8")
        secrets = root / "secrets"
        secrets.mkdir()
        (secrets / "secrets.age").write_text("", encoding="utf-8")
        _packages, issues = inspect_structure(root)
        assert issues == [
            (
                "secrets/secrets.age: unsupported by the canonical flake "
                "layout; move unrestricted project files under prm/ "
                "(for example, prm/secrets.age)"
            ),
        ]


def test_python_scaffold_installs_optional_prm_resources() -> None:
    """Use one canonical Python package template for optional package resources."""
    files = scaffold("python", "report", None)
    default = files[Path("packages/report/default.nix")]
    assert "if [ -d prm ]; then" in default
    assert 'cp -R prm/ "$out/bin/"' in default
    assert '"$out/${python.sitePackages}/$pname.py"' in default
    assert "nativeDeps = [ ];" in default
    assert "pythonDeps = [ ];" in default
    assert "<nixpkgs>" not in default
    assert "passthru.python = python;" in default


def test_python_scaffold_escapes_arbitrary_description() -> None:
    """Produce parseable Python source for descriptions containing quotes and newlines."""
    source = scaffold("python", "report", 'A """ quoted\\ndescription.')[
        Path("packages/report/main.py")
    ]
    module = ast.parse(source)
    assert ast.get_docstring(module) == 'A """ quoted\\ndescription.'


def test_meta_description_uses_nix_syntax() -> None:
    """Read and replace metadata without relying on source formatting."""
    nested = '{ meta = { description = "A \\"quoted\\" description."; }; }'
    direct = '{ meta.description = "A direct description."; }'
    assert _meta_description(nested) == 'A "quoted" description.'
    assert _meta_description(direct) == "A direct description."
    assert _replace_meta_description(nested, "A replacement.") == (
        '{ meta = { description = "A replacement."; }; }'
    )
    escaped = _replace_meta_description(nested, "Literal ${value}.")
    assert r"Literal \${value}." in escaped
    assert _meta_description(escaped) == "Literal ${value}."


def test_imported_status_rejects_invalid_shapes_and_host_paths() -> None:
    """Reject malformed imported resources before they affect the destination."""
    for source in (
        "[]",
        '{"packages": {}}',
        '{"packages": [{"type": "python"}]}',
        '{"hosts": ["../outside"]}',
        (
            '{"readme":"x","packages":[{"type":"python","name":"sample",'
            '"tests":["A-B","A B"]}],"hosts":[]}'
        ),
    ):
        _assert_invalid_imported_status(source)


def _assert_invalid_imported_status(source: str) -> None:
    try:
        _imported_status(source)
    except CommandError:
        return
    msg = f"invalid status was accepted: {source}"
    raise AssertionError(msg)


def test_python_default_allows_only_package_customization() -> None:
    """Permit dependency and shell-hook changes but reject template changes."""
    with tempfile.TemporaryDirectory() as temporary_directory:
        root = Path(temporary_directory)
        package_root = root / "packages" / "report"
        package_root.mkdir(parents=True)
        source = scaffold("python", "report", None)[Path("packages/report/default.nix")]
        source = source.replace(
            "pythonDeps = [ ];",
            "pythonDeps = [ pkgs.some_dependency ];",
        )
        source = source.replace(
            "nativeDeps = [ ];",
            "nativeDeps = [ pkgs.some_native_dependency ];",
        )
        source = source.replace(
            'shellHook = "";',
            "shellHook = ''\n  export EXAMPLE=value\n'';",
        )
        source = source.replace("{ inputs, pkgs, ... }:", "{ pkgs, ... }:")
        package = Package("report", "python", package_root)
        (package_root / "default.nix").write_text(source, encoding="utf-8")
        _check_python_default(package)
        (package_root / "default.nix").write_text(
            source.replace('version = "0.0.0";', 'version = "1.0.0";'),
            encoding="utf-8",
        )
        try:
            _check_python_default(package)
        except CommandError as error:
            error_message = str(error)
        else:
            msg = "static Python package template drift was not detected"
            raise AssertionError(msg)
        assert error_message == (
            "packages/report/default.nix: does not match the canonical "
            "Python package template; permitted customizations are nativeDeps, "
            "pythonDeps, shellHook, and meta.description"
        )


def test_coverage_default_matches_current_template() -> None:
    """Recognize the canonical generated coverage check definition."""
    with tempfile.TemporaryDirectory() as temporary_directory:
        root = Path(temporary_directory)
        check = root / "checks" / "report_coverage"
        check.mkdir(parents=True)
        (check / "default.nix").write_text(
            _current_python_coverage_source(),
            encoding="utf-8",
        )
        _check_coverage_default(root, Package("report", "python", root / "report"))


def test_remote_paths_and_test_names() -> None:
    """Canonicalizes hosted remotes and humanizes Python tests."""
    assert canonical_remote_path("git@github.com:owner/demo.git") == Path(
        "github.com/owner/demo",
    )
    assert _humanize("test_cli_handles_utf8_url") == "CLI handles UTF-8 URL."


def test_gitignore_patterns_are_globally_sorted() -> None:
    """Sort directory and file whitelist patterns together."""
    assert render_gitignore(
        {Path("z/file"), Path("a")},
        {Path("prm")},
    ) == ("*\n!/a\n!/prm/\n!/prm/**\n!/z/\n!/z/file\n")


def test_git_clean_actions_use_canonical_message_style() -> None:
    """Use lowercase imperative action messages for Git cleanup output."""
    assert _clean_output("Would remove tmp.txt\n") == "would remove tmp.txt\n"
    assert _clean_output("Removing tmp.txt\n") == "remove tmp.txt\n"


def _temporary_flake(root: Path) -> None:
    """Create the minimum indexed flake used by convergence tests."""
    git(root, ["init", "--quiet"])
    for relative in (".gitignore", "README", "flake.lock", "flake.nix"):
        (root / relative).write_text("", encoding="utf-8")
    git(root, ["add", ".gitignore", "README", "flake.lock", "flake.nix"])


def test_convergence_preserves_root_and_package_scratch_only() -> None:
    """Preserve both allowed tmp trees while deleting unsupported artifacts."""
    with tempfile.TemporaryDirectory() as temporary_directory:
        root = Path(temporary_directory)
        _temporary_flake(root)
        package = root / "packages" / "sample"
        package.mkdir(parents=True)
        (package / "default.nix").write_text("{ pkgs, ... }: pkgs.emptyFile\n")
        root_tmp = root / "tmp" / "root-state"
        package_tmp = package / "tmp" / "package-state"
        root_tmp.parent.mkdir()
        package_tmp.parent.mkdir()
        root_tmp.write_text("root", encoding="utf-8")
        package_tmp.write_text("package", encoding="utf-8")
        unsupported = root / "result"
        unsupported.write_text("unsupported", encoding="utf-8")
        git(
            root,
            [
                "add",
                "--force",
                "packages/sample/default.nix",
                "packages/sample/tmp/package-state",
                "result",
            ],
        )
        check_flake(root, False)
        assert root_tmp.read_text(encoding="utf-8") == "root"
        assert package_tmp.read_text(encoding="utf-8") == "package"
        assert not unsupported.exists()
        assert (
            "packages/sample/tmp/package-state"
            not in git(
                root,
                ["ls-files"],
            ).stdout.splitlines()
        )


def test_single_force_cleanup_rejects_nested_git_repository() -> None:
    """Leave nested Git data intact and then reject its unsupported structure."""
    with tempfile.TemporaryDirectory() as temporary_directory:
        root = Path(temporary_directory)
        _temporary_flake(root)
        nested = root / "undeclared"
        nested.mkdir()
        git(nested, ["init", "--quiet"])
        error_message = ""
        try:
            check_flake(root, False)
        except CommandError as error:
            error_message = str(error)
        else:
            msg = "nested Git repository passed structural validation"
            raise AssertionError(msg)
        assert "repository layout validation failed" in error_message
        assert (nested / ".git").is_dir()


def test_scaffold_coverage_exists_only_for_declared_behaviors() -> None:
    """Generate coverage and failing stubs only when behavior names are supplied."""
    without_tests = scaffold("python", "sample", None)
    assert Path("checks/sample_coverage/default.nix") not in without_tests
    with_tests = scaffold(
        "python",
        "sample",
        None,
        ["Loads a document", "123", 'Says "yes"\nnow'],
    )
    source = with_tests[Path("packages/sample/main.py")]
    ast.parse(source)
    assert Path("checks/sample_coverage/default.nix") in with_tests
    assert "def test_loads_a_document()" in source
    assert "def test_behavior_123()" in source
    assert "not implemented: Loads a document" in source


if __name__ == "__main__":
    main()
