#!/usr/bin/env python3
# Copyright (c) 2026- Paschalis Bizopoulos
"""Canonicalize home repositories and manage canonical flake repositories."""

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
from dataclasses import dataclass
from pathlib import Path
from typing import TYPE_CHECKING, cast
from urllib.parse import urlparse

import nix_syntax

if TYPE_CHECKING:
    from tree_sitter import Node
PACKAGE_KINDS = ("html", "latex", "nix", "python")
KIND_MARKERS = {
    "html": "index.html",
    "latex": "ms.tex",
    "python": "main.py",
}
ROOT_FILES = {
    ".forgejo/workflows/workflow.yml",
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
class Package:  # noqa: D101
    name: str
    kind: str
    root: Path


def _run(
    arguments: list[str],
    cwd: Path | None = None,
    *,
    check: bool = True,
) -> subprocess.CompletedProcess[str]:
    """Run a process and preserve failures and captured output."""
    completed = subprocess.run(  # noqa: S603
        arguments,
        cwd=cwd,
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


def git(
    root: Path,
    arguments: list[str],
    *,
    check: bool = True,
) -> subprocess.CompletedProcess[str]:
    """Run Git in a selected repository."""
    return _run(
        ["git", "-C", str(root), *arguments],
        check=check,
    )


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
        "cannot determine the repository type; run "
        "'git canonical init home' or "
        "'git canonical init flake REMOTE'"
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


def _clean_arguments(*, dry_run: bool, exclusions: tuple[str, ...]) -> list[str]:
    """Build a native Git clean command with profile-selected exclusions."""
    arguments = ["clean", "-ndx" if dry_run else "-fdx"]
    for exclusion in exclusions:
        arguments.extend(("-e", exclusion))
    return arguments


def _flake_clean_arguments(*, dry_run: bool) -> list[str]:
    """Build the flake cleanup command."""
    return _clean_arguments(
        dry_run=dry_run,
        exclusions=(f"/{SCRATCH_NAME}/", f"/packages/*/{SCRATCH_NAME}/"),
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
    if len(components) < 3 or any(  # noqa: PLR2004
        not re.fullmatch(r"[A-Za-z0-9._-]+", component) or component in {".", ".."}
        for component in components
    ):
        msg = "repository path components must contain only ASCII letters, digits, '.', '-', or '_'"  # noqa: E501
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
        msg = f"{gitignore_path}: must start with * and subsequent lines must start with !/"  # noqa: E501
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
    if actual != expected:
        _change(f"move '{actual}' to '{expected}'", dry_run=dry_run)
        changed = True
        if not dry_run:
            (root / expected).parent.mkdir(parents=True, exist_ok=True)
            git(root, ["mv", "--", str(actual), str(expected)])
            git(
                root,
                [
                    "config",
                    "--file",
                    ".gitmodules",
                    f"submodule.{repository['name']}.path",
                    expected.as_posix(),
                ],
            )
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
            configured = git(
                root,
                ["config", "--get", f"submodule.{repository['name']}.url"],
                check=False,
            )
            if configured.returncode == 0:
                git(
                    root,
                    [
                        "config",
                        "--rename-section",
                        f"submodule.{repository['name']}",
                        f"submodule.{expected.as_posix()}",
                    ],
                )
    if changed and not dry_run:
        git(root, ["add", "--", ".gitmodules"])
    checkout = root / (actual if dry_run and actual != expected else expected)
    if not (checkout / ".git").exists():
        _change(f"initialize submodule '{expected}'", dry_run=dry_run)
        changed = True
        if not dry_run:
            git(root, ["submodule", "update", "--init", "--", str(expected)])
    if (checkout / ".git").exists():
        changed |= _converge_home_checkout(
            root,
            checkout,
            expected,
            repository["url"],
            dry_run=dry_run,
        )
    return changed


def _converge_home_checkout(
    root: Path,
    checkout: Path,
    expected: Path,
    configured_url: str,
    *,
    dry_run: bool,
) -> bool:
    """Synchronize a present submodule URL without changing its recorded commit."""
    changed = False
    origin = git(checkout, ["remote", "get-url", "origin"], check=False)
    if origin.returncode != 0:
        msg = f"{expected}: checkout has no origin remote"
        raise CommandError(msg)
    if origin.stdout.strip() != configured_url:
        _change(
            f"synchronize submodule URL for '{expected}'",
            dry_run=dry_run,
        )
        changed = True
        if not dry_run:
            git(root, ["submodule", "sync", "--recursive", "--", str(expected)])
            synchronized = git(checkout, ["remote", "get-url", "origin"], check=False)
            if (
                synchronized.returncode != 0
                or synchronized.stdout.strip() != configured_url
            ):
                msg = f"{expected}: origin does not match .gitmodules URL after sync"
                raise CommandError(msg)
    return changed


def check_home(root: Path, dry_run: bool) -> list[dict[str, str]]:  # noqa: FBT001
    """Converge a canonical home repository."""
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
    for actual, expected in zip(actual_paths, expected_paths, strict=True):
        if actual != expected and (root / expected).exists():
            msg = f"target already exists: {expected}"
            raise CommandError(msg)
    if (
        actual_paths != expected_paths
        and git(
            root,
            ["diff", "--quiet", "--", ".gitmodules"],
            check=False,
        ).returncode
    ):
        msg = "stage .gitmodules with git add before moving submodules"
        raise CommandError(msg)
    changed = _converge_home_ignore(root, dry_run=dry_run)
    for repository, expected in zip(repositories, expected_paths, strict=True):
        changed |= _converge_home_repository(
            root,
            repository,
            expected,
            dry_run=dry_run,
        )
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
            for kind, marker in KIND_MARKERS.items()
            if (package_root / marker).is_file()
        ]
        if (package_root / "main.py").exists():
            matches = [kind for kind in matches if kind != "latex"]
        if len(matches) > 1:
            msg = f"{package_root.relative_to(root)}: has ambiguous project markers: {', '.join(matches)}"  # noqa: E501
            raise CommandError(
                msg,
            )
        result.append(
            Package(package_root.name, matches[0] if matches else "nix", package_root),
        )
    return result


def validate_name(name: str) -> None:
    """Enforce package naming conventions."""
    if not re.fullmatch(r"[a-z0-9]+(?:_[a-z0-9]+)*|[a-z0-9]+(?:-[a-z0-9]+)*", name):
        msg = f"package name must use snake_case or dash-case: {name}"
        raise CommandError(msg)


def validate_host_name(name: str) -> None:
    """Enforce lower camelCase host names."""
    if not re.fullmatch(r"[a-z][A-Za-z0-9]*", name):
        msg = f"host name must use camelCase: {name}"
        raise CommandError(msg)


def package_files(package: Package) -> set[Path]:
    """Return permitted regular files for a package kind."""
    relative = Path("packages") / package.name
    kind_files = {
        "python": {"main.py", "test_main.py"},
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
        "python": {"test_main.py"},
    }[package.kind]
    return {path for path in package_files(package) if path.name not in optional}


def canonical_checks(root: Path, packages: list[Package]) -> dict[Path, str]:
    """Return generated checks derived from the repository's resources."""
    checks = {
        Path("checks") / f"{package.name}_coverage" / "default.nix": (
            _current_python_coverage_source()
        )
        for package in packages
        if package.kind == "python"
        and (package.root / "test_main.py").is_file()
        and has_python_tests(package.root / "test_main.py")
    }
    hosts = root / "hosts"
    if hosts.is_dir():
        for host in sorted(hosts.iterdir()):
            check = Path("checks") / f"{host.name}VmWithDisko" / "default.nix"
            if (
                host.is_dir()
                and not host.is_symlink()
                and (host / "configuration.nix").is_file()
            ):
                validate_host_name(host.name)
                checks[check] = _current_host_check_source()
    return checks


def allowed_paths(root: Path, packages: list[Package]) -> set[Path]:
    """Compute the repository whitelist represented by .gitignore."""
    allowed = {Path(item) for item in ROOT_FILES if (root / item).exists()}
    allowed.update(canonical_checks(root, packages))
    hosts = root / "hosts"
    if hosts.is_dir():
        for host in hosts.iterdir():
            if host.is_dir() and (host / "configuration.nix").exists():
                allowed.add(Path("hosts") / host.name / "configuration.nix")
                hardware = Path("hosts") / host.name / "hardware-configuration.nix"
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


def _refresh_gitignore(root: Path) -> None:
    """Refresh the whitelist after changing repository resources."""
    packages = detect_packages(root)
    nix_syntax.write_if_changed(
        root / ".gitignore",
        render_gitignore(allowed_paths(root, packages), opaque_trees(root)),
    )


def inspect_structure(root: Path) -> tuple[list[Package], list[str]]:
    """Validate the declared repository subset."""
    packages = detect_packages(root)
    allowed = allowed_paths(root, packages)
    issues: list[str] = []
    for package in packages:
        validate_name(package.name)
        if issue := _python_test_placement_issue(package):
            issues.append(issue)
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


def _python_test_placement_issue(package: Package) -> str | None:
    """Reject embedded tests before convergence can remove their old checks."""
    source = package.root / "main.py"
    if package.kind == "python" and source.is_file() and has_python_tests(source):
        return f"packages/{package.name}/main.py: move test definitions to test_main.py"
    return None


def has_python_tests(path: Path) -> bool:
    """Detect pytest-style tests without executing package source."""
    try:
        module = ast.parse(path.read_text(encoding="utf-8"), filename=str(path))
    except (OSError, SyntaxError, UnicodeError) as error:
        msg = f"{path}: Python source could not be parsed: {error}"
        raise CommandError(
            msg,
        ) from error
    for node in module.body:
        if isinstance(
            node,
            (ast.FunctionDef, ast.AsyncFunctionDef),
        ) and node.name.startswith("test_"):
            return True
        if (
            isinstance(node, ast.ClassDef)
            and node.name.startswith("Test")
            and any(
                isinstance(child, (ast.FunctionDef, ast.AsyncFunctionDef))
                and child.name.startswith("test_")
                for child in node.body
            )
        ):
            return True
    return False


def package_description(package: Package) -> str | None:
    """Extract declared package metadata where supported."""
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
    document, expression = _metadata_expression(source, "meta", ("description",))
    if expression is None or expression.type != "string_expression":
        return None
    if any(node.type == "interpolation" for node in nix_syntax.walk(expression)):
        return None
    decoded = json.loads(document.text(expression).replace(r"\${", "${"))
    return cast("str", decoded)


def _nix_string(value: str) -> str:
    """Encode a non-interpolating Nix string literal."""
    return json.dumps(value).replace("${", r"\${")


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


def _metadata_expression(
    source: str,
    namespace: str,
    requested_path: tuple[str, ...],
) -> tuple[nix_syntax.Document, Node | None]:
    """Find an unambiguous metadata expression in dotted or nested form."""
    document = nix_syntax.parse(source)
    matches = []
    for binding in (
        node for node in nix_syntax.walk(document.root) if node.type == "binding"
    ):
        attrpath = nix_syntax.field(binding, "attrpath")
        expression = nix_syntax.field(binding, "expression")
        if attrpath is None or expression is None:
            continue
        binding_path = nix_syntax.static_attrpath(document, attrpath)
        if binding_path == (namespace, *requested_path):
            matches.append(expression)
        elif binding_path == (namespace,):
            matches.extend(
                _attrset_expression(document, expression, requested_path),
            )
    unique = {node.start_byte: node for node in matches}
    return document, next(iter(unique.values())) if len(unique) == 1 else None


def _check_coverage_default(root: Path, package: Package) -> None:
    """Ensure generated Python coverage checks retain their static definition."""
    check = root / "checks" / f"{package.name}_coverage" / "default.nix"
    if not check.is_file():
        return
    actual = _read_regular(check)
    if not (actual is not None):
        raise AssertionError
    expected = _current_python_coverage_source()
    if nix_syntax.compact(actual) != nix_syntax.compact(expected):
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
  dependencyInputs = pkgs.lib.concatMap (name: packageDrv.${name} or [ ]) [
    "buildInputs"
    "checkInputs"
    "nativeBuildInputs"
    "nativeCheckInputs"
    "propagatedBuildInputs"
    "propagatedNativeBuildInputs"
  ];
  packageDrv = inputs.self.packages.${pkgs.stdenv.system}.${packageName};
  packageName = pkgs.lib.removeSuffix "_coverage" checkName;
  pythonEnv = packageDrv.python.withPackages (
    ps:
    packageDrv.propagatedBuildInputs
    ++ [
      ps.hypothesis
      ps.pytest
      ps.pytest-cov
    ]
  );
in
pkgs.runCommand checkName
  {
    inherit (packageDrv) src;
    nativeBuildInputs = dependencyInputs ++ [ pythonEnv ];
  }
  ''
    export HOME="$(mktemp -d)"
    mkdir -p "$out/html" packages
    ln -s "$src" "packages/${packageName}"
    export PYTHONPATH="$PWD:$PYTHONPATH"
    cd "$out"
    PACKAGE_E2E_EXECUTABLE="${pkgs.lib.getExe packageDrv}" python -c 'import sys; from hypothesis import Phase, settings; settings.register_profile("coverage", phases=[Phase.explicit]); settings.load_profile("coverage"); import pytest; sys.exit(pytest.main(sys.argv[1:]))' -p no:cacheprovider --import-mode=importlib --cov="packages.${packageName}.main" --cov-report "html:$out/html" "$src/test_main.py"
  ''
"""  # noqa: E501


def _current_host_check_source() -> str:
    """Render the canonical host VM check definition."""
    return """{ inputs, pkgs, ... }:
let
  configuration = inputs.self.nixosConfigurations.${host};
  diskoDevices = configuration.config.disko.devices or { };
  host = pkgs.lib.removeSuffix "VmWithDisko" (baseNameOf ./.);
  vm =
    if builtins.attrNames diskoDevices == [ ] then
      configuration.config.system.build.vm
    else
      configuration.config.system.build.vmWithDisko;
in
pkgs.runCommand (baseNameOf ./.)
  {
    buildInputs = [ vm ];
  }
  ''
    touch "$out"
  ''
"""


def _python_static_template_issues(package: Package, source: str) -> list[str]:
    """Check only the stable interface required by Python package templates."""
    return [
        f"missing required {name} definition"
        for name, edits in _python_required_edits(package, source).items()
        if edits
    ]


def _binding_value(source: str, name: str, kind: str) -> str | None:
    """Extract one permitted template binding expression."""
    escaped = re.escape(name)
    patterns = {
        "list": rf"(?s)(?<![\w.]){escaped}\s*=\s*(\[.*?\])\s*;",
        "string": (
            rf"(?s)(?<![\w.]){escaped}\s*=\s*"
            r"""((?:"(?:\\.|[^"\\])*"|''.*?''))\s*;"""
        ),
    }
    match = re.search(patterns[kind], source)
    return match.group(1) if match else None


def _replace_binding(source: str, name: str, value: str) -> str:
    """Replace one binding expression in a generated template."""
    escaped = re.escape(name)
    return re.sub(
        rf"(?s)((?<![\w.]){escaped}\s*=\s*)(?:\[.*?\]|\"(?:\\.|[^\"\\])*\"|''.*?'')(\s*;)",
        lambda match: match.group(1) + value + match.group(2),
        source,
        count=1,
    )


def _nix_binding_edits(
    document: nix_syntax.Document,
    container: Node,
    path: tuple[str, ...],
    value: str,
) -> list[tuple[int, int, bytes]]:
    """Set a scoped binding, retaining unrelated fields and inherited names."""
    while container.type == "parenthesized_expression" or (
        container.type == "binary_expression"
        and document.text(nix_syntax.field(container, "operator")) == "//"
    ):
        container = nix_syntax.field(
            container,
            "expression" if container.type == "parenthesized_expression" else "right",
        )
    assignment = f"{'.'.join(path)} = {value};"
    if container.type not in {
        "attrset_expression",
        "rec_attrset_expression",
        "let_expression",
    }:
        replacement = f"({document.text(container)}) // {{ {assignment} }}"
        return [(container.start_byte, container.end_byte, replacement.encode())]
    bindings = next(
        (node for node in container.named_children if node.type == "binding_set"),
        None,
    )
    children = [] if bindings is None else bindings.named_children
    for binding in children:
        attrpath = nix_syntax.field(binding, "attrpath")
        expression = nix_syntax.field(binding, "expression")
        if attrpath is not None and expression is not None:
            names = nix_syntax.static_attrpath(document, attrpath)
            if names == path:
                if nix_syntax.compact(document.text(expression)) == nix_syntax.compact(
                    value,
                ):
                    return []
                return [(expression.start_byte, expression.end_byte, value.encode())]
            if names and path[: len(names)] == names:
                return _nix_binding_edits(
                    document,
                    expression,
                    path[len(names) :],
                    value,
                )
    inherited = [
        (binding, attr)
        for binding in children
        if (attrs := nix_syntax.field(binding, "attrs")) is not None
        for attr in attrs.named_children
        if document.text(attr) == path[0]
    ]
    if (
        len(path) == 1
        and value == path[0]
        and any(binding.type == "inherit" for binding, _ in inherited)
    ):
        return []
    edits = [(attr.start_byte, attr.end_byte, b"") for _, attr in inherited]
    offset = (
        container.start_byte + 3
        if container.type == "let_expression"
        else container.end_byte - 1
    )
    edits.append((offset, offset, f"\n  {assignment}\n".encode()))
    return edits


def _python_required_edits(
    package: Package,
    source: str,
) -> dict[str, list[tuple[int, int, bytes]]]:
    """Derive validation and repair from the same scoped Python requirements."""
    document = nix_syntax.parse(source)
    body = document.root
    scope = None
    while body.type in {
        "function_expression",
        "let_expression",
        "parenthesized_expression",
    }:
        if body.type == "let_expression":
            scope = body
        body = nix_syntax.field(
            body,
            "expression" if body.type == "parenthesized_expression" else "body",
        )
    argument = nix_syntax.field(body, "argument")
    function = nix_syntax.field(body, "function")
    if (
        body.type != "apply_expression"
        or function is None
        or nix_syntax.compact(document.text(function))
        != "python.pkgs.buildPythonPackage"
        or argument is None
        or argument.type not in {"attrset_expression", "rec_attrset_expression"}
    ):
        msg = (
            "Python package must call python.pkgs.buildPythonPackage "
            "with an attribute set"
        )
        raise CommandError(msg)
    template = scaffold("python", package.name, None)[
        Path("packages") / package.name / "default.nix"
    ]
    install_phase = _binding_value(template, "installPhase", "string")
    if install_phase is None:
        msg = "Python scaffold omitted its install phase"
        raise AssertionError(msg)
    required = {
        "pname": "pname",
        "installPhase": install_phase,
        "meta.mainProgram": "baseNameOf ./." if "-" in package.name else "pname",
        "passthru.python": "python",
        "pyproject": "false",
        "src": "./.",
        "strictDeps": "true",
    }
    edits = {
        name: _nix_binding_edits(document, argument, tuple(name.split(".")), value)
        for name, value in required.items()
    }
    pname = (
        'builtins.replaceStrings [ "-" ] [ "_" ] (baseNameOf ./.)'
        if "-" in package.name
        else "baseNameOf ./."
    )
    if scope is None:
        edits["Python let bindings"] = [
            (
                body.start_byte,
                body.start_byte,
                f"let pname = {pname}; python = pkgs.python3; in ".encode(),
            ),
        ]
    else:
        edits["local pname"] = _nix_binding_edits(
            document,
            scope,
            ("pname",),
            pname,
        )
        bindings = next(
            (node for node in scope.named_children if node.type == "binding_set"),
            None,
        )
        has_python = any(
            (
                (path := nix_syntax.field(binding, "attrpath")) is not None
                and nix_syntax.static_attrpath(document, path) == ("python",)
            )
            or (
                (attrs := nix_syntax.field(binding, "attrs")) is not None
                and any(
                    document.text(attr) == "python" for attr in attrs.named_children
                )
            )
            for binding in ([] if bindings is None else bindings.named_children)
        )
        if not has_python:
            edits["local python"] = _nix_binding_edits(
                document,
                scope,
                ("python",),
                "pkgs.python3",
            )
    return edits


def _canonical_python_default(package: Package, source: str) -> str:
    """Repair required Python bindings without removing custom attributes."""
    edits = _python_required_edits(package, source)
    return cast(
        "bytes",
        nix_syntax.apply_edits(
            source.encode(),
            [edit for changes in edits.values() for edit in changes],
        ),
    ).decode()


def canonical_typed_default(package: Package) -> str | None:
    """Render a typed definition while retaining package-specific fields."""
    if package.kind == "nix":
        return None
    source = _read_regular(package.root / "default.nix")
    if source is None:
        rendered = scaffold(package.kind, package.name, None)[
            Path("packages") / package.name / "default.nix"
        ]
        return (
            _canonical_python_default(package, rendered)
            if package.kind == "python"
            else rendered
        )
    nix_syntax.parse(source, str(package.root / "default.nix"))
    if package.kind == "python":
        return _canonical_python_default(package, source)
    description = package_description(package)
    rendered = scaffold(package.kind, package.name, description)[
        Path("packages") / package.name / "default.nix"
    ]
    fields = {
        "html": (("runtimeDeps", "list"),),
        "latex": (("nativeDeps", "list"),),
    }[package.kind]
    for name, kind in fields:
        value = _binding_value(source, name, kind)
        if value is not None:
            rendered = _replace_binding(rendered, name, value)
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
    if current is not None and nix_syntax.compact(current) == nix_syntax.compact(
        source,
    ):
        source = current
    return _write_managed(root, relative, source, dry_run=dry_run)


def _converge_packages(root: Path, packages: list[Package], dry_run: bool) -> bool:  # noqa: FBT001
    """Converge package templates and required source files."""
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
    return changed


def _converge_checks(root: Path, packages: list[Package], dry_run: bool) -> bool:  # noqa: FBT001
    """Generate every check derived from a canonical package or host."""
    changed = False
    for relative, source in canonical_checks(root, packages).items():
        changed |= _write_managed_nix(root, relative, source, dry_run=dry_run)
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


def _converge_opaque_files(
    root: Path,
    opaque: set[Path],
    tracked: set[Path],
    *,
    dry_run: bool,
) -> tuple[bool, set[Path]]:
    """Stage unmanaged files below opaque trees without changing their modes."""
    files = {
        path.relative_to(root)
        for tree in opaque
        for path in (root / tree).rglob("*")
        if path.is_file() or path.is_symlink()
    }
    changed = False
    for relative in sorted(files - tracked):
        _change(f"stage '{relative}'", dry_run=dry_run)
        changed = True
        if not dry_run:
            git(root, ["add", "--", str(relative)])
            tracked.add(relative)
    return changed, files


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


def _cleanup_flake(root: Path, packages: list[Package], dry_run: bool) -> bool:  # noqa: FBT001
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
    opaque_changed, opaque_files = _converge_opaque_files(
        root,
        opaque,
        tracked,
        dry_run=dry_run,
    )
    changed |= opaque_changed
    changed |= _remove_unsupported_tracked(
        root,
        tracked,
        scratch,
        opaque | scratch | allowed,
        dry_run=dry_run,
    )
    clean_arguments = _flake_clean_arguments(dry_run=dry_run)
    if dry_run:
        for relative in sorted(allowed | opaque_files):
            if (root / relative).exists():
                clean_arguments.extend(("-e", f"/{relative.as_posix()}"))
    clean = git(root, clean_arguments, check=False)
    if clean.returncode != 0:
        raise CommandError(clean.stderr.strip() or "git clean failed")
    if clean.stdout:
        print(clean.stdout, end="")  # noqa: T201
        changed = True
    return changed


def check_flake(root: Path, dry_run: bool) -> list[Package]:  # noqa: FBT001
    """Converge required files, structure, templates, and root whitelist."""
    missing = [
        name
        for name in (".gitignore", "flake.nix", "flake.lock")
        if not (root / name).is_file()
    ]
    if missing:
        raise CommandError("missing required file: " + missing[0])
    packages = detect_packages(root)
    for package in packages:
        if issue := _python_test_placement_issue(package):
            raise CommandError(issue)
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
    changed |= _converge_checks(root, packages, dry_run)
    changed |= _cleanup_flake(root, packages, dry_run)
    if dry_run and changed:
        msg = "flake repository would change"
        raise CommandError(msg)
    return validate_flake_source(root)


def validate_flake_source(root: Path) -> list[Package]:  # noqa: C901
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
    issues.extend(_generated_check_issues(root, packages))
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


def _generated_check_issues(root: Path, packages: list[Package]) -> list[str]:
    """Return missing or noncanonical generated-check issues."""
    issues = []
    for check, expected in canonical_checks(root, packages).items():
        actual = _read_regular(root / check)
        if actual is None:
            issues.append(f"{check}: missing generated check")
        elif nix_syntax.compact(actual) != nix_syntax.compact(expected):
            issues.append(f"{check}: differs from its canonical generated template")
    return issues


def _source_package_issues(root: Path, package: Package) -> list[str]:
    """Return source-only typed-template and generated-check issues."""
    issues: list[str] = []
    relative = Path("packages") / package.name / "default.nix"
    actual = _read_regular(root / relative)
    expected = canonical_typed_default(package)
    if (
        expected is not None
        and actual is not None
        and nix_syntax.compact(actual) != nix_syntax.compact(expected)
    ):
        issues.append(f"{relative}: differs from its canonical typed template")
    if package.kind == "python" and actual is not None:
        issues.extend(
            f"{relative}: {issue}"
            for issue in _python_static_template_issues(package, actual)
        )
    return issues


def scaffold(
    kind: str,
    name: str,
    description: str | None,
) -> dict[Path, str]:
    """Render one supported package."""
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
        "python": """{ pkgs, ... }:
let
  pname = baseNameOf ./.;
  python = pkgs.python3;
in
python.pkgs.buildPythonPackage {
  inherit pname;
  installPhase = ''
    install -Dm644 main.py "$out/${python.sitePackages}/$pname/__init__.py"
    mkdir -p "$out/bin"
    printf '%s\\n' '#!${python.interpreter}' "from $pname import main" 'main()' > "$out/bin/$pname"
    chmod 755 "$out/bin/$pname"
    if [ -d prm ]; then
      cp -R prm/ "$out/${python.sitePackages}/$pname/"
    fi
  '';
  meta = {
    description = __DESCRIPTION__;
    mainProgram = pname;
  };
  passthru.python = python;
  propagatedBuildInputs = [ ];
  pyproject = false;
  src = ./.;
  strictDeps = true;
  version = "0.0.0";
}
""",  # noqa: E501
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
    if "-" in name:
        default = default.replace(
            "baseNameOf ./.",
            'builtins.replaceStrings [ "-" ] [ "_" ] (baseNameOf ./.)',
        )
        if kind == "python":
            default = default.replace("$out/bin/$pname", "$out/bin/${baseNameOf ./.}")
            default = default.replace(
                "mainProgram = pname;",
                "mainProgram = baseNameOf ./.;",
            )
    files: dict[Path, str] = {root / "default.nix": default}
    if kind == "python":
        files[root / "main.py"] = (
            f'''#!/usr/bin/env python3\n{description!r}\n\ndef main() -> None:\n    """Run {name}."""\n\n\nif __name__ == "__main__":\n    main()\n'''  # noqa: E501
        )
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
                / "ms.tex": "\\documentclass{article}\n\\begin{document}\n\\end{document}\n",  # noqa: E501
                root / "ms.bib": "",
            },
        )
    return files


def add_package(root: Path, kind: str, name: str, description: str | None) -> None:
    """Create a package transactionally and stage its managed files."""
    if kind not in PACKAGE_KINDS:
        msg = f"unsupported package type: {kind}\nhint: supported package types: {', '.join(PACKAGE_KINDS)}"  # noqa: E501
        raise CommandError(
            msg,
        )
    validate_name(name)
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
        _refresh_gitignore(root)
        generated = [str(path.relative_to(root)) for path in created] + [".gitignore"]
        completed = git(root, ["add", "--force", "--", *generated], check=False)
        if completed.returncode != 0:
            raise CommandError(completed.stderr.strip() or "git add failed")  # noqa: TRY301
    except BaseException:
        for path in reversed(created):
            path.unlink(missing_ok=True)
            with contextlib.suppress(OSError):
                path.parent.rmdir()
        raise


def add_host(root: Path, name: str) -> None:
    """Create a host and stage its canonical configuration."""
    validate_host_name(name)
    relative = Path("hosts") / name / "configuration.nix"
    path = root / relative
    check_relative = Path("checks") / f"{name}VmWithDisko" / "default.nix"
    check = root / check_relative
    if (
        path.parent.exists()
        or path.parent.is_symlink()
        or check.parent.exists()
        or check.parent.is_symlink()
    ):
        msg = f"host or generated check already exists: {name}"
        raise CommandError(msg)
    try:
        path.parent.mkdir(parents=True)
        path.write_text("{ ... }: { }\n", encoding="utf-8")
        check.parent.mkdir(parents=True)
        check.write_text(_current_host_check_source(), encoding="utf-8")
        _refresh_gitignore(root)
        completed = git(
            root,
            [
                "add",
                "--force",
                "--",
                str(relative),
                str(check_relative),
                ".gitignore",
            ],
            check=False,
        )
        if completed.returncode != 0:
            raise CommandError(completed.stderr.strip() or "git add failed")  # noqa: TRY301
    except BaseException:
        path.unlink(missing_ok=True)
        check.unlink(missing_ok=True)
        with contextlib.suppress(OSError):
            path.parent.rmdir()
        with contextlib.suppress(OSError):
            check.parent.rmdir()
        raise


def remove_resource(root: Path, value: str, dry_run: bool) -> None:  # noqa: FBT001
    """Remove a canonical package or host and stage its related metadata."""
    kind, name, relative = _parse_resource_path(value)
    resource_root = root / relative
    marker = "default.nix" if kind == "package" else "configuration.nix"
    if (
        not resource_root.is_dir()
        or resource_root.is_symlink()
        or not (resource_root / marker).is_file()
    ):
        msg = f"{kind} does not exist: {name}"
        raise CommandError(msg)
    check_root = (
        root
        / "checks"
        / (f"{name}_coverage" if kind == "package" else f"{name}VmWithDisko")
    )
    targets = [
        resource_root,
        *([check_root] if check_root.exists() else []),
    ]
    target_relatives = [str(target.relative_to(root)) for target in targets]
    if dry_run:
        for target in targets:
            print(f"rm '{target.relative_to(root)}'")  # noqa: T201
        print("update '.gitignore'")  # noqa: T201
        return
    for target in targets:
        shutil.rmtree(target)
    _refresh_gitignore(root)
    git(
        root,
        [
            "add",
            "--all",
            "--force",
            "--",
            *target_relatives,
            ".gitignore",
        ],
    )


def _parse_resource_path(value: str) -> tuple[str, str, Path]:
    """Parse a canonical package or host resource path."""
    path = Path(value)
    if path.is_absolute() or len(path.parts) != 2:  # noqa: PLR2004
        msg = f"resource path must be packages/NAME or hosts/NAME: {value}"
        raise CommandError(msg)
    parent, name = path.parts
    if parent == "packages":
        validate_name(name)
        return "package", name, path
    if parent == "hosts":
        validate_host_name(name)
        return "host", name, path
    msg = f"resource path must be packages/NAME or hosts/NAME: {value}"
    raise CommandError(msg)


def rename_resource(root: Path, source: str, destination: str, dry_run: bool) -> None:  # noqa: FBT001
    """Rename one canonical package or host and stage its related metadata."""
    source_kind, source_name, source_relative = _parse_resource_path(source)
    destination_kind, _, destination_relative = _parse_resource_path(destination)
    if source_kind != destination_kind:
        msg = "cannot rename a package to a host or a host to a package"
        raise CommandError(msg)
    source_path = root / source_relative
    destination_path = root / destination_relative
    marker = "default.nix" if source_kind == "package" else "configuration.nix"
    if (
        not source_path.is_dir()
        or source_path.is_symlink()
        or not (source_path / marker).is_file()
    ):
        msg = f"{source_kind} does not exist: {source_name}"
        raise CommandError(msg)
    if destination_path.exists() or destination_path.is_symlink():
        msg = f"destination already exists: {destination_relative}"
        raise CommandError(msg)
    moves = [(source_relative, destination_relative)]
    check_suffix = "_coverage" if source_kind == "package" else "VmWithDisko"
    source_check = Path("checks") / f"{source_name}{check_suffix}"
    destination_name = destination_relative.name
    destination_check = Path("checks") / f"{destination_name}{check_suffix}"
    if (root / source_check).exists():
        if (root / destination_check).exists():
            msg = f"destination already exists: {destination_check}"
            raise CommandError(msg)
        moves.append((source_check, destination_check))
    for old, new in moves:
        _change(f"move '{old}' to '{new}'", dry_run=dry_run)
    _change("update '.gitignore'", dry_run=dry_run)
    if dry_run:
        return
    tracked = _tracked_paths(root)
    tracked_sources = [
        old for old, _new in moves if any(beneath(path, {old}) for path in tracked)
    ]
    for old, new in moves:
        shutil.move(root / old, root / new)
    _refresh_gitignore(root)
    git(
        root,
        [
            "add",
            "--all",
            "--",
            *(str(path) for path in tracked_sources),
            *(str(new) for _old, new in moves),
            ".gitignore",
        ],
    )


def initialize_home() -> None:
    """Initialize and stage the canonical home policy without cleaning."""
    root = Path.home()
    if not (root / ".git").exists():
        _run(["git", "init", str(root)])
    if profile(root, "home") != "home":
        msg = "cannot initialize a flake repository as a home repository"
        raise CommandError(msg)
    _converge_home_ignore(root, dry_run=False)


def _remote_is_empty(remote: str) -> bool:
    """Return whether a hosted remote advertises no heads."""
    completed = _run(
        ["git", "ls-remote", remote],
        check=False,
    )
    if completed.returncode != 0:
        raise CommandError(completed.stderr.strip() or "could not read remote")
    return not completed.stdout.strip()


def initialize_flake(remote: str) -> None:
    """Create a canonical flake at its remote-derived home path."""
    relative = canonical_remote_path(remote)
    home = Path.home()
    if repository_root(home) != home or profile(home) != "home":
        msg = "$HOME must be an initialized canonical home repository"
        raise CommandError(msg)
    if not _remote_is_empty(remote):
        msg = "init flake requires an empty remote"
        raise CommandError(msg)
    readme = f"# {relative.name}\n"
    directory = home / relative
    if directory.exists():
        msg = f"target already exists: {directory}"
        raise CommandError(msg)
    directory.parent.mkdir(parents=True, exist_ok=True)
    _run(["git", "clone", remote, str(directory)])
    try:
        flake = directory / "flake.nix"
        flake.write_text(
            '{ inputs.canonical.url = "github:pbizopoulos/canonical"; outputs = inputs: inputs.canonical.blueprint { inherit inputs; }; }\n',  # noqa: E501
            encoding="utf-8",
        )
        (directory / "README").write_text(readme, encoding="utf-8")
        _run(
            [os.environ.get("GIT_CANONICAL_NIX", "nix"), "flake", "lock"],
            cwd=directory,
        )
        detected_packages = detect_packages(directory)
        _converge_checks(directory, detected_packages, False)  # noqa: FBT003
        (directory / ".gitignore").write_text(
            render_gitignore(
                allowed_paths(directory, detected_packages),
                opaque_trees(directory),
            ),
            encoding="utf-8",
        )
        _run(
            [os.environ.get("GIT_CANONICAL_NIX", "nix"), "fmt"],
            cwd=directory,
        )
        git(directory, ["add", "--all"])
        git(directory, ["branch", "-M", "main"])
        git(directory, ["commit", "-m", "Initialize repository"])
        git(directory, ["push", "--set-upstream", "origin", "main"])
    except BaseException:
        shutil.rmtree(directory)
        with contextlib.suppress(OSError):
            directory.parent.rmdir()
        raise
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


def parser() -> argparse.ArgumentParser:
    """Construct the public command-line parser."""
    result = argparse.ArgumentParser(
        prog="git canonical",
        description="Manage canonical persistent state in HOME and flake repositories.",
    )
    commands = result.add_subparsers(
        dest="command",
        required=True,
        title="commands",
        metavar="COMMAND",
    )
    init = commands.add_parser(
        "init",
        help="initialize HOME or a flake repository",
        description="Initialize HOME or a flake repository.",
    )
    init.add_argument(
        "profile",
        choices=("flake", "home"),
        help="repository profile to initialize",
    )
    init.add_argument(
        "remote",
        nargs="?",
        metavar="REMOTE",
        help="empty hosted Git remote required by the flake profile",
    )
    add = commands.add_parser(
        "add",
        help="add a package or host",
        description="Create and stage a canonical package or host.",
    )
    add.add_argument(
        "resource",
        metavar="RESOURCE",
        help="new packages/NAME or hosts/NAME path",
    )
    add.add_argument(
        "type",
        nargs="?",
        metavar="TYPE",
        help=f"package type ({', '.join(PACKAGE_KINDS)})",
    )
    add.add_argument(
        "description",
        nargs="*",
        metavar="DESCRIPTION",
        help="optional package description",
    )
    remove = commands.add_parser(
        "rm",
        help="remove a package or host",
        description="Remove and stage a canonical package or host.",
    )
    remove.add_argument(
        "resource",
        metavar="RESOURCE",
        help="existing packages/NAME or hosts/NAME path",
    )
    remove.add_argument(
        "-n",
        "--dry-run",
        action="store_true",
        help="print removals without changing the repository",
    )
    move = commands.add_parser(
        "mv",
        help="rename a package or host",
        description="Rename a canonical package or host and stage the result.",
    )
    move.add_argument(
        "source",
        metavar="SOURCE",
        help="existing packages/NAME or hosts/NAME path",
    )
    move.add_argument(
        "destination",
        metavar="DESTINATION",
        help="new path in the same resource collection",
    )
    move.add_argument(
        "-n",
        "--dry-run",
        action="store_true",
        help="print moves without changing the repository",
    )
    converge = commands.add_parser(
        "converge",
        help="converge the repository to its canonical layout",
        description="Converge the repository to its canonical layout.",
    )
    converge.add_argument(
        "-n",
        "--dry-run",
        action="store_true",
        help="report required actions without changing the repository",
    )
    converge.add_argument("--source", type=Path, help=argparse.SUPPRESS)
    return result


def _dispatch_init(options: argparse.Namespace) -> bool:
    """Dispatch initialization and report whether it handled the command."""
    if options.command != "init":
        return False
    if options.profile == "home":
        if options.remote is not None:
            msg = "init home does not accept a remote"
            raise CommandError(msg)
        initialize_home()
    else:
        if options.remote is None:
            msg = "init flake requires REMOTE"
            raise CommandError(msg)
        initialize_flake(options.remote)
    return True


def _normalize_help_arguments(arguments: list[str]) -> list[str]:
    """Translate the help convenience command to argparse's help option."""
    if arguments[:1] == ["help"]:
        return [arguments[1], "--help"] if len(arguments) > 1 else ["--help"]
    return arguments


def _dispatch_add(root: Path, options: argparse.Namespace) -> None:
    """Create the selected package or host resource."""
    kind, name, _relative = _parse_resource_path(options.resource)
    description = " ".join(options.description) or None
    if kind == "host":
        if options.type is not None or description is not None:
            msg = "host creation does not accept a type or description"
            raise CommandError(msg)
        add_host(root, name)
        return
    if options.type is None:
        msg = "package creation requires TYPE"
        raise CommandError(msg)
    add_package(root, options.type, name, description)


def main() -> None:
    """Dispatch the git canonical CLI."""
    arguments = _normalize_help_arguments(sys.argv[1:])
    try:
        options = parser().parse_args(arguments)
        if _dispatch_init(options):
            return
        if options.command == "converge" and options.source is not None:
            validate_flake_source(options.source.resolve())
            return
        root = repository_root()
        current_profile = profile(root)
        if options.command in {"add", "mv", "rm"} and current_profile != "flake":
            msg = f"{current_profile} repositories do not support flake resources"
            raise CommandError(  # noqa: TRY301
                msg,
            )
        if options.command == "converge":
            check_home(
                root,
                options.dry_run,
            ) if current_profile == "home" else check_flake(
                root,
                options.dry_run,
            )
        elif options.command == "add":
            _dispatch_add(root, options)
        elif options.command == "rm":
            remove_resource(root, options.resource, options.dry_run)
        elif options.command == "mv":
            rename_resource(
                root,
                options.source,
                options.destination,
                options.dry_run,
            )
    except (
        CommandError,
        OSError,
        UnicodeError,
        json.JSONDecodeError,
        nix_syntax.NixSyntaxError,
    ) as error:
        print(f"error: {error}", file=sys.stderr)  # noqa: T201
        raise SystemExit(1) from error


if __name__ == "__main__":
    main()
