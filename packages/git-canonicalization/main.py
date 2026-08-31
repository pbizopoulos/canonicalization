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
from typing import Any
from urllib.parse import urlparse

import nix_syntax
import tomllib

PACKAGE_KINDS = ("python", "html", "opentofu", "latex")
KIND_MARKERS = {
    "python": ("main.py",),
    "html": ("index.html",),
    "opentofu": ("main.tf",),
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
    "secrets/secrets.age",
    "secrets/secrets.env.example",
    "secrets/secrets.nix",
}
OPAQUE = {"prm"}


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


def check_home(root: Path, fix: bool) -> list[dict[str, str]]:
    """Check or repair a canonical home repository."""
    gitignore_path = root / ".gitignore"
    source = _read_regular(gitignore_path)
    required = ["!/.gitignore", "!/.gitmodules"]
    if source is None:
        if not fix:
            msg = f"{gitignore_path}: is missing"
            raise CommandError(msg)
        source = "*\n" + "\n".join(required) + "\n"
        gitignore_path.write_text(source, encoding="utf-8")
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
    if missing and not fix:
        msg = f"{gitignore_path}: must whitelist .gitignore and .gitmodules"
        raise CommandError(
            msg,
        )
    if missing:
        gitignore_path.write_text(
            source.rstrip("\n") + "\n" + "\n".join(missing) + "\n",
            encoding="utf-8",
        )
    repositories = home_repositories(root)
    expected_seen: set[Path] = set()
    actual_seen: set[Path] = set()
    for repository in repositories:
        actual = Path(repository["path"])
        expected = canonical_remote_path(repository["url"])
        if expected in expected_seen:
            msg = f"duplicate canonical repository path: {expected}"
            raise CommandError(msg)
        if actual in actual_seen:
            msg = f"duplicate configured repository path: {actual}"
            raise CommandError(msg)
        expected_seen.add(expected)
        actual_seen.add(actual)
        if actual != expected:
            if not fix:
                msg = f"submodule \"{repository['name']}\": path '{actual}' does not match URL; expected '{expected}'"
                raise CommandError(
                    msg,
                )
            (root / expected).parent.mkdir(parents=True, exist_ok=True)
            run(["git", "-C", str(root), "mv", "--", str(actual), str(expected)])
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
            Package(
                package_root.name,
                matches[0] if matches else "other",
                package_root,
            ),
        )
    return result


def validate_name(kind: str, name: str) -> None:
    """Enforce package naming conventions."""
    if name in {"git-canonicalization", "nix-alphabetize", "nix-remove-defaults"}:
        return
    separator = KIND_SEPARATOR[kind]
    if not re.fullmatch(rf"[a-z0-9]+(?:{re.escape(separator)}[a-z0-9]+)*", name):
        msg = f"package name must use snake_case: {name}"
        raise CommandError(msg)


def package_files(package: Package) -> set[Path]:
    """Return allowed regular files for a package kind."""
    relative = Path("packages") / package.name
    kind_files = {
        "python": {"main.py"},
        "html": {"index.html", "script.js", "style.css"},
        "opentofu": {"main.tf", ".terraform.lock.hcl"},
        "latex": {"ms.tex", "ms.bib"},
        "other": set(),
    }[package.kind]
    return {relative / "default.nix", *(relative / item for item in kind_files)}


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
    for parent, names in (("hosts", ("prm",)), ("packages", ("prm", "figures"))):
        base = root / parent
        if base.is_dir():
            for child in base.iterdir():
                if child.is_dir():
                    candidates.update(
                        Path(parent) / child.name / name for name in names
                    )
    return {path for path in candidates if (root / path).is_dir()}


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
        if not (package.root / "default.nix").is_file():
            issues.append(f"packages/{package.name}: missing required file default.nix")
        if package.kind != "other":
            validate_name(package.kind, package.name)
    for path in sorted(root.rglob("*")):
        relative = path.relative_to(root)
        if relative.parts[0] == ".git" or any(
            part in OPAQUE or part in {"tmp", "result"} for part in relative.parts
        ):
            continue
        if path.is_symlink():
            issues.append(
                f"{relative}: expected regular file or directory, found symbolic link",
            )
        elif path.is_file() and relative not in allowed:
            issues.append(f"{relative}: is not allowed")
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
    default = _read_regular(package.root / "default.nix") or ""
    match = re.search(r'description\s*=\s*"([^"\\]*(?:\\.[^"\\]*)*)"', default)
    return (
        None
        if match is None
        else bytes(match.group(1), "utf-8").decode("unicode_escape")
    )


def _compact_nix(source: str) -> str:
    """Normalize insignificant whitespace for template comparisons."""
    return " ".join(source.split())


def _without_dependency_lists(source: str) -> str:
    """Replace native and Python dependency bindings with empty lists."""
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
    return source


def _check_python_default(package: Package) -> None:
    """Ensure generated Python package definitions only customize dependencies."""
    actual = _read_regular(package.root / "default.nix")
    if actual is None:
        return
    expected = scaffold("python", package.name, None)[
        Path("packages") / package.name / "default.nix"
    ]
    if _compact_nix(_without_dependency_lists(actual)) != _compact_nix(expected):
        msg = (
            f"packages/{package.name}/default.nix: differs from the canonical "
            "Python package template outside pythonDeps"
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
    nativeBuildInputs = packageDrv.nativeBuildInputs ++ [ pythonEnv ];
    src = ../.. + "/packages/${packageName}";
  }
  ''
    export HOME="$(mktemp -d)"
    mkdir -p "$out/html"
    cd "$out"
    PACKAGE_E2E_EXECUTABLE="${packageDrv}/bin/${packageName}" python -m pytest -p no:cacheprovider --cov="$src" --cov-report "html:$out/html" "$src/main.py"
  ''
"""


def check_flake(root: Path, fix: bool) -> list[Package]:
    """Check required files, structure, templates, and root whitelist."""
    missing = [
        name
        for name in (".gitignore", "flake.nix", "flake.lock")
        if not (root / name).is_file()
    ]
    if missing:
        raise CommandError("missing required file: " + missing[0])
    packages, issues = inspect_structure(root)
    if issues:
        raise CommandError("directory structure: " + "\nerror: ".join(issues))
    expected = render_gitignore(allowed_paths(root, packages), opaque_trees(root))
    actual = _read_regular(root / ".gitignore")
    if actual != expected:
        if not fix:
            msg = ".gitignore: differs from repository structure policy"
            raise CommandError(msg)
        nix_syntax.write_if_changed(root / ".gitignore", expected)
    for package in packages:
        default = package.root / "default.nix"
        if default.is_file():
            nix_syntax.parse(default.read_bytes(), str(default))
        if package.kind == "python":
            _check_python_default(package)
            _check_coverage_default(root, package)
    checks_root = root / "checks"
    if checks_root.is_dir():
        for check in checks_root.iterdir():
            default = check / "default.nix"
            if default.is_file():
                nix_syntax.parse(default.read_bytes(), str(default))
    return packages


def scaffold(kind: str, name: str, description: str | None) -> dict[Path, str]:
    """Render one supported package and its optional coverage check."""
    description = (
        description
        or {
            "python": "A Python package.",
            "html": "An HTML package.",
            "opentofu": "An OpenTofu package.",
            "latex": "A LaTeX package.",
        }[kind]
    )
    root = Path("packages") / name
    default = """{ inputs, pkgs, ... }:
let
  moduleName = builtins.replaceStrings [ "-" ] [ "_" ] pname;
  nativeDeps = [ ];
  pname = baseNameOf ./.;
  python = pkgs.python3;
  pythonDeps = [ ];
in
python.pkgs.buildPythonPackage {
  inherit pname;
  installPhase = ''
    install -Dm644 main.py "$out/${python.sitePackages}/${moduleName}.py"
    install -Dm755 main.py "$out/bin/$pname"
    if [ -d prm ]; then
      cp -R prm/ "$out/${python.sitePackages}/"
      cp -R prm/ "$out/bin/"
    fi
  '';
  meta.mainProgram = pname;
  nativeBuildInputs = nativeDeps;
  passthru.python = python;
  propagatedBuildInputs = pythonDeps;
  pyproject = false;
  src = inputs.self + "/packages/${pname}";
  strictDeps = true;
  version = "0.0.0";
}
"""
    files: dict[Path, str] = {root / "default.nix": default}
    if kind == "python":
        files[root / "main.py"] = (
            f'''#!/usr/bin/env python3\n"""{description}"""\n\ndef main() -> None:\n    """Run {name}."""\n\nif __name__ == "__main__":\n    main()\n'''
        )
        check = Path("checks") / f"{name}_coverage" / "default.nix"
        files[check] = _python_coverage_source(name)
    elif kind == "html":
        files.update(
            {
                root / "index.html": "<!doctype html><html><body></body></html>\n",
                root / "script.js": "",
                root / "style.css": "",
            },
        )
    elif kind == "opentofu":
        files[root / "main.tf"] = 'terraform { required_version = ">= 1.0" }\n'
    elif kind == "latex":
        files.update(
            {
                root
                / "ms.tex": "\\documentclass{article}\n\\begin{document}\n\\end{document}\n",
                root / "ms.bib": "",
            },
        )
    return files


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


def remove_package(root: Path, name: str, dry_run: bool, force: bool) -> None:
    """Remove a package and generated coverage check safely."""
    package_root = root / "packages" / name
    if not package_root.is_dir() or package_root.is_symlink():
        msg = f"package does not exist: {name}"
        raise CommandError(msg)
    check_root = root / "checks" / f"{name}_coverage"
    targets = [package_root, *([check_root] if check_root.exists() else [])]
    if not force:
        completed = git(
            root,
            [
                "status",
                "--porcelain",
                "--",
                *(str(path.relative_to(root)) for path in targets),
            ],
            check=False,
        )
        if completed.stdout:
            msg = f"package contains local changes: {name}; use --force to remove"
            raise CommandError(
                msg,
            )
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


def initialize(directory: Path, status_path: str | None) -> None:
    """Initialize a canonical flake repository and optional status resources."""
    directory = directory.absolute()
    directory.mkdir(parents=True, exist_ok=True)
    if not (directory / ".git").exists():
        run(["git", "init", str(directory)], quiet=True)
    if profile(directory, "flake") != "flake":
        msg = "cannot initialize a home repository as a flake repository"
        raise CommandError(msg)
    flake = directory / "flake.nix"
    if not flake.exists():
        flake.write_text(
            '{ inputs.canonicalization.url = "github:pbizopoulos/canonicalization"; outputs = inputs: inputs.canonicalization.blueprint { inherit inputs; }; }\n',
            encoding="utf-8",
        )
    if not (directory / "README").exists():
        (directory / "README").write_text(f"# {directory.name}\n", encoding="utf-8")
    if status_path is not None:
        source = (
            sys.stdin.read()
            if status_path == "-"
            else Path(status_path).read_text(encoding="utf-8")
        )
        imported = json.loads(source)
        for item in imported.get("packages", []):
            kind = item["type"]
            if kind not in PACKAGE_KINDS:
                msg = f"unsupported package type: {kind}"
                raise CommandError(msg)
        for item in imported.get("packages", []):
            add_package(directory, item["type"], item["name"], item.get("description"))
        for host in imported.get("hosts", []):
            path = directory / "hosts" / host / "configuration.nix"
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text("{ ... }: { }\n", encoding="utf-8")
    run(
        [os.environ.get("GIT_CANONICALIZATION_NIX", "nix"), "flake", "lock"],
        cwd=directory,
        quiet=True,
    )
    packages = detect_packages(directory)
    gitignore = directory / ".gitignore"
    if not gitignore.exists():
        gitignore.write_text(
            render_gitignore(
                allowed_paths(directory, packages),
                opaque_trees(directory),
            ),
            encoding="utf-8",
        )


def status(root: Path) -> dict[str, Any]:
    """Build the stable repository status payload."""
    current_profile = profile(root)
    if current_profile == "home":
        msg = "home repositories are not compatible with status"
        raise CommandError(msg)
    packages = check_flake(root, False)
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
    init = commands.add_parser("init", help="Initialize a canonical flake repository.")
    init.add_argument("directory", nargs="?", default=".")
    init.add_argument("--from-status", metavar="FILE|-")
    commands.add_parser("status", help="Write repository status as JSON.")
    add = commands.add_parser("add", help="Scaffold a package.")
    add.add_argument("type")
    add.add_argument("name")
    add.add_argument("description", nargs="*")
    remove = commands.add_parser("rm", help="Remove a package and its generated check.")
    remove.add_argument("name")
    remove.add_argument("-n", "--dry-run", action="store_true")
    remove.add_argument("-f", "--force", action="store_true")
    check = commands.add_parser("check", help="Check the selected repository.")
    check.add_argument("--fix", action="store_true")
    return result


def main() -> None:
    """Dispatch the git canonicalization CLI."""
    arguments = sys.argv[1:]
    if arguments[:1] == ["help"]:
        arguments = [arguments[1], "--help"] if len(arguments) > 1 else ["--help"]
    try:
        options = parser().parse_args(arguments)
        if options.command == "init":
            initialize(Path(options.directory), options.from_status)
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
            check_home(root, options.fix) if current_profile == "home" else check_flake(
                root,
                options.fix,
            )
        elif options.command == "add":
            add_package(
                root,
                options.type,
                options.name,
                " ".join(options.description) or None,
            )
        elif options.command == "rm":
            remove_package(root, options.name, options.dry_run, options.force)
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
    """Reject removed Haskell and Python-LaTeX package kinds."""
    assert {"haskell", "python-latex"}.isdisjoint(PACKAGE_KINDS)


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


def test_python_scaffold_installs_optional_prm_resources() -> None:
    """Use one canonical Python package template for optional package resources."""
    files = scaffold("python", "report", None)
    default = files[Path("packages/report/default.nix")]
    assert "if [ -d prm ]; then" in default
    assert 'cp -R prm/ "$out/bin/"' in default
    assert 'builtins.replaceStrings [ "-" ] [ "_" ] pname' in default
    assert "nativeDeps = [ ];" in default
    assert "pythonDeps = [ ];" in default
    assert "<nixpkgs>" not in default
    assert "passthru.python = python;" in default


def test_python_default_allows_only_dependency_customization() -> None:
    """Permit native and Python dependency changes but reject template changes."""
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
        package = Package("report", "python", package_root)
        (package_root / "default.nix").write_text(source, encoding="utf-8")
        _check_python_default(package)
        (package_root / "default.nix").write_text(
            source.replace('version = "0.0.0";', 'version = "1.0.0";'),
            encoding="utf-8",
        )
        try:
            _check_python_default(package)
        except CommandError:
            pass
        else:
            msg = "static Python package template drift was not detected"
            raise AssertionError(msg)


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


if __name__ == "__main__":
    main()
