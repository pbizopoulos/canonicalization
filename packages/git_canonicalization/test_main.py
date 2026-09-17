# Copyright (c) 2026- Paschalis Bizopoulos
"""Tests for git_canonicalization."""

from __future__ import annotations

import ast
import contextlib
import io
import json
import subprocess
import sys
import tempfile
from pathlib import Path

import pytest
from hypothesis import example, given
from hypothesis import strategies as st

from packages.git_canonicalization.main import (
    SCRATCH_NAME,
    CommandError,
    Package,
    _binding_value,
    _canonical_python_default,
    _check_coverage_default,
    _converge_home_checkout,
    _converge_home_ignore,
    _converge_packages,
    _current_host_check_source,
    _current_python_coverage_source,
    _dispatch_add,
    _flake_clean_arguments,
    _home_clean_arguments,
    _meta_description,
    _nix_string,
    _normalize_help_arguments,
    _python_static_template_issues,
    _read_regular,
    _run,
    _source_package_issues,
    _tracked_paths,
    allowed_paths,
    canonical_checks,
    canonical_remote_path,
    canonical_typed_default,
    check_flake,
    detect_packages,
    git,
    has_python_tests,
    inspect_structure,
    opaque_trees,
    package_files,
    parser,
    remove_resource,
    rename_resource,
    render_gitignore,
    required_package_files,
    scaffold,
)


def test_python_package_allows_latex_resources_in_prm() -> None:
    """Classify LaTeX resources under prm as an opaque Python implementation detail."""
    with tempfile.TemporaryDirectory() as temporary_directory:
        root = Path(temporary_directory)
        package = root / "packages" / "report"
        (package / "prm").mkdir(parents=True)
        (package / "default.nix").write_text("{ }: { }\n", encoding="utf-8")
        (package / "main.py").write_text("", encoding="utf-8")
        (package / "prm" / "ms.tex").write_text("", encoding="utf-8")
        if not (detect_packages(root) == [Package("report", "python", package)]):
            raise AssertionError


def test_separate_tests_are_optional_and_derive_coverage() -> None:
    """Convergence preserves and stages optional tests without shipping them."""
    with tempfile.TemporaryDirectory() as temporary_directory:
        root = Path(temporary_directory)
        _temporary_flake(root)
        for relative, source in scaffold("python", "sample", None).items():
            path = root / relative
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(source, encoding="utf-8")
        check_flake(root, False)  # noqa: FBT003
        package = root / "packages" / "sample"
        tests = package / "test_main.py"
        check = root / "checks" / "sample_coverage" / "default.nix"
        if tests.exists() or check.exists():
            raise AssertionError
        tests.write_text("def test_behavior(): pass\n", encoding="utf-8")
        check_flake(root, False)  # noqa: FBT003
        if not check.is_file() or tests.relative_to(root) not in _tracked_paths(root):
            raise AssertionError
        if tests.stat().st_mode & 0o111:
            raise AssertionError
        check_flake(root, True)  # noqa: FBT003
        template = check.read_text(encoding="utf-8")
        for fragment in (
            '--cov="packages.${packageName}.main"',
            "--import-mode=importlib",
            '"$src/test_main.py"',
        ):
            if fragment not in template:
                raise AssertionError
        tests.write_text('"""No tests yet."""\n', encoding="utf-8")
        check_flake(root, False)  # noqa: FBT003
        if check.exists() or not tests.is_file():
            raise AssertionError


def test_embedded_tests_are_rejected_before_cleanup() -> None:
    """Report misplaced tests without deleting files during failed convergence."""
    with tempfile.TemporaryDirectory() as temporary_directory:
        root = Path(temporary_directory)
        _temporary_flake(root)
        for relative, source in scaffold("python", "sample", None).items():
            path = root / relative
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(source, encoding="utf-8")
        main_path = root / "packages" / "sample" / "main.py"
        main_path.write_text("async def test_behavior(): pass\n", encoding="utf-8")
        disposable = root / "artifact"
        disposable.write_text("preserve on failure", encoding="utf-8")
        expected = "packages/sample/main.py: move test definitions to test_main.py"
        if expected not in inspect_structure(root)[1]:
            raise AssertionError
        for dry_run in (True, False):
            with pytest.raises(CommandError) as error:
                check_flake(root, dry_run)
            if str(error.value) != expected:
                raise AssertionError
        if not disposable.exists():
            raise AssertionError


def test_test_detection_distinguishes_code_from_fixture_strings() -> None:
    """Detect synchronous, asynchronous, and class tests without executing input."""
    with tempfile.TemporaryDirectory() as temporary_directory:
        source = Path(temporary_directory) / "test_main.py"
        for contents, expected in (
            ("def test_behavior(): pass\n", True),
            ("async def test_behavior(): pass\n", True),
            ("class TestBehavior:\n    def test_method(self): pass\n", True),
            ('fixture = "def test_behavior(): pass"\n', False),
            ("def main():\n    assert True\n", False),
        ):
            source.write_text(contents, encoding="utf-8")
            if has_python_tests(source) != expected:
                raise AssertionError


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
        if not (detect_packages(root) == [detected]):
            raise AssertionError
        if not (package_files(detected) == {Path("packages/deployment/default.nix")}):
            raise AssertionError


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
        if not (
            required_package_files(package)
            == {
                Path("packages/cv/default.nix"),
                Path("packages/cv/index.html"),
            }
        ):
            raise AssertionError
        if not (inspect_structure(root) == ([package], [])):
            raise AssertionError
        if _converge_packages(root, [package], False):  # noqa: FBT003
            raise AssertionError
        if (package_root / "script.js").exists():
            raise AssertionError
        if (package_root / "style.css").exists():
            raise AssertionError


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
        if not (
            issues
            == [
                (
                    "secrets/secrets.age: unsupported by the canonical flake "
                    "layout; move unrestricted project files under prm/ "
                    "(for example, prm/secrets.age)"
                ),
            ]
        ):
            raise AssertionError


def test_package_named_check_is_not_a_canonical_coverage_check() -> None:
    """Reject legacy checks whose names omit the required coverage suffix."""
    with tempfile.TemporaryDirectory() as temporary_directory:
        root = Path(temporary_directory)
        package = root / "packages" / "report"
        package.mkdir(parents=True)
        (package / "default.nix").write_text("{ }: { }\n", encoding="utf-8")
        (package / "main.py").write_text("", encoding="utf-8")
        check = root / "checks" / "report"
        check.mkdir(parents=True)
        (check / "default.nix").write_text("{ }: { }\n", encoding="utf-8")
        _packages, issues = inspect_structure(root)
        if not (
            issues
            == [
                (
                    "checks/report/default.nix: unsupported by the canonical flake "
                    "layout; move unrestricted project files under prm/ "
                    "(for example, prm/default.nix)"
                ),
            ]
        ):
            raise AssertionError


def test_orphan_coverage_check_is_not_canonical() -> None:
    """Reject coverage checks without a tested Python package owner."""
    with tempfile.TemporaryDirectory() as temporary_directory:
        root = Path(temporary_directory)
        check = root / "checks" / "orphan_coverage"
        check.mkdir(parents=True)
        (check / "default.nix").write_text("{ }: { }\n", encoding="utf-8")
        _packages, issues = inspect_structure(root)
        if not (
            issues
            == [
                (
                    "checks/orphan_coverage/default.nix: unsupported by the canonical "
                    "flake layout; move unrestricted project files under prm/ "
                    "(for example, prm/default.nix)"
                ),
            ]
        ):
            raise AssertionError


def test_standalone_check_is_not_canonical() -> None:
    """Reject checks that cannot be derived from a package or host."""
    with tempfile.TemporaryDirectory() as temporary_directory:
        root = Path(temporary_directory)
        check = root / "checks" / "source_conformance"
        check.mkdir(parents=True)
        (check / "default.nix").write_text("{ }: { }\n", encoding="utf-8")
        _packages, issues = inspect_structure(root)
        if not (
            issues
            == [
                (
                    "checks/source_conformance/default.nix: unsupported by the "
                    "canonical flake layout; move unrestricted project files under "
                    "prm/ (for example, prm/default.nix)"
                ),
            ]
        ):
            raise AssertionError


def test_host_check_falls_back_to_regular_vm() -> None:
    """Use Disko's VM only for hosts that define Disko devices."""
    source = _current_host_check_source()
    if "configuration.config.disko.devices or { };" not in source:
        raise AssertionError
    if "configuration.config.system.build.vm\n" not in source:
        raise AssertionError
    if "configuration.config.system.build.vmWithDisko;" not in source:
        raise AssertionError
    if "buildInputs = [ vm ];" not in source:
        raise AssertionError


def test_host_check_requires_its_host() -> None:
    """Allow the supported VM check only while its host exists."""
    with tempfile.TemporaryDirectory() as temporary_directory:
        root = Path(temporary_directory)
        check = root / "checks" / "demoVmWithDisko" / "default.nix"
        check.parent.mkdir(parents=True)
        check.write_text(_current_host_check_source(), encoding="utf-8")
        _packages, issues = inspect_structure(root)
        if not (len(issues) == 1):
            raise AssertionError
        if str(check.relative_to(root)) not in issues[0]:
            raise AssertionError
        host = root / "hosts" / "demo" / "configuration.nix"
        host.parent.mkdir(parents=True)
        host.write_text("{ ... }: { }\n", encoding="utf-8")
        _packages, issues = inspect_structure(root)
        if not (issues == []):
            raise AssertionError
        check.unlink()
        if not (
            canonical_checks(root, [])
            == {
                Path(
                    "checks/demoVmWithDisko/default.nix",
                ): _current_host_check_source(),
            }
        ):
            raise AssertionError


def test_host_names_use_camel_case() -> None:
    """Reject an existing host resource whose name is not camelCase."""
    with tempfile.TemporaryDirectory() as temporary_directory:
        root = Path(temporary_directory)
        host = root / "hosts" / "install-iso"
        host.mkdir(parents=True)
        (host / "configuration.nix").write_text("{ ... }: { }\n", encoding="utf-8")
        try:
            inspect_structure(root)
        except CommandError as error:
            error_message = str(error)
        else:
            msg = "non-camelCase host name was accepted"
            raise AssertionError(msg)
        if not (error_message == "host name must use camelCase: install-iso"):
            raise AssertionError


def test_python_scaffold_installs_optional_prm_resources() -> None:  # noqa: C901, PLR0912
    """Namespace Python modules and resources so package environments compose."""
    files = scaffold("python", "report", None)
    default = files[Path("packages/report/default.nix")]
    if "if [ -d prm ]; then" not in default:
        raise AssertionError
    if 'cp -R prm/ "$out/${python.sitePackages}/$pname/"' not in default:
        raise AssertionError
    if '"$out/${python.sitePackages}/$pname/__init__.py"' not in default:
        raise AssertionError
    if '"from $pname import main"' not in default:
        raise AssertionError
    if "#!${python.interpreter}" not in default:
        raise AssertionError
    if not ('cp -R prm/ "$out/bin/"' not in default):
        raise AssertionError
    if not ('"$out/${python.sitePackages}/$pname.py"' not in default):
        raise AssertionError
    if "pname = baseNameOf ./.;" not in default:
        raise AssertionError
    if "pyproject = false;" not in default:
        raise AssertionError
    if "src = ./.;" not in default:
        raise AssertionError
    if "strictDeps = true;" not in default:
        raise AssertionError
    if "canonicalization.tests" in default:
        msg = "Python scaffold included test-name metadata"
        raise AssertionError(msg)
    if not ("<nixpkgs>" not in default):
        raise AssertionError
    if "passthru.python = python;" not in default:
        msg = "Python scaffold omitted python from passthru"
        raise AssertionError(msg)
    evaluated = _run(
        [
            "nix",
            "--extra-experimental-features",
            "nix-command",
            "eval",
            "--raw",
            "--expr",
            (
                f"({default}) {{ pkgs.python3.pkgs.buildPythonPackage = "
                "attrs: attrs.meta.mainProgram; }"
            ),
        ],
    )
    if not (evaluated.stdout == Path.cwd().name):
        raise AssertionError


def test_python_scaffold_escapes_arbitrary_description() -> None:
    """Produce parseable Python source for descriptions containing quotes and newlines."""  # noqa: E501
    source = scaffold("python", "report", 'A """ quoted\\ndescription.')[
        Path("packages/report/main.py")
    ]
    module = ast.parse(source)
    if not (ast.get_docstring(module) == 'A """ quoted\\ndescription.'):
        raise AssertionError


def test_meta_description_uses_nix_syntax() -> None:
    """Read metadata and preserve literal interpolation through Nix syntax."""
    nested = '{ meta = { description = "A \\"quoted\\" description."; }; }'
    direct = '{ meta.description = "A direct description."; }'
    if not (_meta_description(nested) == 'A "quoted" description.'):
        raise AssertionError
    if not (_meta_description(direct) == "A direct description."):
        raise AssertionError
    escaped = _nix_string("Literal ${value}.")
    if r"Literal \${value}." not in escaped:
        raise AssertionError
    if not (
        _meta_description(f"{{ meta.description = {escaped}; }}")
        == ("Literal ${value}.")
    ):
        raise AssertionError


def test_python_default_preserves_custom_attributes() -> None:
    """Permit package-specific Nix attributes while retaining static fields."""
    with tempfile.TemporaryDirectory() as temporary_directory:
        root = Path(temporary_directory)
        package_root = root / "packages" / "report"
        package_root.mkdir(parents=True)
        source = scaffold("python", "report", None)[Path("packages/report/default.nix")]
        source = source.replace(
            "  meta = {",
            "  buildInputs = [ pkgs.some_dependency ];\n  meta = {",
        )
        package = Package("report", "python", package_root)
        (package_root / "main.py").write_text("", encoding="utf-8")
        (package_root / "default.nix").write_text(source, encoding="utf-8")
        if not (canonical_typed_default(package) == source):
            raise AssertionError
        if not (_source_package_issues(root, package) == []):
            raise AssertionError


def test_python_default_requires_static_build_fields() -> None:
    """Repair Python definitions that omit one of the canonical build fields."""
    with tempfile.TemporaryDirectory() as temporary_directory:
        root = Path(temporary_directory)
        package_root = root / "packages" / "report"
        package_root.mkdir(parents=True)
        source = scaffold("python", "report", None)[Path("packages/report/default.nix")]
        source = source.replace("  strictDeps = true;\n", "")
        package = Package("report", "python", package_root)
        (package_root / "main.py").write_text("", encoding="utf-8")
        (package_root / "default.nix").write_text(source, encoding="utf-8")
        repaired = canonical_typed_default(package)
        if not (repaired is not None):
            raise AssertionError
        if "strictDeps = true;" not in repaired:
            raise AssertionError
        (package_root / "default.nix").write_text(repaired, encoding="utf-8")
        if not (_source_package_issues(root, package) == []):
            raise AssertionError


def test_python_default_restores_python_passthru() -> None:
    """Restore missing interpreter metadata without losing custom attributes."""
    with tempfile.TemporaryDirectory() as temporary_directory:
        package = Package("report", "python", Path(temporary_directory))
        template = scaffold("python", "report", None)[
            Path("packages/report/default.nix")
        ]
        original = "  passthru.python = python;\n"
        for passthru in (
            "",
            '  passthru = { custom = "kept"; };\n',
            '  passthru.custom = "kept";\n',
            '  passthru = { python = null; custom = "kept"; };\n',
            "  passthru.python = null;\n",
            '  passthru = ( { custom = "kept"; } );\n',
        ):
            source = template.replace(original, passthru)
            if "missing required passthru.python definition" not in (
                _python_static_template_issues(package, source)
            ):
                raise AssertionError
            repaired = _canonical_python_default(package, source)
            if _python_static_template_issues(package, repaired):
                raise AssertionError
            if _canonical_python_default(package, repaired) != repaired:
                raise AssertionError
            if 'custom = "kept";' in source and 'custom = "kept";' not in repaired:
                raise AssertionError
            evaluated = _run(
                [
                    "nix",
                    "--extra-experimental-features",
                    "nix-command",
                    "eval",
                    "--json",
                    "--expr",
                    (
                        'let python = { sitePackages = "site"; interpreter = "python"; '
                        "pkgs.buildPythonPackage = x: x; }; "
                        f"package = ({repaired}) {{ pkgs.python3 = python; }}; "
                        "in package.passthru.python.interpreter"
                    ),
                ],
                package.root,
            )
            if json.loads(evaluated.stdout) != "python":
                raise AssertionError


def test_python_repairs_are_scoped_and_complete() -> None:
    """Repair missing fields in one pass without changing unrelated bindings."""
    with tempfile.TemporaryDirectory() as temporary_directory:
        package = Package("report", "python", Path(temporary_directory))
        template = scaffold("python", "report", None)[
            Path("packages/report/default.nix")
        ]
        install_phase = _binding_value(template, "installPhase", "string")
        custom = (
            '  custom = { src = "kept"; pname = "custom"; '
            'mainProgram = "custom"; installPhase = "custom"; };\n'
        )
        cases = [
            template.replace("  inherit pname;\n", ""),
            template.replace("  python = pkgs.python3;\n", ""),
            template.replace("  pname = baseNameOf ./.;\n", ""),
            template.replace("  src = ./.;\n", "").replace(
                f"  installPhase = {install_phase};\n",
                "",
            ),
            template.replace("  src = ./.;\n", "").replace(
                "  inherit pname;",
                custom + "  inherit pname;",
            ),
            template.replace("    mainProgram = pname;\n", "").replace(
                "  inherit pname;",
                custom + "  inherit pname;",
            ),
            template.replace(
                f"installPhase = {install_phase};",
                "installPhase = null;",
            ),
            template.replace(
                "let\n  pname = baseNameOf ./.;\n  python = pkgs.python3;\nin\n",
                "",
            ),
        ]
        for source in cases:
            if not _python_static_template_issues(package, source):
                raise AssertionError
            repaired = _canonical_python_default(package, source)
            if _python_static_template_issues(package, repaired):
                raise AssertionError
            if _canonical_python_default(package, repaired) != repaired:
                raise AssertionError
            if custom in source and custom not in repaired:
                raise AssertionError
            evaluated = _run(
                [
                    "nix",
                    "--extra-experimental-features",
                    "nix-command",
                    "eval",
                    "--json",
                    "--expr",
                    (
                        'let python = { sitePackages = "site"; interpreter = "python"; '
                        "pkgs.buildPythonPackage = x: x; }; "
                        f"package = ({repaired}) {{ pkgs.python3 = python; }}; "
                        "in package.pname == baseNameOf ./. "
                        "&& package.src == ./. "
                        "&& package.meta.mainProgram == package.pname "
                        '&& package.passthru.python.interpreter == "python" '
                        "&& builtins.isString package.installPhase"
                    ),
                ],
                package.root,
            )
            if json.loads(evaluated.stdout) is not True:
                raise AssertionError


def test_python_repairs_preserve_commented_inherit() -> None:
    """Recognize inherited fields structurally, including intervening comments."""
    with tempfile.TemporaryDirectory() as temporary_directory:
        package = Package("report", "python", Path(temporary_directory))
        source = scaffold("python", "report", None)[Path("packages/report/default.nix")]
        source = source.replace(
            "passthru.python = python;",
            "passthru = { inherit /* interpreter */ python; };",
        ).replace("inherit pname;", "inherit /* package name */ pname;")
        if _python_static_template_issues(package, source):
            raise AssertionError
        if _canonical_python_default(package, source) != source:
            raise AssertionError


def test_coverage_default_matches_current_template() -> None:
    """Recognize the canonical generated coverage check definition."""
    template = _current_python_coverage_source()
    if "dependencyInputs = pkgs.lib.concatMap" not in template:
        raise AssertionError
    if '"nativeCheckInputs"' not in template:
        raise AssertionError
    with tempfile.TemporaryDirectory() as temporary_directory:
        root = Path(temporary_directory)
        check = root / "checks" / "report_coverage"
        check.mkdir(parents=True)
        (check / "default.nix").write_text(
            template,
            encoding="utf-8",
        )
        package = Package("report", "python", root / "report")
        _check_coverage_default(root, package)
        (check / "default.nix").write_text("{ pkgs, ... }: pkgs.emptyFile\n")
        try:
            _check_coverage_default(root, package)
        except CommandError:
            pass
        else:
            msg = "noncanonical coverage definition was accepted"
            raise AssertionError(msg)


def test_remote_paths() -> None:
    """Canonicalize hosted remotes."""
    if not (
        canonical_remote_path("git@github.com:owner/demo.git")
        == Path(
            "github.com/owner/demo",
        )
    ):
        raise AssertionError


def test_home_checkout_converges_origin_and_gitlink() -> None:
    """Use .gitmodules as authority and stage only a published clean HEAD."""
    with tempfile.TemporaryDirectory() as temporary_directory:
        root = Path(temporary_directory)
        checkout = root / "github.com" / "owner" / "demo"
        checkout.mkdir(parents=True)
        git(root, ["init", "--quiet"])
        git(checkout, ["init", "--quiet"])
        git(checkout, ["config", "user.email", "test@example.com"])
        git(checkout, ["config", "user.name", "Test"])
        source = checkout / "README"
        source.write_text("first\n", encoding="utf-8")
        git(checkout, ["add", "README"])
        git(checkout, ["commit", "--quiet", "-m", "first"])
        git(checkout, ["remote", "add", "origin", "git@github.com:owner/demo.git"])
        first = git(checkout, ["rev-parse", "HEAD"]).stdout.strip()
        git(checkout, ["update-ref", "refs/remotes/origin/main", first])
        git(root, ["add", str(checkout.relative_to(root))])
        source.write_text("second\n", encoding="utf-8")
        git(checkout, ["add", "README"])
        git(checkout, ["commit", "--quiet", "-m", "second"])
        second = git(checkout, ["rev-parse", "HEAD"]).stdout.strip()
        git(checkout, ["update-ref", "refs/remotes/origin/main", second])
        expected = Path("github.com/owner/demo")
        (root / ".gitmodules").write_text(
            '[submodule "github.com/owner/demo"]\n'
            "\tpath = github.com/owner/demo\n"
            "\turl = git@github.com:owner/demo\n",
            encoding="utf-8",
        )
        git(
            root,
            [
                "config",
                "submodule.github.com/owner/demo.url",
                "git@github.com:owner/demo.git",
            ],
        )
        if not (
            _converge_home_checkout(
                root,
                checkout,
                expected,
                "git@github.com:owner/demo",
                dry_run=False,
            )
        ):
            raise AssertionError
        if not (
            git(checkout, ["remote", "get-url", "origin"]).stdout.strip()
            == ("git@github.com:owner/demo")
        ):
            raise AssertionError
        indexed = git(root, ["ls-files", "--stage", "--", str(expected)]).stdout
        if not (indexed.split()[1] == second):
            raise AssertionError


def test_home_checkout_rejects_dirty_or_unpublished_head() -> None:
    """Do not record submodule state that another checkout cannot reproduce."""
    with tempfile.TemporaryDirectory() as temporary_directory:
        root = Path(temporary_directory)
        checkout = root / "demo"
        checkout.mkdir()
        git(root, ["init", "--quiet"])
        git(checkout, ["init", "--quiet"])
        git(checkout, ["config", "user.email", "test@example.com"])
        git(checkout, ["config", "user.name", "Test"])
        source = checkout / "README"
        source.write_text("clean\n", encoding="utf-8")
        git(checkout, ["add", "README"])
        git(checkout, ["commit", "--quiet", "-m", "initial"])
        git(checkout, ["remote", "add", "origin", "git@example.com:owner/demo"])
        git(root, ["add", "demo"])
        error_message = ""
        try:
            _converge_home_checkout(
                root,
                checkout,
                Path("demo"),
                "git@example.com:owner/demo",
                dry_run=False,
            )
        except CommandError as error:
            error_message = str(error)
        else:
            msg = "unpublished submodule HEAD was accepted"
            raise AssertionError(msg)
        if "not known to an origin remote-tracking ref" not in error_message:
            raise AssertionError
        source.write_text("dirty\n", encoding="utf-8")
        error_message = ""
        try:
            _converge_home_checkout(
                root,
                checkout,
                Path("demo"),
                "git@example.com:owner/demo",
                dry_run=False,
            )
        except CommandError as error:
            error_message = str(error)
        else:
            msg = "dirty submodule worktree was accepted"
            raise AssertionError(msg)
        if "submodule worktree is dirty" not in error_message:
            raise AssertionError


def test_canonicalize_is_the_convergence_command() -> None:
    """Name the mutating operation after what it does."""
    if not (parser().parse_args(["canonicalize"]).command == "canonicalize"):
        raise AssertionError
    try:
        parser().parse_args(["check"])
    except SystemExit:
        pass
    else:
        msg = "legacy check command was accepted"
        raise AssertionError(msg)


def test_mv_renames_packages_generated_checks_and_hosts() -> None:
    """Move both resource kinds and keep generated package paths aligned."""
    with tempfile.TemporaryDirectory() as temporary_directory:
        root = Path(temporary_directory)
        git(root, ["init"])
        package = root / "packages" / "old_package"
        check = root / "checks" / "old_package_coverage"
        host = root / "hosts" / "oldHost"
        package.mkdir(parents=True)
        check.mkdir(parents=True)
        host.mkdir(parents=True)
        (package / "default.nix").write_text("{ }: { }\n", encoding="utf-8")
        (package / "main.py").write_text("", encoding="utf-8")
        (package / "test_main.py").write_text(
            "def test_behavior() -> None:\n    pass\n",
            encoding="utf-8",
        )
        (check / "default.nix").write_text(
            _current_python_coverage_source(),
            encoding="utf-8",
        )
        (host / "configuration.nix").write_text("{ ... }: { }\n", encoding="utf-8")
        packages = detect_packages(root)
        (root / ".gitignore").write_text(
            render_gitignore(allowed_paths(root, packages), opaque_trees(root)),
            encoding="utf-8",
        )
        git(root, ["add", "--force", "--all"])
        if Path("packages/old_package/main.py") not in _tracked_paths(root):
            raise AssertionError
        rename_resource(
            root,
            "packages/old_package",
            "packages/new_package",
            False,  # noqa: FBT003
        )
        rename_resource(root, "hosts/oldHost", "hosts/newHost", False)  # noqa: FBT003
        if package.exists():
            raise AssertionError
        if not ((root / "packages" / "new_package" / "main.py").is_file()):
            raise AssertionError
        if check.exists():
            raise AssertionError
        if not ((root / "checks" / "new_package_coverage" / "default.nix").is_file()):
            raise AssertionError
        if host.exists():
            raise AssertionError
        if not ((root / "hosts" / "newHost" / "configuration.nix").is_file()):
            raise AssertionError
        if not (
            _read_regular(root / ".gitignore")
            == render_gitignore(
                allowed_paths(root, detect_packages(root)),
                opaque_trees(root),
            )
        ):
            raise AssertionError


def test_mv_rejects_cross_resource_and_noncanonical_paths() -> None:
    """Keep rename operations within one canonical resource collection."""
    for source, destination, expected in (
        ("packages/demo", "hosts/demo", "cannot rename"),
        ("demo", "packages/example", "packages/NAME or hosts/NAME"),
        ("packages/bad-name", "packages/example", "snake_case"),
        ("hosts/demo", "hosts/install-iso", "camelCase"),
    ):
        if expected not in _rename_error(source, destination):
            raise AssertionError


def _rename_error(source: str, destination: str) -> str:
    """Return the user-facing failure for an invalid rename."""
    try:
        rename_resource(Path(), source, destination, True)  # noqa: FBT003
    except CommandError as error:
        return str(error)
    msg = f"invalid rename was accepted: {source} -> {destination}"
    raise AssertionError(msg)


def test_add_and_rm_manage_hosts_as_explicit_resources() -> None:
    """Create and remove packages and hosts through qualified resource paths."""
    with tempfile.TemporaryDirectory() as temporary_directory:
        root = Path(temporary_directory)
        git(root, ["init", "--quiet"])
        (root / ".gitignore").write_text("*\n", encoding="utf-8")
        _dispatch_add(root, parser().parse_args(["add", "hosts/newHost"]))
        configuration = root / "hosts" / "newHost" / "configuration.nix"
        if not (configuration.read_text(encoding="utf-8") == "{ ... }: { }\n"):
            raise AssertionError
        if Path("hosts/newHost/configuration.nix") not in _tracked_paths(root):
            raise AssertionError
        host_check = root / "checks" / "newHostVmWithDisko"
        if not (
            (host_check / "default.nix").read_text(encoding="utf-8")
            == (_current_host_check_source())
        ):
            raise AssertionError
        if Path("checks/newHostVmWithDisko/default.nix") not in _tracked_paths(root):
            raise AssertionError
        remove_resource(root, "hosts/newHost", False)  # noqa: FBT003
        if configuration.parent.exists():
            raise AssertionError
        if host_check.exists():
            raise AssertionError
        if not (Path("hosts/newHost/configuration.nix") not in _tracked_paths(root)):
            raise AssertionError
        if not (
            Path("checks/newHostVmWithDisko/default.nix")
            not in _tracked_paths(
                root,
            )
        ):
            raise AssertionError
        _dispatch_add(
            root,
            parser().parse_args(["add", "packages/example", "nix"]),
        )
        remove_resource(root, "packages/example", False)  # noqa: FBT003
        if (root / "packages" / "example").exists():
            raise AssertionError


def test_top_level_help_is_concise_and_conventional() -> None:
    """List commands without embedding a usage guide in parser help."""
    help_text = _render_help([])
    for expected in (
        "usage: git_canonicalization",
        "commands:",
        "init",
        "add",
        "mv",
        "rm",
        "canonicalize",
        "-h, --help",
    ):
        if expected not in help_text:
            raise AssertionError
    for unwanted in ("Choose an action", "Use native Git", "Layout policy"):
        if not (unwanted not in help_text):
            raise AssertionError


def test_subcommand_help_describes_arguments_and_hides_internal_options() -> None:
    """Describe public inputs without custom examples or policy text."""
    expected = {
        "init": ("REMOTE", "flake repository"),
        "add": ("RESOURCE", "packages/NAME or hosts/NAME", "TYPE"),
        "mv": ("SOURCE", "DESTINATION"),
        "rm": ("RESOURCE", "packages/NAME or hosts/NAME"),
        "canonicalize": ("--dry-run", "canonical layout"),
    }
    for command, fragments in expected.items():
        help_text = _render_help([command])
        if not (all(fragment in help_text for fragment in fragments)):
            raise AssertionError
    if not ("--source" not in _render_help(["canonicalize"])):
        raise AssertionError


def test_removed_status_interfaces_are_rejected() -> None:
    """Keep retired status import and export interfaces out of the CLI."""
    argparse_error = 2
    for arguments in (
        ["status"],
        ["init", "flake", "remote", "--from-status", "status.json"],
    ):
        with contextlib.redirect_stderr(io.StringIO()):
            try:
                parser().parse_args(arguments)
            except SystemExit as error:
                if error.code != argparse_error:
                    msg = f"removed status interface exited with {error.code}"
                    raise AssertionError(msg) from error
            else:
                msg = f"removed status interface was accepted: {arguments}"
                raise AssertionError(msg)


def test_help_command_is_equivalent_to_help_option() -> None:
    """Support top-level and command help through either spelling."""
    if not (
        _render_cli_help(_normalize_help_arguments(["help"]))
        == _render_cli_help(
            ["--help"],
        )
    ):
        raise AssertionError
    if not (
        _render_cli_help(
            _normalize_help_arguments(["help", "add"]),
        )
        == _render_cli_help(
            ["add", "--help"],
        )
    ):
        raise AssertionError


def _render_help(arguments: list[str]) -> str:
    """Render parser help for a command without invoking repository behavior."""
    return _render_cli_help([*arguments, "--help"])


def _render_cli_help(arguments: list[str]) -> str:
    """Render parser output for an exact help invocation."""
    output = io.StringIO()
    with contextlib.redirect_stdout(output):
        try:
            parser().parse_args(arguments)
        except SystemExit as error:
            if error.code != 0:
                msg = f"argparse help exited with status {error.code}"
                raise AssertionError(msg) from error
        else:
            msg = "argparse help did not exit"
            raise AssertionError(msg)
    return output.getvalue()


def test_gitignore_patterns_are_globally_sorted() -> None:
    """Sort directory and file whitelist patterns together."""
    if not (
        render_gitignore(
            {Path("z/file"), Path("a")},
            {Path("prm")},
        )
        == ("*\n!/a\n!/prm/\n!/prm/**\n!/z/\n!/z/file\n")
    ):
        raise AssertionError


def test_home_initialization_uses_canonical_ignore_policy() -> None:
    """Create required home negations and reject non-negation entries."""
    with tempfile.TemporaryDirectory() as temporary_directory:
        root = Path(temporary_directory)
        git(root, ["init", "--quiet"])
        if not (_converge_home_ignore(root, dry_run=False)):
            raise AssertionError
        if not (
            (root / ".gitignore").read_text(encoding="utf-8")
            == ("*\n!/.gitignore\n!/.gitmodules\n")
        ):
            raise AssertionError
        (root / ".gitignore").write_text("*\nunsupported\n", encoding="utf-8")
        try:
            _converge_home_ignore(root, dry_run=False)
        except CommandError:
            pass
        else:
            msg = "non-negation home ignore entry was accepted"
            raise AssertionError(msg)


def test_git_clean_arguments_are_profile_specific() -> None:
    """Preserve each profile's scratch trees through native Git clean options."""
    if not (
        _home_clean_arguments(dry_run=True)
        == [
            "clean",
            "-ndx",
            "-e",
            f"/{SCRATCH_NAME}/",
        ]
    ):
        raise AssertionError
    if not (
        _flake_clean_arguments(dry_run=False)
        == [
            "clean",
            "-fdx",
            "-e",
            f"/{SCRATCH_NAME}/",
            "-e",
            f"/packages/*/{SCRATCH_NAME}/",
        ]
    ):
        raise AssertionError


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
        check_flake(root, False)  # noqa: FBT003
        if not (root_tmp.read_text(encoding="utf-8") == "root"):
            raise AssertionError
        if not (package_tmp.read_text(encoding="utf-8") == "package"):
            raise AssertionError
        if unsupported.exists():
            raise AssertionError
        if not (
            "packages/sample/tmp/package-state"
            not in git(
                root,
                ["ls-files"],
            ).stdout.splitlines()
        ):
            raise AssertionError


def test_convergence_derives_checks_and_removes_orphans() -> None:
    """Generate resource-owned checks and delete standalone check state."""
    with tempfile.TemporaryDirectory() as temporary_directory:
        root = Path(temporary_directory)
        _temporary_flake(root)
        files = scaffold("python", "sample", None)
        files[Path("packages/sample/test_main.py")] = (
            "def test_works() -> None:\n    pass\n"
        )
        generated_check = Path("checks/sample_coverage/default.nix")
        for relative, source in files.items():
            if relative != generated_check:
                path = root / relative
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text(source, encoding="utf-8")
        host = root / "hosts" / "demo" / "configuration.nix"
        host.parent.mkdir(parents=True)
        host.write_text("{ ... }: { }\n", encoding="utf-8")
        orphan = root / "checks" / "source_conformance" / "default.nix"
        orphan.parent.mkdir(parents=True)
        orphan.write_text("{ }: { }\n", encoding="utf-8")
        git(root, ["add", "--force", "--", "packages", "hosts", "checks"])
        check_flake(root, False)  # noqa: FBT003
        if not (
            (root / generated_check).read_text(encoding="utf-8")
            == (_current_python_coverage_source())
        ):
            raise AssertionError
        if not (
            (root / "checks" / "demoVmWithDisko" / "default.nix").read_text(
                encoding="utf-8",
            )
            == _current_host_check_source()
        ):
            raise AssertionError
        if orphan.parent.exists():
            raise AssertionError


def test_convergence_stages_untracked_opaque_package_files() -> None:
    """Stage new opaque resources before Git cleanup can remove them."""
    with tempfile.TemporaryDirectory() as temporary_directory:
        root = Path(temporary_directory)
        _temporary_flake(root)
        package = root / "packages" / "sample"
        resource = package / "prm" / "visual" / "snapshot.png"
        resource.parent.mkdir(parents=True)
        (package / "default.nix").write_text(
            "{ pkgs, ... }: pkgs.emptyFile\n",
            encoding="utf-8",
        )
        resource.write_bytes(b"snapshot")
        check_flake(root, False)  # noqa: FBT003
        if not (resource.read_bytes() == b"snapshot"):
            raise AssertionError
        if (
            resource.relative_to(root).as_posix()
            not in git(
                root,
                ["ls-files"],
            ).stdout.splitlines()
        ):
            raise AssertionError


def test_convergence_preserves_forgejo_workflow() -> None:
    """Preserve the canonical Forgejo Actions workflow."""
    with tempfile.TemporaryDirectory() as temporary_directory:
        root = Path(temporary_directory)
        _temporary_flake(root)
        workflow = root / ".forgejo" / "workflows" / "workflow.yml"
        workflow.parent.mkdir(parents=True)
        workflow.write_text("name: CI\n", encoding="utf-8")
        git(root, ["add", "--force", str(workflow.relative_to(root))])
        check_flake(root, False)  # noqa: FBT003
        if not (workflow.is_file()):
            raise AssertionError


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
            check_flake(root, False)  # noqa: FBT003
        except CommandError as error:
            error_message = str(error)
        else:
            msg = "nested Git repository passed structural validation"
            raise AssertionError(msg)
        if "repository layout validation failed" not in error_message:
            raise AssertionError
        if not ((nested / ".git").is_dir()):
            raise AssertionError


def test_python_default_does_not_require_test_metadata() -> None:
    """Canonicalize tested Python packages without test-name metadata."""
    with tempfile.TemporaryDirectory() as temporary_directory:
        package_root = Path(temporary_directory) / "packages" / "sample"
        package_root.mkdir(parents=True)
        (package_root / "main.py").write_text("", encoding="utf-8")
        (package_root / "test_main.py").write_text(
            "def test_behavior(): pass\n",
            encoding="utf-8",
        )
        (package_root / "default.nix").write_text(
            scaffold("python", "sample", None)[Path("packages/sample/default.nix")],
            encoding="utf-8",
        )
        package = Package("sample", "python", package_root)
        rendered = canonical_typed_default(package)
        if rendered is None:
            msg = "Python default was not rendered"
            raise AssertionError(msg)
        if "canonicalization.tests" in rendered:
            msg = "Python default included test-name metadata"
            raise AssertionError(msg)
        (package_root / "default.nix").write_text(rendered, encoding="utf-8")
        issues = _source_package_issues(Path(temporary_directory), package)
        if issues:
            msg = f"rendered Python default was not canonical: {issues}"
            raise AssertionError(msg)


def test_coverage_profile_runs_only_explicit_examples(tmp_path: Path) -> None:
    """The generated coverage bootstrap suppresses random generation."""
    source = tmp_path / "test_property.py"
    source.write_text(
        "from hypothesis import given, example, strategies as st\n"
        "from pathlib import Path\n"
        "@given(st.integers())\n"
        "@example(123)\n"
        "def test_explicit(value):\n"
        "    with Path('examples').open('a') as output:\n"
        "        output.write(str(value) + '\\n')\n"
        "@given(st.integers())\n"
        "def test_no_example(value):\n"
        "    raise AssertionError('must not generate')\n",
        encoding="utf-8",
    )
    bootstrap = (
        _current_python_coverage_source().split("python -c '", 1)[1].split("'", 1)[0]
    )
    result = subprocess.run(  # noqa: S603
        [sys.executable, "-c", bootstrap, "-p", "no:cacheprovider", str(source)],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )
    if result.returncode or "1 passed, 1 skipped" not in result.stdout:
        raise AssertionError(result.stdout + result.stderr)
    if (tmp_path / "examples").read_text(encoding="utf-8") != "123\n":
        message = "coverage must execute exactly the explicit input"
        raise AssertionError(message)


_REMOTE_PART = st.text(alphabet="abcXYZ012_-", min_size=1, max_size=12)


@given(host=_REMOTE_PART, owner=_REMOTE_PART, repository=_REMOTE_PART)
@example(host="Forge", owner="Owner", repository="repo_1")
def test_remote_spellings_have_one_canonical_path(
    host: str,
    owner: str,
    repository: str,
) -> None:
    """Transport, username, suffix, and host case do not change checkout identity."""
    hostname = host + ".example"
    expected = Path(hostname.lower(), owner, repository)
    remotes = [
        f"git@{hostname}:{owner}/{repository}.git",
        f"{hostname}:{owner}/{repository}",
    ]
    remotes.extend(
        f"{scheme}://git@{hostname.upper()}/{owner}/{repository}{suffix}"
        for scheme in ("https", "http", "ssh", "git+ssh", "git")
        for suffix in ("", ".git", ".git/")
    )
    for remote in remotes:
        if canonical_remote_path(remote) != expected:
            msg = "equivalent remotes mapped to different checkout paths"
            raise AssertionError(msg)


@given(
    owner=_REMOTE_PART,
    repository=_REMOTE_PART,
    unsafe=st.sampled_from([".", "..", "", "bad name", "é"]),
)
@example(owner="owner", repository="repo", unsafe="..")
def test_remote_paths_reject_unsafe_components(
    owner: str,
    repository: str,
    unsafe: str,
) -> None:
    """Hosted paths may not traverse or introduce unsupported checkout components."""
    for remote in (
        f"https://forge.example/{owner}/{unsafe}/{repository}.git",
        f"git@forge.example:{owner}/{unsafe}/{repository}.git",
    ):
        with pytest.raises(CommandError):
            canonical_remote_path(remote)


@given(
    names=st.sets(
        st.text(alphabet="abcxyz012", min_size=1, max_size=8),
        min_size=1,
        max_size=6,
    ),
    contents=st.binary(max_size=30),
)
@example(names={"a", "ab"}, contents=b"asset")
def test_gitignore_whitelist_matches_git(names: set[str], contents: bytes) -> None:
    """Whitelist ancestors expose selected files and opaque trees, but not siblings."""
    with tempfile.TemporaryDirectory() as directory:
        root = Path(directory)
        _temporary_flake(root)
        allowed = {Path("packages", name, "main.py") for name in names}
        trees = {Path("packages", name, "prm") for name in names}
        visible = allowed | {tree / "nested" / "asset.bin" for tree in trees}
        hidden = {Path("unlisted.bin")}
        for name in names:
            hidden.update(
                {
                    Path("packages", name, "scratch.bin"),
                    Path("packages", name, "prm_extra", "asset.bin"),
                },
            )
        for relative in visible | hidden:
            path = root / relative
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_bytes(contents)
        (root / ".gitignore").write_text(
            render_gitignore(allowed, trees),
            encoding="utf-8",
        )
        result = git(
            root,
            [
                "-c",
                "core.excludesFile=/dev/null",
                "check-ignore",
                "--no-index",
                *sorted(str(path) for path in visible | hidden),
            ],
            check=False,
        )
        if result.returncode != 0 or set(result.stdout.splitlines()) != {
            str(path) for path in hidden
        }:
            msg = "rendered whitelist disagrees with Git's ignore behavior"
            raise AssertionError(msg)
