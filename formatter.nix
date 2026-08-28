{
  flake,
  inputs,
  pkgs,
  ...
}:
let
  formatter = treefmtEval.config.build.wrapper;
  git-canonicalization-script = pkgs.writeShellScriptBin "git-canonicalization-check" ''
    ${
      inputs.self.packages.${pkgs.stdenv.system}."git-canonicalization"
    }/bin/git-canonicalization check --fix
  '';
  treefmtEval = inputs.treefmt-nix.lib.evalModule pkgs {
    programs = {
      actionlint.enable = true;
      beautysh.enable = true;
      biome = {
        enable = true;
        formatUnsafe = true;
        priority = 1;
      };
      clang-format.enable = true;
      deadnix.enable = true;
      dos2unix.enable = true;
      hclfmt.enable = true;
      nixfmt.enable = true;
      oxfmt = {
        enable = true;
        priority = 1;
      };
      ruff-check = {
        enable = true;
        extendSelect = [ "ALL" ];
      };
      ruff-format.enable = true;
      rustfmt.enable = true;
      shellcheck.enable = true;
      shfmt.enable = true;
      statix.enable = true;
      taplo.enable = true;
      texfmt.enable = true;
      toml-sort.enable = true;
      yamlfmt.enable = true;
      yamllint.enable = true;
    };
    projectRootFile = "flake.nix";
    settings = {
      formatter = {
        bibtex-tidy = {
          command = pkgs.bibtex-tidy;
          includes = [ "*.bib" ];
          options = [
            "--duplicates"
            "--no-align"
            "--no-wrap"
            "--sort"
            "--sort-fields"
            "--v2"
          ];
        };
        biome.options = [ "--max-diagnostics=none" ];
        git-canonicalization = {
          command = "${git-canonicalization-script}/bin/git-canonicalization-check";
          includes = [ "*.nix" ];
          priority = 1;
        };
        html-tidy = {
          command = pkgs.html-tidy;
          includes = [ "*.html" ];
          options = [
            "--quiet"
            "yes"
            "--tidy-mark"
            "no"
            "--wrap"
            "0"
            "--write-back"
            "yes"
          ];
        };
        mypy = {
          command = pkgs.mypy;
          excludes = [
            "packages/git-canonicalization/main.py"
            "packages/nix-alphabetize/main.py"
            "packages/nix-remove-defaults/main.py"
          ];
          includes = [ "*.py" ];
          options = [
            "--cache-dir=/tmp/.mypy_cache"
            "--explicit-package-bases"
            "--ignore-missing-imports"
            "--strict"
          ];
        };
        nix-alphabetize = {
          command = inputs.self.packages.${pkgs.stdenv.system}.nix-alphabetize;
          includes = [ "*.nix" ];
        };
        oxlint = {
          command = pkgs.oxlint;
          includes = [ "*.js" ];
          options = [
            "-A"
            "all"
          ];
          priority = 1;
        };
        remove-empty-lines = {
          command = inputs.self.packages.${pkgs.stdenv.system}.remove_empty_lines;
          excludes = [ "README" ];
          includes = [ "*" ];
        };
        remove-new-lines = {
          command = inputs.self.packages.${pkgs.stdenv.system}.remove_new_lines;
          includes = [
            "*.css"
            "*.html"
            "*.js"
            "*.rs"
          ];
        };
        ruff-check.options = [
          "--cache-dir=/tmp/.ruff_cache"
          "--unsafe-fixes"
        ];
        ruff-format.options = [ "--cache-dir=/tmp/.ruff_cache" ];
        shfmt.options = [ "--posix" ];
        texfmt.options = [ "--nowrap" ];
        uncomment = {
          command = inputs.self.packages.${pkgs.stdenv.system}.uncomment;
          includes = [ "*" ];
        };
      };
      global.excludes = [
        "*/prm/**"
        "*/tmp/**"
      ];
    };
  };
in
formatter
// {
  passthru = formatter.passthru // {
    tests.check = treefmtEval.config.build.check flake;
  };
}
