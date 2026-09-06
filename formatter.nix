{
  flake,
  inputs,
  pkgs,
  ...
}:
let
  rawFormatter = treefmtEval.config.build.wrapper;
  treefmtEval = inputs.treefmt-nix.lib.evalModule pkgs {
    programs = {
      actionlint = {
        enable = true;
        priority = 6;
      };
      deadnix = {
        enable = true;
        priority = 3;
      };
      dos2unix = {
        enable = true;
        priority = 1;
      };
      hclfmt = {
        enable = true;
        priority = 5;
      };
      nixfmt = {
        enable = true;
        priority = 5;
      };
      oxfmt = {
        enable = true;
        includes = [
          "*.css"
          "*.html"
          "*.js"
        ];
        priority = 5;
      };
      ruff-check = {
        enable = true;
        extendSelect = [ "ALL" ];
        priority = 3;
      };
      ruff-format = {
        enable = true;
        priority = 5;
      };
      statix = {
        enable = true;
        priority = 3;
      };
      texfmt = {
        enable = true;
        priority = 5;
      };
      yamlfmt = {
        enable = true;
        priority = 5;
      };
      yamllint = {
        enable = true;
        priority = 6;
      };
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
          priority = 5;
        };
        mypy = {
          command = pkgs.mypy;
          includes = [ "*.py" ];
          options = [
            "--cache-dir=/tmp/.mypy_cache"
            "--explicit-package-bases"
            "--ignore-missing-imports"
            "--strict"
          ];
          priority = 6;
        };
        nix-alphabetize = {
          command = inputs.self.packages.${pkgs.stdenv.system}.nix_alphabetize;
          includes = [ "*.nix" ];
          priority = 4;
        };
        oxlint = {
          command = pkgs.oxlint;
          includes = [ "*.js" ];
          options = [
            "--fix-dangerously"
            "-D"
            "all"
          ];
          priority = 3;
        };
        remove-empty-lines = {
          command = inputs.self.packages.${pkgs.stdenv.system}.remove_empty_lines;
          excludes = [ "README" ];
          includes = [ "*" ];
          priority = 2;
        };
        remove-new-lines = {
          command = inputs.self.packages.${pkgs.stdenv.system}.remove_new_lines;
          includes = [
            "*.css"
            "*.html"
            "*.js"
          ];
          priority = 2;
        };
        ruff-check.options = [
          "--cache-dir=/tmp/.ruff_cache"
          "--unsafe-fixes"
        ];
        ruff-format.options = [ "--cache-dir=/tmp/.ruff_cache" ];
        uncomment = {
          command = inputs.self.packages.${pkgs.stdenv.system}.uncomment;
          includes = [ "*" ];
          priority = 1;
        };
      };
      global.excludes = [
        "*/prm/**"
        "*/tmp/**"
        "prm/**"
        "tmp/**"
      ];
    };
  };
  wrapper = pkgs.writeShellApplication {
    name = "treefmt";
    runtimeInputs = [ inputs.self.packages.${pkgs.stdenv.system}.git_canonicalization ];
    text = ''
      git_canonicalization check
      exec ${rawFormatter}/bin/treefmt "$@"
    '';
  };
in
wrapper
// {
  passthru = wrapper.passthru // {
    raw = rawFormatter;
    tests.check = treefmtEval.config.build.check flake;
  };
}
