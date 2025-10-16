# no-check
{ inputs, pkgs, ... }:
inputs.treefmt-nix.lib.mkWrapper pkgs {
  programs = {
    actionlint.enable = true;
    beautysh.enable = true;
    biome.enable = true;
    deadnix.enable = true;
    hlint.enable = true;
    nixfmt.enable = true;
    ormolu.enable = true;
    prettier.enable = true;
    ruff-check.enable = true;
    ruff-format.enable = true;
    shellcheck.enable = true;
    shfmt.enable = true;
    statix.enable = true;
    terraform.enable = true;
    texfmt.enable = true;
    toml-sort.enable = true;
    yamlfmt.enable = true;
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
      check-nix = {
        command = inputs.self.packages.${pkgs.stdenv.system}.check-nix;
        includes = [ "*.nix" ];
        priority = 0;
      };
      check-python = {
        command = inputs.self.packages.${pkgs.stdenv.system}.check_python;
        includes = [ "*.py" ];
        priority = 0;
      };
      check-repository-directory-structure = {
        command = inputs.self.packages.${pkgs.stdenv.system}.check_repository_directory_structure;
        includes = [ "flake.nix" ];
      };
      mypy = {
        command = pkgs.mypy;
        includes = [ "*.py" ];
        options = [
          "--cache-dir"
          "tmp/mypy"
          "--explicit-package-bases"
          "--ignore-missing-imports"
          "--strict"
        ];
        priority = 2;
      };
      nixfmt = {
        priority = 1;
        strict = true;
      };
      ruff-check = {
        options = [
          "--cache-dir"
          "tmp/ruff"
          "--select"
          "ALL"
          "--unsafe-fixes"
        ];
        priority = 2;
      };
      ruff-format = {
        options = [
          "--cache-dir"
          "tmp/ruff"
        ];
        priority = 1;
      };
      shfmt.options = [
        "--posix"
        "--simplify"
      ];
      statix.priority = 2;
      texfmt.options = [ "--nowrap" ];
      toml-sort = {
        includes = [ "*.toml" ];
        options = [ "--all" ];
      };
    };
    global.excludes = [
      "*/prm/**"
      "*/tmp/**"
    ];
  };
}
