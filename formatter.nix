{
  flake,
  inputs,
  pkgs,
  ...
}:
let
  clippy-script = pkgs.writeShellScriptBin "clippy" ''
    [ -n "$NIX_BUILD_TOP" ] && exit 0
    export PATH="${
      pkgs.lib.makeBinPath [
        pkgs.cargo
        pkgs.clippy
        pkgs.rustc
        pkgs.stdenv.cc
      ]
    }:$PATH"
    find packages -name Cargo.toml -execdir cargo clippy --fix --allow-dirty --allow-staged \
      --all-targets --all-features -- \
      -D warnings -D clippy::all -D clippy::pedantic -D clippy::nursery -D clippy::cargo \;
  '';
  formatter = treefmtEval.config.build.wrapper;
  treefmtEval = inputs.treefmt-nix.lib.evalModule pkgs {
    programs = {
      actionlint.enable = true;
      beautysh.enable = true;
      biome = {
        enable = true;
        formatUnsafe = true;
      };
      cabal-fmt.enable = true;
      clang-format.enable = true;
      deadnix.enable = true;
      dos2unix.enable = true;
      hclfmt.enable = true;
      hlint.enable = true;
      jsonfmt.enable = true;
      mdformat.enable = true;
      mdsh.enable = true;
      nixfmt.enable = true;
      ormolu.enable = true;
      ruff-check = {
        enable = true;
        extendSelect = [
          "ALL"
        ];
      };
      ruff-format.enable = true;
      rustfmt.enable = true;
      shellcheck.enable = true;
      shfmt = {
        enable = true;
        simplify = true;
      };
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
          includes = [
            "*.bib"
          ];
          options = [
            "--duplicates"
            "--no-align"
            "--no-wrap"
            "--sort"
            "--sort-fields"
            "--v2"
          ];
        };
        biome.options = [
          "--max-diagnostics=none"
        ];
        check-repository-file-contents = {
          command = inputs.self.packages.${pkgs.stdenv.system}."check-repository-file-contents";
          includes = [
            "flake.nix"
          ];
          priority = 0;
        };
        clippy = {
          command = "${clippy-script}/bin/clippy";
          includes = [
            "*.rs"
          ];
          priority = 1;
        };
        mypy = {
          command = pkgs.mypy;
          includes = [
            "*.py"
          ];
          options = [
            "--cache-dir=/tmp/.mypy_cache"
            "--explicit-package-bases"
            "--ignore-missing-imports"
            "--strict"
          ];
        };
        nix-alphabetize = {
          command = inputs.self.packages.${pkgs.stdenv.system}.nix-alphabetize;
          includes = [
            "*.nix"
          ];
          priority = 0;
        };
        nixfmt.priority = 1;
        remove-empty-lines = {
          command = inputs.self.packages.${pkgs.stdenv.system}."remove-empty-lines";
          includes = [
            "*"
          ];
          priority = 0;
        };
        ruff-check.options = [
          "--cache-dir=/tmp/.ruff_cache"
          "--unsafe-fixes"
        ];
        ruff-format.options = [
          "--cache-dir=/tmp/.ruff_cache"
        ];
        rustfmt.priority = 1;
        shfmt.options = [
          "--posix"
        ];
        ssort = {
          command = pkgs.python3Packages.ssort;
          includes = [
            "*.py"
          ];
          priority = 1;
        };
        uncomment = {
          command = inputs.self.packages.${pkgs.stdenv.system}.uncomment;
          includes = [
            "*"
          ];
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
