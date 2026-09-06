{ inputs, pkgs, ... }:
let
  checker = inputs.self.packages.${pkgs.stdenv.system}.git_canonicalization;
in
pkgs.runCommand "source_conformance"
  {
    nativeBuildInputs = [ checker ];
    src = ../..;
  }
  ''
    git_canonicalization check --source "$src"
    touch "$out"
  ''
