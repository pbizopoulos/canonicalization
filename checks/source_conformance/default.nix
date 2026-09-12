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
    git_canonicalization canonicalize --source "$src"
    touch "$out"
  ''
