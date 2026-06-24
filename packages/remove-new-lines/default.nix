{
  pkgs ? import <nixpkgs> { },
}:
pkgs.rustPlatform.buildRustPackage rec {
  cargoHash = "sha256-fi0jrujGpWiDoXOezmOcLI4BE45r4iAAepCPNqSXzPA=";
  doInstallCheck = pkgs.stdenv.isLinux;
  env = {
    RUSTDOCFLAGS = "-D warnings";
    RUSTFLAGS = "-D warnings";
  };
  installCheckPhase = ''
    runHook preInstallCheck
    test -x "$out/bin/${pname}"
    workspace="$PWD/installcheck"
    mkdir -p "$workspace"
    "$out/bin/${pname}" "$workspace"
    runHook postInstallCheck
  '';
  meta.mainProgram = pname;
  passthru.rustCheckNativeBuildInputs = [
    pkgs.cargo
    pkgs.rustc
    pkgs.stdenv.cc
  ];
  pname = baseNameOf ./.;
  src = ./.;
  strictDeps = true;
  version = "0.0.0";
}
