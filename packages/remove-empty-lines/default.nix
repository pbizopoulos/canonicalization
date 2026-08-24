{
  pkgs ? import <nixpkgs> { },
}:
let
  cargoToml = cargoTomlFormat.generate "Cargo.toml" {
    dependencies = {
      anyhow = "1.0";
      content_inspector = "0.2";
      ignore = "0.4";
    };
    "dev-dependencies" = {
      quickcheck = "1";
      tempfile = "3.8";
    };
    package = {
      inherit version;
      edition = "2021";
      name = pname;
    };
  };
  cargoTomlFormat = pkgs.formats.toml { };
  pname = baseNameOf ./.;
  version = "0.1.0";
in
pkgs.rustPlatform.buildRustPackage rec {
  cargoLock.lockFile = ./Cargo.lock;
  doInstallCheck = pkgs.stdenv.isLinux;
  env = {
    RUSTDOCFLAGS = "-D warnings";
    RUSTFLAGS = "-D warnings -F unsafe-code";
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
  passthru = {
    inherit cargoToml;
    rustCheckNativeBuildInputs = [
      pkgs.cargo
      pkgs.rustc
      pkgs.stdenv.cc
    ];
    updateCargoLock = pkgs.writeShellApplication {
      name = "update-remove-empty-lines-cargo-lock";
      runtimeInputs = [
        pkgs.cargo
      ];
      text = ''
        workdir="''${1:-.}"
        temporary_directory=$(mktemp -d)
        trap 'rm -rf "$temporary_directory"' EXIT
        cp ${cargoToml} "$temporary_directory/Cargo.toml"
        cargo generate-lockfile --manifest-path "$temporary_directory/Cargo.toml"
        install -Dm644 "$temporary_directory/Cargo.lock" "$workdir/Cargo.lock"
      '';
    };
  };
  pname = baseNameOf ./.;
  postPatch = ''
    cp ${cargoToml} Cargo.toml
  '';
  src = ./.;
  strictDeps = true;
  version = "0.1.0";
}
