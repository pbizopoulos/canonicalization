{
  pkgs ? import <nixpkgs> { },
}:
pkgs.rustPlatform.buildRustPackage rec {
  cargoHash = "sha256-4Tpc7xOULVqqUzlg2G581bdTxP6SaHR7n9vdq8rqxX0=";
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
  passthru.rustCheckNativeBuildInputs = [
    pkgs.cargo
    pkgs.rustc
    pkgs.stdenv.cc
  ];
  pname = baseNameOf ./.;
  src = ./.;
  strictDeps = true;
  version = "0.1.0";
}
