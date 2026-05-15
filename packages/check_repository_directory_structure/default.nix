{
  pkgs ? import <nixpkgs> { },
}:
pkgs.rustPlatform.buildRustPackage rec {
  buildInputs = [
    pkgs.openssl
    pkgs.zlib
  ];
  cargoHash = "sha256-fCa2ITVSNmdt45RBW/NqpERS5BrCMri+P3aRE+9qezE=";
  doInstallCheck = pkgs.stdenv.isLinux;
  env = {
    RUSTDOCFLAGS = "-D warnings";
    RUSTFLAGS = "-D warnings";
  };
  installCheckPhase = ''
    runHook preInstallCheck
    test -x "$out/bin/${pname}"
    set -o pipefail
    "$out/bin/${pname}" --help 2>&1 | grep -F "${pname}"
    runHook postInstallCheck
  '';
  meta.mainProgram = pname;
  nativeBuildInputs = [
    pkgs.git
    pkgs.pkg-config
  ];
  pname = baseNameOf ./.;
  src = ./.;
  strictDeps = true;
  version = "0.0.0";
}
