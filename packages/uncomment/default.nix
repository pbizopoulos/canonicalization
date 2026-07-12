{
  pkgs ? import <nixpkgs> { },
}:
pkgs.stdenv.mkDerivation rec {
  buildInputs = [
    pkgs.stdenv.cc.cc.lib
  ];
  doInstallCheck = pkgs.stdenv.isLinux;
  installCheckPhase = ''
    runHook preInstallCheck
    test -x "$out/bin/${pname}"
    set -o pipefail
    "$out/bin/${pname}" --help 2>&1 | grep -F "${pname}"
    runHook postInstallCheck
  '';
  installPhase = ''
    runHook preInstall
    install -Dm755 ${pname} $out/bin/${pname}
    runHook postInstall
  '';
  meta = {
    description = "A fast Rust-based CLI tool for removing comments from source code.";
    mainProgram = pname;
  };
  nativeBuildInputs = [
    pkgs.autoPatchelfHook
  ];
  pname = baseNameOf ./.;
  sourceRoot = ".";
  src = pkgs.fetchurl {
    sha256 = "4cBM1u/qPsMxv2+jy7j2+kjO6SN1BVehv/GDdOeD1g4=";
    url = "https://github.com/Goldziher/${pname}/releases/download/v${version}/${pname}-x86_64-unknown-linux-gnu.tar.gz";
  };
  strictDeps = true;
  version = "3.4.0";
}
