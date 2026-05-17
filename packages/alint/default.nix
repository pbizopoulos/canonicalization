{
  pkgs ? import <nixpkgs> { },
}:
pkgs.stdenv.mkDerivation rec {
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
    install -Dm755 ${pname}-v${version}-x86_64-unknown-linux-musl/${pname} $out/bin/${pname}
    runHook postInstall
  '';
  meta.mainProgram = pname;
  pname = baseNameOf ./.;
  sourceRoot = ".";
  src = pkgs.fetchurl {
    sha256 = "4yOzM6f8Rdsw2YxsqSpIhHCNuRZRf8j3AAcK2T5VZlU=";
    url = "https://github.com/asamarts/${pname}/releases/download/v${version}/${pname}-v${version}-x86_64-unknown-linux-musl.tar.gz";
  };
  strictDeps = true;
  version = "0.9.23";
}
