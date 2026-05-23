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
    sha256 = "3yPgFR7bH5vHPcNc7XBiKmWg5TyYR9f9J0eDY73ulRk=";
    url = "https://github.com/asamarts/${pname}/releases/download/v${version}/${pname}-v${version}-x86_64-unknown-linux-musl.tar.gz";
  };
  strictDeps = true;
  version = "0.10.2";
}
