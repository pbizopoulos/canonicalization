{
  pkgs ? import <nixpkgs> { },
}:
pkgs.stdenv.mkDerivation rec {
  doInstallCheck = pkgs.stdenv.isLinux;
  installCheckPhase = ''
    runHook preInstallCheck
    set -o pipefail
    "$out/bin/${pname}" --help 2>&1 | grep -F "${pname}"
    runHook postInstallCheck
  '';
  installPhase = ''
    runHook preInstall
    install -Dm755 ${pname} $out/bin/${pname}
    runHook postInstall
  '';
  meta.mainProgram = pname;
  nativeBuildInputs = [
    pkgs.autoPatchelfHook
  ];
  pname = baseNameOf ./.;
  sourceRoot = ".";
  src = pkgs.fetchurl {
    sha256 = "6jUmVZ5SIKRgaF6V6gy2aFu4ZgcKbhl1O7g16UcnIQQ=";
    url = "https://github.com/Goldziher/${pname}/releases/download/v${version}/${pname}-x86_64-unknown-linux-gnu.tar.gz";
  };
  strictDeps = true;
  version = "3.0.2";
}
