{
  pkgs ? import <nixpkgs> { },
}:
pkgs.stdenv.mkDerivation rec {
  doInstallCheck = pkgs.stdenv.isLinux;
  installCheckPhase = ''
    runHook preInstallCheck
    "$out/bin/${pname}" --help
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
    sha256 = "SVrPfhkfRSh/1p/fvoGE686L56AmARSEul9cIgAiHzc=";
    url = "https://github.com/asamarts/${pname}/releases/download/v${version}/${pname}-v${version}-x86_64-unknown-linux-musl.tar.gz";
  };
  strictDeps = true;
  version = "0.9.20";
}
