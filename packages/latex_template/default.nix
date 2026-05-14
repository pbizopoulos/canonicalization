{
  pkgs ? import <nixpkgs> { },
}:
pkgs.stdenv.mkDerivation rec {
  buildPhase = ''
    latexmk -pdf ms.tex
  '';
  installPhase = ''
    install -Dm644 ms.pdf $out/ms.pdf
  '';
  meta.mainProgram = pname;
  nativeBuildInputs = [
    pkgs.texliveFull
  ];
  pname = baseNameOf ./.;
  src = ./.;
  strictDeps = true;
  version = "0.0.0";
}
