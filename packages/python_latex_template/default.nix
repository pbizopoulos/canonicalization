{
  pkgs ? import <nixpkgs> { },
}:
let
  pythonDeps = [
    pkgs.python3Packages.matplotlib
    pkgs.python3Packages.pandas
  ];
  pythonEnv = pkgs.python3.withPackages (_: pythonDeps);
in
pkgs.python3Packages.buildPythonPackage rec {
  installCheckPhase = ''
    runHook preInstallCheck
    HOME="$(mktemp -d)"
    DEBUG=1 "$out/bin/${pname}"
    runHook postInstallCheck
  '';
  installPhase = ''
    datadir="$out/share/${pname}"
    install -Dm644 ./main.py ./ms.tex ./ms.bib -t "$datadir"
    mkdir -p "$out/bin"
    cat > "$out/bin/${pname}" <<EOF
    #!${pkgs.bash}/bin/bash
    set -euo pipefail
    mkdir -p tmp
    ${pythonEnv}/bin/python3 "$datadir/main.py"
    cp "$datadir"/ms.{tex,bib} tmp/
    ${pkgs.texliveFull}/bin/latexmk -cd -pdf tmp/ms.tex
    EOF
    chmod +x "$out/bin/${pname}"
  '';
  meta.mainProgram = pname;
  pname = baseNameOf ./.;
  propagatedBuildInputs = pythonDeps;
  pyproject = false;
  src = ./.;
  strictDeps = true;
  version = "0.0.0";
}
