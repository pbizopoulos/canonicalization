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
    test -x "$out/bin/${pname}"
    HOME="$(mktemp -d)" DEBUG=1 PYTHONWARNINGS=error coverage run --source="$src" -m pyinstrument "$src/main.py"
    coverage report
    runHook postInstallCheck
  '';
  installPhase = ''
    datadir="$out/share/${pname}"
    install -Dm644 ./main.py ./ms.tex ./ms.bib -t "$datadir"
    mkdir -p "$out/bin"
    cat > "$out/bin/${pname}" <<EOF
    #!/usr/bin/env bash
    set -euo pipefail
    mkdir -p tmp
    ${pythonEnv}/bin/python3 "$datadir/main.py"
    cp "$datadir"/ms.{tex,bib} tmp/
    ${pkgs.texliveFull}/bin/latexmk -cd -pdf tmp/ms.tex
    EOF
    chmod +x "$out/bin/${pname}"
  '';
  meta.mainProgram = pname;
  nativeInstallCheckInputs = [
    pkgs.python3Packages.coverage
    pkgs.python3Packages.pyinstrument
  ];
  pname = baseNameOf ./.;
  propagatedBuildInputs = pythonDeps;
  pyproject = false;
  src = ./.;
  strictDeps = true;
  version = "0.0.0";
}
