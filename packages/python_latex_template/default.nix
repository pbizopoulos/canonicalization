{
  pkgs ? import <nixpkgs> { },
}:
let
  python = pkgs.python3;
  runtimePythonDeps = [
    python.pkgs.matplotlib
    python.pkgs.pandas
  ];
in
python.pkgs.buildPythonPackage rec {
  installPhase = ''
    datadir="$out/share/${pname}"
    install -Dm644 main.py ms.tex ms.bib -t "$datadir"
    install -Dm644 main.py $out/${python.sitePackages}/${pname}.py
    mkdir -p "$out/bin"
    cat > "$out/bin/${pname}" <<EOF
    #!${pkgs.bash}/bin/bash
    set -euo pipefail
    mkdir -p tmp
    ${python.withPackages (_: runtimePythonDeps)}/bin/python3 "$datadir/main.py"
    cp "$datadir"/ms.{tex,bib} tmp/
    ${pkgs.texliveFull}/bin/latexmk -cd -pdf tmp/ms.tex
    EOF
    chmod +x "$out/bin/${pname}"
  '';
  meta = {
    description = "A Python and LaTeX template package.";
    mainProgram = pname;
  };
  passthru = {
    inherit python;
  };
  pname = baseNameOf ./.;
  propagatedBuildInputs = runtimePythonDeps ++ [
    python.pkgs.hypothesis
    python.pkgs.pytest
  ];
  pyproject = false;
  src = ./.;
  strictDeps = true;
  version = "0.0.0";
}
