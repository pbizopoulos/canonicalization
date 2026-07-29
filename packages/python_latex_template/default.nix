{
  pkgs ? import <nixpkgs> { },
}:
let
  python = pkgs.python3;
  pythonDeps = [
    python.pkgs.matplotlib
  ];
  pythonEnv = python.withPackages (_: pythonDeps);
in
python.pkgs.buildPythonPackage rec {
  buildPhase = ''
    mkdir -p tmp
    ${pythonEnv}/bin/python3 main.py
    cp ms.{tex,bib} tmp/
    ${pkgs.texliveFull}/bin/latexmk -cd -pdf tmp/ms.tex
  '';
  installPhase = ''
    datadir="$out/share/${pname}"
    install -Dm755 main.py "$out/bin/${pname}"
    install -Dm644 main.py ms.tex ms.bib -t "$datadir"
    install -Dm644 tmp/ms.pdf "$out/ms.pdf"
    install -Dm644 main.py $out/${python.sitePackages}/${pname}.py
  '';
  meta = {
    description = "A Python and LaTeX template package.";
    mainProgram = pname;
  };
  nativeBuildInputs = [
    pkgs.texliveFull
  ];
  passthru.python = python;
  pname = baseNameOf ./.;
  propagatedBuildInputs = pythonDeps;
  pyproject = false;
  src = ./.;
  strictDeps = true;
  version = "0.0.0";
}
