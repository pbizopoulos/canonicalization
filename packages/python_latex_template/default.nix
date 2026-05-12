{
  pkgs ? import <nixpkgs> { },
}:
let
  pythonDeps = [
    pkgs.python3Packages.matplotlib
    pkgs.python3Packages.pandas
  ];
  pythonEnv = pkgs.python3.withPackages (_: pythonDeps);
  runtimeScript = pkgs.writeShellScript "python_latex_template" ''
    set -euo pipefail
    script_dir="$(cd "$(dirname "$0")" && pwd)"
    rm -rf tmp
    ${pythonEnv}/bin/python3 "$script_dir/main.py"
    cp "$script_dir"/ms.{tex,bib} tmp/
    ${pkgs.texliveFull}/bin/latexmk -cd -pdf tmp/ms.tex
  '';
in
pkgs.python3Packages.buildPythonPackage rec {
  installPhase = ''
    install -Dm644 ./main.py $out/bin/main.py
    install -Dm644 ./ms.tex $out/bin/ms.tex
    install -Dm644 ./ms.bib $out/bin/ms.bib
    install -Dm755 ${runtimeScript} $out/bin/${pname}
  '';
  meta.mainProgram = pname;
  pname = builtins.baseNameOf src;
  propagatedBuildInputs = pythonDeps ++ [
    pkgs.texliveFull
  ];
  pyproject = false;
  src = ./.;
  version = "0.0.0";
}
