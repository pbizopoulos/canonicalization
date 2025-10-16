{
  pkgs ? import <nixpkgs> { },
}:
pkgs.python312Packages.buildPythonPackage rec {
  installPhase = ''mkdir -p $out/bin && cp ./main.py $out/bin/${pname}'';
  meta.mainProgram = pname;
  pname = builtins.baseNameOf src;
  propagatedBuildInputs = [
    pkgs.python312Packages.fire
    pkgs.python312Packages.fqdn
    pkgs.python312Packages.gitpython
  ];
  pyproject = false;
  src = ./.;
  version = "0.0.0";
}
