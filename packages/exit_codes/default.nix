{
  pkgs ? import <nixpkgs> { },
}:
let
  python = pkgs.python312;
in
python.pkgs.buildPythonPackage rec {
  format = "wheel";
  meta.description = "Platform-independent exit codes.";
  pname = baseNameOf ./.;
  propagatedBuildInputs = [ ];
  pythonImportsCheck = [
    pname
  ];
  src = python.pkgs.fetchPypi rec {
    inherit
      format
      pname
      version
      ;
    dist = python;
    python = "py2.py3";
    sha256 = "CURIRHcgQ/m+IhKIVgiHkr4Pkc2+Jpr5krEzilRk3IU=";
  };
  version = "1.3.0";
}
