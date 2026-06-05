{
  pkgs ? import <nixpkgs> { },
}:
let
  python = pkgs.python312;
in
python.pkgs.buildPythonPackage rec {
  format = "setuptools";
  pname = baseNameOf ./.;
  propagatedBuildInputs = [ ];
  pythonImportsCheck = [
    pname
  ];
  src = python.pkgs.fetchPypi rec {
    inherit
      pname
      version
      ;
    sha256 = "a375510899d7ccec143e919aef41c853afc61d9a43426c206595362d981cd171";
  };
  version = "0.16.3";
}
