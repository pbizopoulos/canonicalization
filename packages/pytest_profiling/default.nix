{
  pkgs ? import <nixpkgs> { },
}:
let
  python = pkgs.python3;
in
python.pkgs.buildPythonPackage rec {
  format = "wheel";
  meta = {
    description = "Profiling plugin for pytest.";
    homepage = "https://github.com/man-group/pytest-plugins";
    license = pkgs.lib.licenses.mit;
  };
  pname = baseNameOf ./.;
  propagatedBuildInputs = [
    python.pkgs.gprof2dot
    python.pkgs.pytest
    python.pkgs.six
  ];
  pythonImportsCheck = [
    pname
  ];
  src = python.pkgs.fetchPypi rec {
    inherit
      format
      pname
      version
      ;
    dist = "py3";
    python = "py3";
    sha256 = "sha256-PdhxOpYpi0LYPej1lR3zraPmGz5dKgaVZoQXVSnheuo=";
  };
  version = "1.8.1";
}
