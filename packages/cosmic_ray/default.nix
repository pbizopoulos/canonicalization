{
  pkgs ? import <nixpkgs> { },
}:
let
  exit-codes = pkgs.python3Packages.buildPythonPackage rec {
    format = "wheel";
    pname = "exit_codes";
    pythonImportsCheck = [
      pname
    ];
    src = pkgs.python3Packages.fetchPypi rec {
      inherit
        pname
        version
        format
        ;
      dist = python;
      python = "py2.py3";
      sha256 = "CURIRHcgQ/m+IhKIVgiHkr4Pkc2+Jpr5krEzilRk3IU=";
    };
    version = "1.3.0";
  };
  qprompt = pkgs.python3Packages.buildPythonPackage rec {
    pname = "qprompt";
    pyproject = false;
    src = pkgs.python3Packages.fetchPypi rec {
      inherit
        pname
        version
        ;
      sha256 = "a375510899d7ccec143e919aef41c853afc61d9a43426c206595362d981cd171";
    };
    version = "0.16.3";
  };
in
pkgs.python3Packages.buildPythonApplication rec {
  format = "wheel";
  installCheckPhase = ''
    runHook preInstallCheck
    set -o pipefail
    "$out/bin/cosmic-ray" --help 2>&1 | grep -F "cosmic-ray"
    runHook postInstallCheck
  '';
  meta.mainProgram = "cosmic-ray";
  pname = baseNameOf ./.;
  propagatedBuildInputs = [
    exit-codes
    pkgs.python3Packages.aiohttp
    pkgs.python3Packages.anybadge
    pkgs.python3Packages.attrs
    pkgs.python3Packages.click
    pkgs.python3Packages.decorator
    pkgs.python3Packages.gitpython
    pkgs.python3Packages.parso
    pkgs.python3Packages.rich
    pkgs.python3Packages.sqlalchemy
    pkgs.python3Packages.stevedore
    pkgs.python3Packages.toml
    pkgs.python3Packages.yattag
    qprompt
  ];
  pythonImportsCheck = [
    pname
  ];
  src = pkgs.python3Packages.fetchPypi rec {
    inherit
      pname
      version
      format
      ;
    dist = python;
    python = "py3";
    sha256 = "RIr35qo2W9uyVBBrkHpneVD8eF93RauaZZ1HNShbswc=";
  };
  strictDeps = true;
  version = "8.4.6";
}
