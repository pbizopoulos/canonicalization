{
  inputs,
  pkgs ? import <nixpkgs> { },
}:
let
  python = pkgs.python312;
in
python.pkgs.buildPythonApplication rec {
  format = "wheel";
  installCheckPhase = ''
    runHook preInstallCheck
    test -x "$out/bin/cosmic-ray"
    set -o pipefail
    "$out/bin/cosmic-ray" --help 2>&1 | grep -F "cosmic-ray"
    runHook postInstallCheck
  '';
  meta.mainProgram = "cosmic-ray";
  pname = baseNameOf ./.;
  propagatedBuildInputs = [
    inputs.self.packages.${pkgs.stdenv.system}.exit_codes
    inputs.self.packages.${pkgs.stdenv.system}.qprompt
    python.pkgs.aiohttp
    python.pkgs.anybadge
    python.pkgs.attrs
    python.pkgs.click
    python.pkgs.decorator
    python.pkgs.gitpython
    python.pkgs.parso
    python.pkgs.rich
    python.pkgs.sqlalchemy
    python.pkgs.stevedore
    python.pkgs.toml
    python.pkgs.yattag
  ];
  pythonImportsCheck = [
    pname
  ];
  src = pkgs.fetchurl {
    sha256 = "RIr35qo2W9uyVBBrkHpneVD8eF93RauaZZ1HNShbswc=";
    url = "https://files.pythonhosted.org/packages/9b/45/f78505d164e38a1bf00bd2ec8f54f340969c61c4d2486aaa2e18090a1ccf/cosmic_ray-${version}-py3-none-any.whl";
  };
  version = "8.4.6";
}
