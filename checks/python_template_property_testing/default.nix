{
  inputs,
  pkgs,
  ...
}:
let
  checkName = builtins.baseNameOf ./.;
  packageName = pkgs.lib.removeSuffix "_property_testing" checkName;
  packageDrv = inputs.self.packages.${pkgs.stdenv.system}.${packageName};
  packagePythonDeps = packageDrv.propagatedBuildInputs or [ ];
  python = packageDrv.python or pkgs.python3;
  pythonEnv = python.withPackages (
    _: pkgs.lib.unique (packagePythonDeps ++ [ python.pkgs.hypothesis ])
  );
in
pkgs.runCommand "${checkName}"
  {
    nativeBuildInputs = [
      pkgs.coreutils
      pythonEnv
    ];
    src = ../../packages/${packageName};
  }
  ''
    export HOME="$(mktemp -d)"
    export PYTHONWARNINGS=error
    PYTHONPATH="$src" python -m unittest -v main.PropertyTests
    touch "$out"
  ''
