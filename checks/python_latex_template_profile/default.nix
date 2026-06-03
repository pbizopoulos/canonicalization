{
  inputs,
  pkgs,
  ...
}:
let
  checkName = builtins.baseNameOf ./.;
  packageName = pkgs.lib.removeSuffix "_profile" checkName;
  packageDrv = inputs.self.packages.${pkgs.stdenv.system}.${packageName};
  packagePythonDeps = packageDrv.propagatedBuildInputs or [ ];
  python = packageDrv.python or pkgs.python3;
  pythonEnv = python.withPackages (
    _: pkgs.lib.unique (packagePythonDeps ++ [ python.pkgs.pyinstrument ])
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
    DEBUG=1 PYTHONWARNINGS=error pyinstrument "$src/main.py"
    touch "$out"
  ''
