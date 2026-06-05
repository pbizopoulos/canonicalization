{
  inputs,
  pkgs,
  ...
}:
let
  checkName = builtins.baseNameOf ./.;
  packageDrv = inputs.self.packages.${pkgs.stdenv.system}.${packageName};
  packageName = pkgs.lib.removeSuffix "_property_testing" checkName;
  python = packageDrv.python or pkgs.python3;
  pythonEnv = python.withPackages (
    _:
    pkgs.lib.unique (
      (packageDrv.propagatedBuildInputs or [ ])
      ++ [
        python.pkgs.hypothesis
      ]
    )
  );
in
pkgs.runCommand checkName
  {
    nativeBuildInputs = [
      pythonEnv
    ];
    src = ../.. + "/packages/${packageName}";
  }
  ''
    export HOME="$(mktemp -d)"
    export PYTHONWARNINGS=error
    PYTHONPATH="$src" python -m unittest -v main.PropertyTests
    touch "$out"
  ''
