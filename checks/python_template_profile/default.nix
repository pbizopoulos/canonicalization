{
  inputs,
  pkgs,
  ...
}:
let
  checkName = builtins.baseNameOf ./.;
  packageDrv = inputs.self.packages.${pkgs.stdenv.system}.${packageName};
  packageName = pkgs.lib.removeSuffix "_profile" checkName;
  pythonEnv = packageDrv.python.withPackages (
    _:
    pkgs.lib.unique (
      packageDrv.propagatedBuildInputs
      ++ [
        packageDrv.python.pkgs.pyinstrument
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
    PYTHONWARNINGS=error pyinstrument "$src/main.py"
    touch "$out"
  ''
