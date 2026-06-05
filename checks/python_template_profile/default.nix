{
  inputs,
  pkgs,
  ...
}:
let
  checkName = builtins.baseNameOf ./.;
  packageDrv = inputs.self.packages.${pkgs.stdenv.system}.${packageName};
  packageName = pkgs.lib.removeSuffix "_profile" checkName;
  python = packageDrv.python or pkgs.python3;
  pythonEnv = python.withPackages (
    _:
    pkgs.lib.unique (
      (packageDrv.propagatedBuildInputs or [ ])
      ++ [
        python.pkgs.pyinstrument
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
