{
  inputs,
  pkgs,
  ...
}:
let
  checkName = builtins.baseNameOf ./.;
  packageDrv = inputs.self.packages.${pkgs.stdenv.system}.${packageName};
  packageName = pkgs.lib.removeSuffix "_coverage" checkName;
  pythonEnv = packageDrv.python.withPackages (
    _:
    pkgs.lib.unique (
      (packageDrv.propagatedBuildInputs or [ ])
      ++ [
        packageDrv.python.pkgs.coverage
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
    mkdir -p coverage
    cp -R --no-preserve=mode "$src" workspace
    cd workspace
    python -m coverage run -m unittest main.py
    python -m coverage report | tee "$OLDPWD/coverage/summary.txt"
    python -m coverage html -d "$OLDPWD/coverage/html"
    touch "$out"
  ''
