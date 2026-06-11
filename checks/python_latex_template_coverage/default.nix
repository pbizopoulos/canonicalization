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
    packageDrv.propagatedBuildInputs
    ++ [
      packageDrv.python.pkgs.pytest-cov
    ]
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
    workspace="$PWD/workspace"
    coverageDir="$PWD/coverage"
    mkdir -p "$coverageDir"
    cp -R --no-preserve=mode "$src" "$workspace"
    cd "$workspace"
    python -m pytest --cov=. --cov-report term --cov-report "html:$coverageDir/html" main.py
    touch "$out"
  ''
