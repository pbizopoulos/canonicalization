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
      packageDrv.propagatedBuildInputs
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
    workspace="$PWD/workspace"
    coverageDir="$PWD/coverage"
    mkdir -p "$coverageDir"
    cp -R --no-preserve=mode "$src" "$workspace"
    cd "$workspace"
    python -m coverage run -m unittest main.py
    python -m coverage report | tee "$coverageDir/summary.txt"
    python -m coverage html -d "$coverageDir/html"
    touch "$out"
  ''
