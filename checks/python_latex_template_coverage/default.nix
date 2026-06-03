{
  inputs,
  pkgs,
  ...
}:
let
  checkName = builtins.baseNameOf ./.;
  packageDrv = inputs.self.packages.${pkgs.stdenv.system}.${packageName};
  packageName = pkgs.lib.removeSuffix "_coverage" checkName;
  packagePythonDeps = packageDrv.propagatedBuildInputs or [ ];
  python = packageDrv.python or pkgs.python3;
  pythonEnv = python.withPackages (
    _:
    pkgs.lib.unique (
      packagePythonDeps
      ++ [
        python.pkgs.coverage
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
    coverage_dir="$PWD/coverage"
    mkdir -p "$coverage_dir"
    DEBUG=1 PYTHONWARNINGS=error coverage run --source="$src" "$src/main.py"
    coverage report | tee "$coverage_dir/summary.txt"
    coverage html -d "$coverage_dir/html"
    touch "$out"
  ''
