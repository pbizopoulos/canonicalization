{
  pkgs,
  ...
}:
let
  checkName = builtins.baseNameOf ./.;
  packageName = "python_latex_template";
  pythonDeps = [
    pkgs.python3Packages.matplotlib
    pkgs.python3Packages.pandas
  ];
  pythonEnv = pkgs.python3.withPackages (
    _:
    pythonDeps
    ++ [
      pkgs.python3Packages.coverage
      pkgs.python3Packages.hypothesis
    ]
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
    coverage_dir="$PWD/coverage"
    mkdir -p "$coverage_dir"
    DEBUG=1 PYTHONWARNINGS=error coverage run --source="$src" "$src/main.py"
    coverage report | tee "$coverage_dir/summary.txt"
    coverage html -d "$coverage_dir/html"
    touch "$out"
  ''
