{
  pkgs,
  ...
}:
let
  checkName = builtins.baseNameOf ./.;
  packageName = "python_template";
  pyPkgs = pkgs.python312Packages;
in
pkgs.runCommand "${checkName}"
  {
    nativeBuildInputs = [
      pkgs.coreutils
      pkgs.python312
      pyPkgs.coverage
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
