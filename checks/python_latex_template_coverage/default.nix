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
    mkdir -p "$out/html"
    python -m pytest --cov="$src" --cov-report term --cov-report "json:$out/report.json" --cov-report "html:$out/html" "$src/main.py"
    python - "$out/report.json" "$out/coverage-summary.tsv" <<'PY'
    import json
    import pathlib
    import sys
    totals = json.loads(pathlib.Path(sys.argv[1]).read_text())["totals"]
    pathlib.Path(sys.argv[2]).write_text(
        f"coverage-v1\tstatements\t{totals['covered_lines']}\t{totals['num_statements']}\n"
    )
    PY
  ''
