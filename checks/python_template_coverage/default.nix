{
  inputs,
  pkgs,
  ...
}:
let
  checkName = baseNameOf ./.;
  packageDrv = inputs.self.packages.${pkgs.stdenv.system}.${packageName};
  packageName = pkgs.lib.removeSuffix "_coverage" checkName;
  pythonEnv = packageDrv.python.withPackages (
    _:
    packageDrv.propagatedBuildInputs
    ++ [
      packageDrv.python.pkgs.pytest
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
    PACKAGE_E2E_EXECUTABLE="${packageDrv}/bin/${packageName}" python -m pytest -p no:cacheprovider --cov="$src" --cov-report term --cov-report "json:$out/report.json" --cov-report "html:$out/html" "$src/main.py"
    python - "$out/report.json" "$out/coverage-summary.tsv" <<'PY'
    import json
    import pathlib
    import sys
    report_path, coverage_path = map(pathlib.Path, sys.argv[1:])
    totals = json.loads(report_path.read_text())["totals"]
    coverage_path.write_text(
        f"coverage\tstatements\t{totals['covered_lines']}\t{totals['num_statements']}\n"
    )
    PY
  ''
