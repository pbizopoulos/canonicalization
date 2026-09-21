{ inputs, pkgs, ... }:
let
  checkName = baseNameOf ./.;
  dependencyInputs = pkgs.lib.concatMap (name: packageDrv.${name} or [ ]) [
    "buildInputs"
    "checkInputs"
    "nativeBuildInputs"
    "nativeCheckInputs"
    "propagatedBuildInputs"
    "propagatedNativeBuildInputs"
  ];
  packageDrv = inputs.self.packages.${pkgs.stdenv.system}.${packageName};
  packageName = pkgs.lib.removeSuffix "_coverage" checkName;
  pythonEnv = packageDrv.python.withPackages (
    ps:
    packageDrv.propagatedBuildInputs
    ++ [
      ps.coverage
      ps.hypothesis
      ps.pytest
    ]
  );
in
pkgs.runCommand checkName
  {
    inherit (packageDrv) src;
    nativeBuildInputs = dependencyInputs ++ [ pythonEnv ];
  }
  ''
    export HOME="$(mktemp -d)"
    mkdir -p "$out/html" packages "$TMPDIR/coverage-startup"
    ln -s "$src" "packages/${packageName}"
    export COVERAGE_FILE="$out/.coverage"
    export COVERAGE_PROCESS_START="$TMPDIR/coverage.ini"
    cat > "$COVERAGE_PROCESS_START" <<EOF
    [run]
    parallel = true
    data_file = $out/.coverage
    source =
        $src
        ${packageDrv}/${packageDrv.python.sitePackages}/${packageDrv.pname}
    omit =
        */test_main.py
        */prm/*
    EOF
    printf '%s\n' 'import coverage; coverage.process_startup()' > "$TMPDIR/coverage-startup/sitecustomize.py"
    export PYTHONPATH="$TMPDIR/coverage-startup:$PWD:${pythonEnv}/${packageDrv.python.sitePackages}:$PYTHONPATH"
    cd "$out"
    PACKAGE_E2E_EXECUTABLE="${pkgs.lib.getExe packageDrv}" python -c 'import sys; from hypothesis import Phase, settings; settings.register_profile("coverage", phases=[Phase.explicit]); settings.load_profile("coverage"); import pytest; sys.exit(pytest.main(sys.argv[1:]))' -p no:cacheprovider --import-mode=importlib "$src/test_main.py"
    unset COVERAGE_PROCESS_START
    python -m coverage combine --rcfile="$TMPDIR/coverage.ini"
    python - <<'PYTHON'
    import os
    import coverage
    data = coverage.CoverageData()
    data.read()
    mapped = coverage.CoverageData(basename=".coverage-mapped")
    installed = "${packageDrv}/${packageDrv.python.sitePackages}/${packageDrv.pname}/__init__.py"
    mapped.update(data, map_path=lambda path: os.environ["src"] + "/main.py" if path == installed else path)
    mapped.write()
    os.replace(mapped.data_filename(), data.data_filename())
    PYTHON
    python -m coverage html --rcfile="$TMPDIR/coverage.ini" -d "$out/html"
    python -m coverage json --rcfile="$TMPDIR/coverage.ini" -o "$out/coverage.json"
  ''
