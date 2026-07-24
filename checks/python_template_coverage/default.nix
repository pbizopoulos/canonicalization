{
  inputs,
  pkgs,
  ...
}:
let
  checkName = builtins.baseNameOf ./.;
  packageDrv = inputs.self.packages.${pkgs.stdenv.system}.${packageName};
  packageName = pkgs.lib.removeSuffix "_coverage" checkName;
  profilingDrv = import ((inputs.canonicalization or inputs.self) + "/packages/pytest_profiling") {
    inherit pkgs;
  };
  pythonEnv = packageDrv.python.withPackages (
    _:
    packageDrv.propagatedBuildInputs
    ++ [
      packageDrv.python.pkgs.pytest-cov
      profilingDrv
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
    python -m pytest -p no:cacheprovider --profile --pstats-dir "$out/prof" --cov="$src" --cov-report term --cov-report "json:$out/report.json" --cov-report "html:$out/html" --junitxml="$out/junit.xml" "$src/main.py"
    python - "$src/main.py" "$out/report.json" "$out/junit.xml" "$out/coverage-summary.tsv" "$out/test-timings.tsv" "$out/prof/combined.prof" "$out/profile-report.txt" "$out/profile-summary.tsv" <<'PY'
    import ast
    import json
    import pathlib
    import pstats
    import re
    import sys
    import xml.etree.ElementTree as ET
    source_path, report_path, junit_path, coverage_path, timings_path, profile_path, profile_report_path, profile_summary_path = map(pathlib.Path, sys.argv[1:])
    totals = json.loads(report_path.read_text())["totals"]
    coverage_path.write_text(
        f"coverage-v1\tstatements\t{totals['covered_lines']}\t{totals['num_statements']}\n"
    )
    specifications = {}
    for node in ast.parse(source_path.read_text()).body:
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)) and node.name.startswith("test_"):
            words = re.sub(r"^test_(?:property_)?", "", node.name).replace("_", " ")
            specifications[node.name] = ast.get_docstring(node) or words[:1].upper() + words[1:] + "."
    timing_lines = []
    for test_case in sorted(ET.parse(junit_path).iter("testcase"), key=lambda element: element.attrib["name"]):
        test_name = test_case.attrib["name"].split("[", 1)[0]
        timing_lines.append(f"test\t{test_case.attrib['time']}\t{specifications[test_name]}")
    timings_path.write_text("\n".join(timing_lines) + "\n")
    with profile_report_path.open("w") as stream:
        profile_stats = pstats.Stats(str(profile_path), stream=stream)
        profile_stats.sort_stats("cumulative").print_stats(20)
    profile_summary_path.write_text(
        "\n".join([f"profile-v1\ttotal-seconds\t{profile_stats.total_tt}", *timing_lines]) + "\n"
    )
    PY
  ''
