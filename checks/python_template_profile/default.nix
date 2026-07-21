{
  inputs,
  pkgs,
  ...
}:
let
  checkName = builtins.baseNameOf ./.;
  packageDrv = inputs.self.packages.${pkgs.stdenv.system}.${packageName};
  packageName = pkgs.lib.removeSuffix "_profile" checkName;
  pythonEnv = packageDrv.python.withPackages (
    _:
    packageDrv.propagatedBuildInputs
    ++ [
      packageDrv.python.pkgs.pyinstrument
    ]
  );
in
pkgs.runCommand checkName
  {
    nativeBuildInputs = [
      pkgs.time
      pythonEnv
    ];
    src = ../.. + "/packages/${packageName}";
  }
  ''
    export HOME="$(mktemp -d)"
    mkdir -p "$out"
    PYTHONWARNINGS=error ${pkgs.time}/bin/time -f %e -o "$out/total-seconds" \
      pyinstrument -r text -o "$out/report.txt" -m pytest -p no:cacheprovider --junitxml="$out/junit.xml" "$src/main.py"
    python - "$src/main.py" "$out/junit.xml" "$out/total-seconds" "$out/profile-summary.tsv" <<'PY'
    import ast
    import pathlib
    import re
    import sys
    import xml.etree.ElementTree as ET
    source_path, junit_path, total_path, output_path = map(pathlib.Path, sys.argv[1:])
    specifications = {}
    for node in ast.parse(source_path.read_text()).body:
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)) and node.name.startswith("test_"):
            words = re.sub(r"^test_(?:property_)?", "", node.name).replace("_", " ")
            specifications[node.name] = ast.get_docstring(node) or words[:1].upper() + words[1:] + "."
    lines = [f"profile-v1\ttotal-seconds\t{total_path.read_text().strip()}"]
    for test_case in sorted(ET.parse(junit_path).iter("testcase"), key=lambda element: element.attrib["name"]):
        test_name = test_case.attrib["name"].split("[", 1)[0]
        lines.append(f"test\t{test_case.attrib['time']}\t{specifications[test_name]}")
    output_path.write_text("\n".join(lines) + "\n")
    PY
  ''
