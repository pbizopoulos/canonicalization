{
  inputs,
  pkgs,
  ...
}:
let
  inherit (packageDrv) cargoDeps;
  checkName = builtins.baseNameOf ./.;
  packageDrv = inputs.self.packages.${pkgs.stdenv.system}.${packageName};
  packageName = pkgs.lib.removeSuffix "-profile" checkName;
  rustBaseInputs = packageDrv.passthru.rustCheckNativeBuildInputs;
in
pkgs.runCommand "${checkName}"
  {
    nativeBuildInputs = rustBaseInputs ++ [
      pkgs.cargo-nextest
      pkgs.perf
      pkgs.python3
      pkgs.time
    ];
    src = ../.. + "/packages/${packageName}";
  }
  ''
    workspace="$PWD/workspace"
    cp -R --no-preserve=mode "$src" "$workspace"
    install -Dm644 "${cargoDeps}/.cargo/config.toml" "$workspace/.cargo/config.toml"
    substituteInPlace "$workspace/.cargo/config.toml" \
      --replace-fail "@vendor@" "${cargoDeps}"
    mkdir -p "$out" "$workspace/.config"
    cat > "$workspace/.config/nextest.toml" <<EOF
    [profile.profile.junit]
    path = "$out/junit.xml"
    EOF
    cd "$workspace"
    cargo nextest list --profile profile >/dev/null
    if perf stat -e cpu-clock true >/dev/null 2>&1; then
      NEXTEST_TEST_THREADS=1 ${pkgs.time}/bin/time -f %e -o "$out/total-seconds" \
        perf record --no-buildid-mmap --call-graph dwarf -e cpu-clock -o "$out/perf.data" -- \
        cargo nextest run --profile profile
      perf report --stdio -i "$out/perf.data" > "$out/report.txt"
    else
      echo "perf is unavailable in this environment; timings were still recorded." > "$out/report.txt"
      NEXTEST_TEST_THREADS=1 ${pkgs.time}/bin/time -f %e -o "$out/total-seconds" \
        cargo nextest run --profile profile
    fi
    python - "$out/junit.xml" "$out/total-seconds" "$out/profile-summary.tsv" <<'PY'
    import pathlib
    import re
    import sys
    import xml.etree.ElementTree as ET
    junit_path, total_path, output_path = map(pathlib.Path, sys.argv[1:])
    lines = [f"profile-v1\ttotal-seconds\t{total_path.read_text().strip()}"]
    for test_case in sorted(ET.parse(junit_path).iter("testcase"), key=lambda element: element.attrib["name"]):
        identifier = test_case.attrib["name"].rsplit("::", 1)[-1]
        words = re.sub(r"^(?:test|quickcheck)_", "", identifier).replace("_", " ")
        test_name = words[:1].upper() + words[1:] + "."
        lines.append(f"test\t{test_case.attrib['time']}\t{test_name}")
    output_path.write_text("\n".join(lines) + "\n")
    PY
  ''
