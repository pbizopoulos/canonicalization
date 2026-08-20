{
  inputs,
  pkgs,
  ...
}:
let
  inherit (packageDrv) cargoDeps;
  checkName = baseNameOf ./.;
  packageDrv = inputs.self.packages.${pkgs.stdenv.system}.${packageName};
  packageName = pkgs.lib.removeSuffix "-coverage" checkName;
in
pkgs.runCommand checkName
  {
    nativeBuildInputs = packageDrv.passthru.rustCheckNativeBuildInputs ++ [
      pkgs.cargo-llvm-cov
      pkgs.cargo-nextest
      pkgs.jq
      pkgs.llvmPackages.llvm
      pkgs.perf
      pkgs.python3
      pkgs.time
    ];
    src = ../.. + "/packages/${packageName}";
  }
  ''
    export LLVM_COV='${pkgs.lib.getExe' pkgs.llvmPackages.llvm "llvm-cov"}'
    export LLVM_PROFDATA='${pkgs.lib.getExe' pkgs.llvmPackages.llvm "llvm-profdata"}'
    workspace="$PWD/workspace"
    cp -R --no-preserve=mode "$src" "$workspace"
    install -Dm644 "${cargoDeps}/.cargo/config.toml" "$workspace/.cargo/config.toml"
    substituteInPlace "$workspace/.cargo/config.toml" \
      --replace-fail "@vendor@" "${cargoDeps}"
    mkdir -p "$out" "$workspace/.config"
    export PACKAGE_E2E_EXECUTABLE="${packageDrv}/bin/${packageName}"
    cat > "$workspace/.config/nextest.toml" <<EOF
    [profile.profile.junit]
    path = "$out/junit.xml"
    EOF
    cd "$workspace"
    eval "$(cargo llvm-cov show-env --sh)"
    cargo nextest list --profile profile >/dev/null
    cargo llvm-cov clean --profraw-only
    if perf stat -e cpu-clock true >/dev/null 2>&1; then
      NEXTEST_TEST_THREADS=1 ${pkgs.time}/bin/time -f %e -o "$workspace/total-seconds" \
        perf record --no-buildid-mmap --call-graph dwarf -e cpu-clock -o "$out/perf.data" -- \
        cargo nextest run --profile profile
      perf report --stdio -i "$out/perf.data" > "$out/profile-report.txt"
    else
      echo "perf is unavailable in this environment; timings were still recorded." > "$out/profile-report.txt"
      NEXTEST_TEST_THREADS=1 ${pkgs.time}/bin/time -f %e -o "$workspace/total-seconds" \
        cargo nextest run --profile profile
    fi
    cargo llvm-cov report --json --summary-only --output-path "$out/report.json"
    covered="$(jq -r '.data[0].totals.lines.covered' "$out/report.json")"
    total="$(jq -r '.data[0].totals.lines.count' "$out/report.json")"
    test "$covered" != null && test "$total" != null
    printf 'coverage-v1\tlines\t%s\t%s\n' "$covered" "$total" > "$out/coverage-summary.tsv"
    python - "$out/junit.xml" "$workspace/total-seconds" "$out/profile-summary.tsv" <<'PY'
    import json
    import pathlib
    import sys
    import xml.etree.ElementTree as ET
    junit_path, total_path, profile_path = map(pathlib.Path, sys.argv[1:])
    lines = [f"profile-v2\ttotal-seconds\t{total_path.read_text().strip()}"]
    for test_case in sorted(ET.parse(junit_path).iter("testcase"), key=lambda element: element.attrib["name"]):
        identifier = test_case.attrib["name"].rsplit("::", 1)[-1]
        lines.append(f"test\t{test_case.attrib['time']}\t{json.dumps(identifier, ensure_ascii=False)}\tnull")
    profile_path.write_text("\n".join(lines) + "\n")
    PY
  ''
