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
    ];
    src = ../.. + "/packages/${packageName}";
  }
  ''
    export LLVM_COV='${pkgs.lib.getExe' pkgs.llvmPackages.llvm "llvm-cov"}'
    export LLVM_PROFDATA='${pkgs.lib.getExe' pkgs.llvmPackages.llvm "llvm-profdata"}'
    workspace="$PWD/workspace"
    cp -R --no-preserve=mode "$src" "$workspace"
    cp "${packageDrv.passthru.cargoToml}" "$workspace/Cargo.toml"
    ln -s "${cargoDeps}" "$workspace/cargo-vendor-dir"
    install -Dm644 "${cargoDeps}/.cargo/config.toml" "$workspace/.cargo/config.toml"
    substituteInPlace "$workspace/.cargo/config.toml" \
      --replace-warn "@vendor@" "${cargoDeps}"
    mkdir -p "$out"
    export PACKAGE_E2E_EXECUTABLE="${packageDrv}/bin/${packageName}"
    cd "$workspace"
    eval "$(cargo llvm-cov show-env --sh)"
    cargo llvm-cov clean --profraw-only
    NEXTEST_TEST_THREADS=1 cargo nextest run
    cargo llvm-cov report --json --summary-only --output-path "$out/report.json"
    covered="$(jq -r '.data[0].totals.lines.covered' "$out/report.json")"
    total="$(jq -r '.data[0].totals.lines.count' "$out/report.json")"
    test "$covered" != null && test "$total" != null
    printf 'coverage\tlines\t%s\t%s\n' "$covered" "$total" > "$out/coverage-summary.tsv"
  ''
