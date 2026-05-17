{
  inputs,
  pkgs,
  ...
}:
let
  checkName = builtins.baseNameOf ./.;
  packageName = "rust-template";
in
pkgs.runCommand "${checkName}"
  {
    nativeBuildInputs = [
      pkgs.perf
      inputs.self.packages.${pkgs.stdenv.system}.${packageName}
    ];
    src = ../../packages/${packageName};
  }
  ''
    run_cmd() {
      rust-template
    }
    if perf stat -e cpu-clock true >/dev/null 2>&1; then
      perf record --no-buildid-mmap --call-graph dwarf -e cpu-clock -o perf.data -- \
        run_cmd
      perf report --stdio -i perf.data
    else
      echo "perf is unavailable in this environment; running without profiling."
      run_cmd
    fi
    touch "$out"
  ''
