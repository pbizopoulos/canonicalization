{
  inputs,
  pkgs,
  ...
}:
let
  checkName = builtins.baseNameOf ./.;
  packageDrv = inputs.self.packages.${pkgs.stdenv.system}.${packageName};
  packageName = pkgs.lib.removeSuffix "-profile" checkName;
in
pkgs.runCommand "${checkName}"
  {
    nativeBuildInputs = [
      packageDrv
      pkgs.perf
    ];
    src = ../.. + "/packages/${packageName}";
  }
  ''
    workspace="$PWD/workspace"
    mkdir -p "$workspace"
    printf 'line1\n\nline2\n' > "$workspace/test.txt"
    run_cmd() {
      remove-empty-lines "$workspace"
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
