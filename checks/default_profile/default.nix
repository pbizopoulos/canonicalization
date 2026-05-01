{
  inputs,
  pkgs,
  ...
}:
let
  checkName = builtins.baseNameOf ./.;
  packageName = "default";
  repoRoot = ../../.;
in
pkgs.runCommand "${checkName}"
  {
    nativeBuildInputs = [
      inputs.self.packages.${pkgs.stdenv.system}.${packageName}
      pkgs.git
      pkgs.perf
    ];
    src = repoRoot;
  }
  ''
    export HOME="$PWD"
    workspace="$PWD/workspace"
    mkdir -p "$workspace/target"
    run_cmd() {
      env CANONICALIZATION_ROOT="$src" default "$workspace/target" --templates rust
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
