{
  inputs,
  pkgs,
  ...
}:
let
  checkName = builtins.baseNameOf ./.;
  packageName = pkgs.lib.removeSuffix "-profile" checkName;
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
    temp_dir="$PWD/workspace"
    mkdir -p "$temp_dir"
    printf 'line1\n\nline2\n' > "$temp_dir/test.txt"
    run_cmd() {
      remove-empty-lines "$temp_dir"
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
