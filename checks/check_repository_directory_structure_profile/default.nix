{
  inputs,
  pkgs,
  ...
}:
let
  checkName = builtins.baseNameOf ./.;
  packageName = "check_repository_directory_structure";
in
pkgs.runCommand "${checkName}"
  {
    nativeBuildInputs = [
      pkgs.perf
      pkgs.git
      inputs.self.packages.${pkgs.stdenv.system}.${packageName}
    ];
    src = ../../packages/${packageName};
  }
  ''
    temp_dir="$PWD/repo"
    mkdir -p "$temp_dir"
    cd "$temp_dir"
    git init -b main
    git config user.email test@example.com
    git config user.name "Test User"
    printf 'test\n' > flake.nix
    git add flake.nix
    git commit -m initial
    run_cmd() {
      check_repository_directory_structure flake.nix
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
