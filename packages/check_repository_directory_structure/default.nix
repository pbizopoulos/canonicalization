{
  pkgs ? import <nixpkgs> { },
}:
let
  pname = baseNameOf ./.;
  wrapperScript = pkgs.writeShellScript "${pname}-wrapper" ''
    set -euo pipefail
    export PATH='${
      pkgs.lib.makeBinPath [
        pkgs.cargo
        pkgs.cargo-llvm-cov
        pkgs.cargo-mutants
        pkgs.coreutils
        pkgs.llvmPackages.clang
        pkgs.llvmPackages.llvm
        pkgs.pkg-config
        pkgs.rustc
        pkgs.stdenv.cc
      ]
    }':"$PATH"
    export LIBCLANG_PATH='${pkgs.llvmPackages.libclang.lib}/lib'
    export LLVM_COV='${pkgs.lib.getExe' pkgs.llvmPackages.llvm "llvm-cov"}'
    export LLVM_PROFDATA='${pkgs.lib.getExe' pkgs.llvmPackages.llvm "llvm-profdata"}'
    export PKG_CONFIG_PATH='${
      pkgs.lib.makeSearchPathOutput "dev" "lib/pkgconfig" [
        pkgs.openssl
        pkgs.zlib
      ]
    }'
    resolve_source_root() {
      local candidate
      local current_dir="$PWD"
      if [ -n "''${CANONICALIZATION_ROOT:-}" ]; then
        candidate="$CANONICALIZATION_ROOT/packages/${pname}"
        if [ -f "$candidate/Cargo.toml" ]; then
          printf '%s\n' "$candidate"
          return 0
        fi
      fi
      while [ "$current_dir" != "/" ]; do
        candidate="$current_dir/packages/${pname}"
        if [ -f "$candidate/Cargo.toml" ]; then
          printf '%s\n' "$candidate"
          return 0
        fi
        current_dir="$(dirname "$current_dir")"
      done
      if [ -f "$PWD/Cargo.toml" ]; then
        printf '%s\n' "$PWD"
        return 0
      fi
      return 1
    }
    if [ "''${DEBUG:-0}" = "1" ]; then
      if source_root="$(resolve_source_root)"; then
        coverage_dir="$source_root/tmp/coverage"
        rm -rf "$coverage_dir"
        mkdir -p "$coverage_dir"
        cd "$source_root"
        cargo llvm-cov clean --workspace
        cargo llvm-cov --locked --no-report
        cargo llvm-cov report --html --output-dir "$coverage_dir/html"
        cargo llvm-cov report --summary-only | tee "$coverage_dir/summary.txt"
        mkdir -p "$source_root/tmp"
        rm -rf "$source_root/tmp/mutants.out"
        set +e
        cargo mutants --no-config --colors never --cap-lints true --jobs 1 --output "$source_root/tmp"
        mutation_status=$?
        set -e
        if [ "$mutation_status" -ne 0 ] && [ "$mutation_status" -ne 2 ]; then
          exit "$mutation_status"
        fi
        exit 0
      fi
    fi
    exec "@wrappedBin@" "$@"
  '';
in
pkgs.rustPlatform.buildRustPackage {
  inherit pname;
  buildInputs = [
    pkgs.openssl
    pkgs.zlib
  ];
  cargoHash = "sha256-rTjBv800ZzIx656m8q1XQaTKpmF/F1JAtLN/HtDdEkM=";
  doCheck = pkgs.stdenv.isLinux;
  env.RUSTFLAGS = "-D warnings";
  meta.mainProgram = pname;
  nativeBuildInputs = [
    pkgs.git
    pkgs.pkg-config
  ];
  postInstall = ''
    mv "$out/bin/${pname}" "$out/bin/.${pname}-wrapped"
    install -m755 ${wrapperScript} "$out/bin/${pname}"
    substituteInPlace "$out/bin/${pname}" \
      --replace-fail "@wrappedBin@" "$out/bin/.${pname}-wrapped"
  '';
  src = ./.;
  version = "0.0.0";
}
