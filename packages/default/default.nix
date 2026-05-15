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
        pkgs.rustc
        pkgs.stdenv.cc
      ]
    }':"$PATH"
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
        cd "$source_root"
        cargo test --locked
      fi
    fi
    exec "@wrappedBin@" "$@"
  '';
in
pkgs.rustPlatform.buildRustPackage {
  inherit pname;
  cargoHash = "sha256-PYXmMBz8X00zGWH2UtpVQBK8r4k8/drXUIpBF6aSbms=";
  doInstallCheck = pkgs.stdenv.isLinux;
  env = {
    RUSTDOCFLAGS = "-D warnings";
    RUSTFLAGS = "-D warnings";
  };
  installCheckPhase = ''
    runHook preInstallCheck
    test -x "$out/bin/${pname}"
    mkdir -p "$PWD/installcheck-root" "$PWD/installcheck-out"
    CANONICALIZATION_ROOT="$PWD/installcheck-root" \
      "$out/bin/${pname}" "$PWD/installcheck-out"
    test -d "$PWD/installcheck-out/.git"
    runHook postInstallCheck
  '';
  meta.mainProgram = pname;
  nativeInstallCheckInputs = [
    pkgs.git
  ];
  postInstall = ''
    mv "$out/bin/${pname}" "$out/bin/.${pname}-wrapped"
    install -m755 ${wrapperScript} "$out/bin/${pname}"
    substituteInPlace "$out/bin/${pname}" \
      --replace-fail "@wrappedBin@" "$out/bin/.${pname}-wrapped"
  '';
  src = ./.;
  strictDeps = true;
  version = "0.0.0";
}
