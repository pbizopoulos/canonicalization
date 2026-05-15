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
  buildInputs = [
    pkgs.openssl
    pkgs.zlib
  ];
  cargoHash = "sha256-w4LvRdS5zom3TfCNNBYtq44LIPTJ7YYpemfuyfxqAPs=";
  doInstallCheck = pkgs.stdenv.isLinux;
  env = {
    RUSTDOCFLAGS = "-D warnings";
    RUSTFLAGS = "-D warnings";
  };
  installCheckPhase = ''
    runHook preInstallCheck
    test -x "$out/bin/${pname}"
    NIX_BUILD_TOP=1 "$out/bin/${pname}"
    runHook postInstallCheck
  '';
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
  strictDeps = true;
  version = "0.0.0";
}
