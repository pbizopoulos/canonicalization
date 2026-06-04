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
    if [ "''${DEBUG:-0}" = "1" ]; then
      cd "packages/${pname}"
      cargo test --locked
    fi
    exec "@wrappedBin@" "$@"
  '';
in
pkgs.rustPlatform.buildRustPackage {
  inherit pname;
  cargoHash = "sha256-4Tpc7xOULVqqUzlg2G581bdTxP6SaHR7n9vdq8rqxX0=";
  doInstallCheck = pkgs.stdenv.isLinux;
  env = {
    RUSTDOCFLAGS = "-D warnings";
    RUSTFLAGS = "-D warnings";
  };
  installCheckPhase = ''
    runHook preInstallCheck
    test -x "$out/bin/${pname}"
    workspace="$PWD/installcheck"
    mkdir -p "$workspace"
    printf 'line1\n\nline2\n' > "$workspace/input.txt"
    "$out/bin/${pname}" "$workspace"
    test "$(wc -l < "$workspace/input.txt")" -eq 2
    grep -Fxq "line1" "$workspace/input.txt"
    grep -Fxq "line2" "$workspace/input.txt"
    runHook postInstallCheck
  '';
  meta.mainProgram = pname;
  passthru.rustCheckNativeBuildInputs = [
    pkgs.cargo
    pkgs.rustc
    pkgs.stdenv.cc
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
