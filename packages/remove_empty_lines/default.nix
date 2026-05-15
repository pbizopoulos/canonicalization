{
  pkgs ? import <nixpkgs> { },
}:
pkgs.rustPlatform.buildRustPackage rec {
  cargoHash = "sha256-rNhwPCFAZ/k+hrFJFmgMFcKKXJrHjYUuOCdjiX2Tex0=";
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
  pname = baseNameOf ./.;
  src = ./.;
  strictDeps = true;
  version = "0.0.0";
}
