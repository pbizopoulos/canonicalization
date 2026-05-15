{
  pkgs ? import <nixpkgs> { },
}:
pkgs.rustPlatform.buildRustPackage rec {
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
  pname = baseNameOf ./.;
  src = ./.;
  strictDeps = true;
  version = "0.0.0";
}
