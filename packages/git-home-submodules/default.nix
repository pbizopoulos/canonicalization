{
  pkgs ? import <nixpkgs> { },
}:
pkgs.rustPlatform.buildRustPackage rec {
  cargoHash = "sha256-XkJ4c8CcbwmvWEltX+n4J7r7E2ruHKVPd6jebU81oh4=";
  doInstallCheck = pkgs.stdenv.isLinux;
  env = {
    RUSTDOCFLAGS = "-D warnings";
    RUSTFLAGS = "-D warnings";
  };
  installCheckPhase = ''
    runHook preInstallCheck
    "$out/bin/${pname}" --help >/dev/null
    runHook postInstallCheck
  '';
  meta = {
    description = "Manage Git submodules in a canonical hierarchy below the home directory.";
    mainProgram = pname;
  };
  nativeBuildInputs = [
    pkgs.makeWrapper
  ];
  nativeCheckInputs = [
    pkgs.git
  ];
  passthru.rustCheckNativeBuildInputs = [
    pkgs.cargo
    pkgs.rustc
    pkgs.stdenv.cc
  ];
  pname = baseNameOf ./.;
  postInstall = ''
    wrapProgram "$out/bin/${pname}" --prefix PATH : ${
      pkgs.lib.makeBinPath [
        pkgs.git
      ]
    }
  '';
  src = ./.;
  strictDeps = true;
  version = "0.0.0";
}
