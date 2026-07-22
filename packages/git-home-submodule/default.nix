{
  pkgs ? import <nixpkgs> { },
}:
pkgs.rustPlatform.buildRustPackage rec {
  cargoHash = "sha256-ECtdW4/yF8gcbn+616yU5LWwItY7WJd0cM6BOwTSVRo=";
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
    description = "Manage repositories as canonical submodules of a private, allowlisted home-directory Git superproject.";
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
