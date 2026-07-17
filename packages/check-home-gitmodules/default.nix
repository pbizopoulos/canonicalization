{
  pkgs ? import <nixpkgs> { },
}:
pkgs.rustPlatform.buildRustPackage rec {
  cargoHash = "sha256-JYC3GclPnG/jBS3fr6mpc0H44rAuyX4C2dd2ddY66u0=";
  doInstallCheck = pkgs.stdenv.isLinux;
  env = {
    RUSTDOCFLAGS = "-D warnings";
    RUSTFLAGS = "-D warnings";
  };
  installCheckPhase = ''
    runHook preInstallCheck
    test -x "$out/bin/${pname}"
    workspace="$PWD/installcheck"
    mkdir -p "$workspace/home"
    export HOME="$workspace/home"
    if "$out/bin/${pname}"; then
      echo "expected a missing .gitmodules file to fail" >&2
      exit 1
    fi
    : > "$HOME/.gitmodules"
    "$out/bin/${pname}"
    printf '%s\n' '[submodule "example"]' '  path = github.com/owner/repository' > "$HOME/.gitmodules"
    "$out/bin/${pname}"
    if "$out/bin/${pname}" unexpected-argument; then
      echo "expected an unexpected argument to fail" >&2
      exit 1
    fi
    printf '%s\n' '[submodule "example"]' '  path = owner/repository' > "$HOME/.gitmodules"
    if "$out/bin/${pname}"; then
      echo "expected malformed path check to fail" >&2
      exit 1
    fi
    runHook postInstallCheck
  '';
  meta.mainProgram = pname;
  pname = baseNameOf ./.;
  src = ./.;
  strictDeps = true;
  version = "0.0.0";
}
