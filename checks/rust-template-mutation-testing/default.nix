{
  inputs,
  pkgs,
  ...
}:
let
  checkName = builtins.baseNameOf ./.;
  packageName = "rust-template";
  inherit (inputs.self.packages.${pkgs.stdenv.system}.${packageName}) cargoDeps;
in
pkgs.runCommand "${checkName}"
  {
    nativeBuildInputs = [
      pkgs.cargo
      pkgs.cargo-mutants
      pkgs.rustc
      pkgs.stdenv.cc
    ];
    src = ../../packages/${packageName};
  }
  ''
    cp -R --no-preserve=mode "$src" "$PWD/workspace"
    install -Dm644 "${cargoDeps}/.cargo/config.toml" "$PWD/workspace/.cargo/config.toml"
    substituteInPlace "$PWD/workspace/.cargo/config.toml" \
      --replace-fail "@vendor@" "${cargoDeps}"
    cd "$PWD/workspace"
    cargo mutants || mutation_status=$?
    if [ "''${mutation_status:-0}" != 0 ] && [ "''${mutation_status:-0}" != 2 ]; then
      exit "$mutation_status"
    fi
    touch "$out"
  ''
