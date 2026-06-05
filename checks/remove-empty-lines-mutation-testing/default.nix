{
  inputs,
  pkgs,
  ...
}:
let
  inherit (packageDrv) cargoDeps;
  checkName = builtins.baseNameOf ./.;
  packageDrv = inputs.self.packages.${pkgs.stdenv.system}.${packageName};
  packageName = pkgs.lib.removeSuffix "-mutation-testing" checkName;
  rustBaseInputs = packageDrv.passthru.rustCheckNativeBuildInputs;
in
pkgs.runCommand "${checkName}"
  {
    nativeBuildInputs = rustBaseInputs ++ [
      pkgs.cargo-mutants
    ];
    src = ../.. + "/packages/${packageName}";
  }
  ''
    workspace="$PWD/workspace"
    cp -R --no-preserve=mode "$src" "$workspace"
    install -Dm644 "${cargoDeps}/.cargo/config.toml" "$workspace/.cargo/config.toml"
    substituteInPlace "$workspace/.cargo/config.toml" \
      --replace-fail "@vendor@" "${cargoDeps}"
    cd "$workspace"
    cargo mutants || mutation_status=$?
    if [ "''${mutation_status:-0}" != 0 ] && [ "''${mutation_status:-0}" != 2 ]; then
      exit "$mutation_status"
    fi
    touch "$out"
  ''
