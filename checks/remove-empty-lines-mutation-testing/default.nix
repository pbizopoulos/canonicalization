{
  inputs,
  pkgs,
  ...
}:
let
  checkName = builtins.baseNameOf ./.;
  packageName = "remove-empty-lines";
  inherit (inputs.self.packages.${pkgs.stdenv.system}.${packageName}) cargoDeps;
  rustBaseInputs =
    inputs.self.packages.${pkgs.stdenv.system}.${packageName}.passthru.rustCheckNativeBuildInputs;
in
pkgs.runCommand "${checkName}"
  {
    nativeBuildInputs = rustBaseInputs ++ [
      pkgs.cargo-mutants
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
