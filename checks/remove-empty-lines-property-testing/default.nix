{
  inputs,
  pkgs,
  ...
}:
let
  checkName = builtins.baseNameOf ./.;
  packageName = pkgs.lib.removeSuffix "-property-testing" checkName;
  inherit (inputs.self.packages.${pkgs.stdenv.system}.${packageName}) cargoDeps;
  rustBaseInputs =
    inputs.self.packages.${pkgs.stdenv.system}.${packageName}.passthru.rustCheckNativeBuildInputs;
in
pkgs.runCommand "${checkName}"
  {
    nativeBuildInputs = rustBaseInputs;
    src = ../../packages/${packageName};
  }
  ''
    cp -R --no-preserve=mode "$src" "$PWD/workspace"
    install -Dm644 "${cargoDeps}/.cargo/config.toml" "$PWD/workspace/.cargo/config.toml"
    substituteInPlace "$PWD/workspace/.cargo/config.toml" \
      --replace-fail "@vendor@" "${cargoDeps}"
    cd "$PWD/workspace"
    cargo test --locked
    touch "$out"
  ''
