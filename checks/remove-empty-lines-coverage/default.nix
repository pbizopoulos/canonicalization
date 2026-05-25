{
  inputs,
  pkgs,
  ...
}:
let
  checkName = builtins.baseNameOf ./.;
  packageName = pkgs.lib.removeSuffix "-coverage" checkName;
  inherit (inputs.self.packages.${pkgs.stdenv.system}.${packageName}) cargoDeps;
  rustBaseInputs =
    inputs.self.packages.${pkgs.stdenv.system}.${packageName}.passthru.rustCheckNativeBuildInputs;
in
pkgs.runCommand "${checkName}"
  {
    nativeBuildInputs = rustBaseInputs ++ [
      pkgs.cargo-llvm-cov
      pkgs.llvmPackages.llvm
    ];
    src = ../../packages/${packageName};
  }
  ''
    export LLVM_COV='${pkgs.lib.getExe' pkgs.llvmPackages.llvm "llvm-cov"}'
    export LLVM_PROFDATA='${pkgs.lib.getExe' pkgs.llvmPackages.llvm "llvm-profdata"}'
    cp -R --no-preserve=mode "$src" "$PWD/workspace"
    install -Dm644 "${cargoDeps}/.cargo/config.toml" "$PWD/workspace/.cargo/config.toml"
    substituteInPlace "$PWD/workspace/.cargo/config.toml" \
      --replace-fail "@vendor@" "${cargoDeps}"
    cd "$PWD/workspace"
    cargo llvm-cov
    touch "$out"
  ''
