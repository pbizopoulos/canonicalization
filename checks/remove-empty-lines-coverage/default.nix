{
  inputs,
  pkgs,
  ...
}:
let
  inherit (packageDrv) cargoDeps;
  checkName = builtins.baseNameOf ./.;
  packageDrv = inputs.self.packages.${pkgs.stdenv.system}.${packageName};
  packageName = pkgs.lib.removeSuffix "-coverage" checkName;
  rustBaseInputs = packageDrv.passthru.rustCheckNativeBuildInputs;
in
pkgs.runCommand "${checkName}"
  {
    nativeBuildInputs = rustBaseInputs ++ [
      pkgs.cargo-llvm-cov
      pkgs.llvmPackages.llvm
    ];
    src = ../.. + "/packages/${packageName}";
  }
  ''
    export LLVM_COV='${pkgs.lib.getExe' pkgs.llvmPackages.llvm "llvm-cov"}'
    export LLVM_PROFDATA='${pkgs.lib.getExe' pkgs.llvmPackages.llvm "llvm-profdata"}'
    workspace="$PWD/workspace"
    cp -R --no-preserve=mode "$src" "$workspace"
    install -Dm644 "${cargoDeps}/.cargo/config.toml" "$workspace/.cargo/config.toml"
    substituteInPlace "$workspace/.cargo/config.toml" \
      --replace-fail "@vendor@" "${cargoDeps}"
    cd "$workspace"
    cargo llvm-cov
    touch "$out"
  ''
