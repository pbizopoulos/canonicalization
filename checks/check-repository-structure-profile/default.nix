{
  pkgs,
  ...
}:
let
  checkName = builtins.baseNameOf ./.;
  packageName = "check-repository-structure";
  packageDrv = import ../../packages/${packageName}/default.nix { inherit pkgs; };
  profileGhc = pkgs.haskellPackages.ghcWithPackages (_: packageDrv.passthru.haskellExecutableDepends);
in
pkgs.runCommand "${checkName}"
  {
    nativeBuildInputs = [
      pkgs.coreutils
      profileGhc
    ];
    src = ../../packages/${packageName};
  }
  ''
    export HOME="$PWD"
    workspace="$PWD/workspace"
    packageName="${packageName}"
    rm -rf "$workspace"
    mkdir -p "$workspace"
    cd "$workspace"
    "${profileGhc}/bin/ghc" \
      -prof \
      -fprof-auto \
      -rtsopts \
      -O2 \
      -outputdir "$PWD" \
      -odir "$PWD" \
      -hidir "$PWD" \
      -o "$PWD/$packageName" \
      "$src/Main.hs"
    DEBUG=1 "$PWD/$packageName" +RTS -p -RTS
    cat "$PWD/$packageName.prof"
    touch "$out"
  ''
