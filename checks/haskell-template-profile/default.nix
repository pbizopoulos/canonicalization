{
  pkgs,
  ...
}:
let
  checkName = builtins.baseNameOf ./.;
  packageName = "haskell-template";
  profileGhc = pkgs.haskellPackages.ghcWithPackages (ps: [
    ps.HUnit
    ps.aeson
    ps.base
    ps.bytestring
  ]);
in
pkgs.runCommand "${checkName}"
  {
    nativeBuildInputs = [
      profileGhc
      pkgs.coreutils
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
