{
  pkgs,
  ...
}:
let
  checkName = builtins.baseNameOf ./.;
  packageName = pkgs.lib.removeSuffix "-property-testing" checkName;
  packageDrv = import ../../packages/${packageName}/default.nix { inherit pkgs; };
  debugGhc = pkgs.haskellPackages.ghcWithPackages (_: packageDrv.passthru.haskellExecutableDepends);
in
pkgs.runCommand "${checkName}"
  {
    nativeBuildInputs = [
      debugGhc
    ];
    src = ../../packages/${packageName};
  }
  ''
    export HOME="$PWD"
    packageName="${packageName}"
    "${debugGhc}/bin/ghc" \
      -O2 \
      -outputdir "$PWD" \
      -odir "$PWD" \
      -hidir "$PWD" \
      -o "$PWD/$packageName" \
      "$src/Main.hs"
    DEBUG=1 PROPERTY_TESTS=1 "$PWD/$packageName"
    touch "$out"
  ''
