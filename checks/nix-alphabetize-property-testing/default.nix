{
  pkgs,
  ...
}:
let
  checkName = builtins.baseNameOf ./.;
  debugGhc = pkgs.haskellPackages.ghcWithPackages (_: packageDrv.passthru.haskellExecutableDepends);
  packageDrv = import (../.. + "/packages/${packageName}/default.nix") {
    inherit pkgs;
  };
  packageName = pkgs.lib.removeSuffix "-property-testing" checkName;
in
pkgs.runCommand "${checkName}"
  {
    nativeBuildInputs = [
      debugGhc
    ];
    src = ../.. + "/packages/${packageName}";
  }
  ''
    export HOME="$PWD"
    packageName="${packageName}"
    cat > "$PWD/TestMain.hs" <<EOF
    module TestMain (main) where
    import qualified Main as PackageMain
    main :: IO ()
    main = PackageMain.runPackageTests
    EOF
    "${debugGhc}/bin/ghc" \
      -O2 \
      -main-is TestMain.main \
      -i"$src" \
      -outputdir "$PWD" \
      -odir "$PWD" \
      -hidir "$PWD" \
      -o "$PWD/$packageName" \
      "$PWD/TestMain.hs" \
      "$src/Main.hs"
    "$PWD/$packageName"
    touch "$out"
  ''
