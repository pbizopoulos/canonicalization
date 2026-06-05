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
    workspace="$PWD/workspace"
    packageName="${packageName}"
    mkdir -p "$workspace"
    cd "$workspace"
    cat > TestMain.hs <<EOF
    module TestMain (main) where
    import qualified Main as PackageMain
    main :: IO ()
    main = PackageMain.runPackageTests
    EOF
    "${debugGhc}/bin/ghc" \
      -O2 \
      -main-is TestMain.main \
      -i"$src" \
      -outputdir "$workspace" \
      -odir "$workspace" \
      -hidir "$workspace" \
      -o "$workspace/$packageName" \
      "$workspace/TestMain.hs" \
      "$src/Main.hs"
    "$workspace/$packageName"
    touch "$out"
  ''
