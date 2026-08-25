{
  pkgs,
  ...
}:
let
  checkName = baseNameOf ./.;
  packageDrv = import (../.. + "/packages/${packageName}/default.nix") {
    inherit pkgs;
  };
  packageName = pkgs.lib.removeSuffix "-coverage" checkName;
  testGhc = pkgs.haskellPackages.ghcWithPackages (_: packageDrv.passthru.haskellExecutableDepends);
in
pkgs.runCommand checkName
  {
    nativeBuildInputs = [
      packageDrv
      pkgs.git
      testGhc
    ];
    src = ../.. + "/packages/${packageName}";
  }
  ''
    export HOME="$PWD"
    workspace="$PWD/workspace"
    packageName="${packageName}"
    mkdir -p "$out/html" "$workspace/coverage" "$workspace/hpc"
    cd "$workspace"
    cat > "$workspace/TestMain.hs" <<EOF
    module TestMain (main) where
    import qualified Main as PackageMain
    main :: IO ()
    main = PackageMain.runPackageTests
    EOF
    "${testGhc}/bin/ghc" \
      -fhpc \
      -hpcdir "$workspace/hpc" \
      -O2 \
      -main-is TestMain.main \
      -i"$src" \
      -outputdir "$workspace" \
      -odir "$workspace" \
      -hidir "$workspace" \
      -o "$workspace/$packageName" \
      "$workspace/TestMain.hs" \
      "$src/Main.hs"
    PACKAGE_E2E_EXECUTABLE="${packageDrv}/bin/${packageName}" HPCTIXFILE="$workspace/coverage/$packageName.tix" \
      "$workspace/$packageName"
    hpc markup "$workspace/coverage/${packageName}.tix" --hpcdir="$workspace/hpc" --destdir="$out/html"
  ''
