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
  packageName = pkgs.lib.removeSuffix "-coverage" checkName;
in
pkgs.runCommand checkName
  {
    nativeBuildInputs = [
      debugGhc
    ];
    src = ../.. + "/packages/${packageName}";
  }
  ''
    export HOME="$PWD"
    mkdir -p coverage/html hpc
    cat > TestMain.hs <<EOF
    module TestMain (main) where
    import qualified Main as PackageMain
    main :: IO ()
    main = PackageMain.runPackageTests
    EOF
    ghc -fhpc -hpcdir hpc -main-is TestMain.main \
      -i"$src" -outputdir . -odir . -hidir . \
      -o "${packageName}" TestMain.hs "$src/Main.hs"
    HPCTIXFILE="coverage/${packageName}.tix" "./${packageName}"
    hpc markup "coverage/${packageName}.tix" --hpcdir=hpc --destdir=coverage/html
    hpc report "coverage/${packageName}.tix" --hpcdir=hpc | tee coverage/summary.txt
    touch "$out"
  ''
