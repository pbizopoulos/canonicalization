{
  pkgs,
  ...
}:
let
  checkName = builtins.baseNameOf ./.;
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
    mkdir -p "$workspace/coverage/html" "$workspace/hpc"
    cd "$workspace"
    cat > TestMain.hs <<EOF
    module TestMain (main) where
    import qualified Main as PackageMain
    main :: IO ()
    main = PackageMain.runPackageTests
    EOF
    ghc -fhpc -hpcdir "$workspace/hpc" -main-is TestMain.main \
      -i"$src" -outputdir "$workspace" -odir "$workspace" -hidir "$workspace" \
      -o "${packageName}" TestMain.hs "$src/Main.hs"
    HPCTIXFILE="$workspace/coverage/${packageName}.tix" "./${packageName}"
    hpc markup "$workspace/coverage/${packageName}.tix" --hpcdir="$workspace/hpc" --destdir="$workspace/coverage/html"
    hpc report "$workspace/coverage/${packageName}.tix" --hpcdir="$workspace/hpc" | tee "$workspace/coverage/summary.txt"
    touch "$out"
  ''
