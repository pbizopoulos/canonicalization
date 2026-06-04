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
    coverage_dir="$PWD/coverage"
    hpcdir="$PWD/hpc"
    export HOME="$PWD"
    packageName="${packageName}"
    mkdir -p "$coverage_dir/html" "$hpcdir"
    cat > "$PWD/TestMain.hs" <<EOF
    module TestMain (main) where
    import qualified Main as PackageMain
    main :: IO ()
    main = PackageMain.runPackageTests
    EOF
    "${debugGhc}/bin/ghc" \
      -fhpc \
      -hpcdir "$hpcdir" \
      -main-is TestMain.main \
      -i"$src" \
      -outputdir "$PWD" \
      -odir "$PWD" \
      -hidir "$PWD" \
      -o "$PWD/$packageName" \
      "$PWD/TestMain.hs" \
      "$src/Main.hs"
    HPCTIXFILE="$coverage_dir/$packageName.tix" "$PWD/$packageName"
    "${debugGhc}/bin/hpc" markup "$coverage_dir/$packageName.tix" \
      --hpcdir="$hpcdir" \
      --destdir="$coverage_dir/html"
    "${debugGhc}/bin/hpc" report "$coverage_dir/$packageName.tix" \
      --hpcdir="$hpcdir" | tee "$coverage_dir/summary.txt"
    touch "$out"
  ''
