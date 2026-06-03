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
    "${debugGhc}/bin/ghc" \
      -fhpc \
      -hpcdir "$hpcdir" \
      -outputdir "$PWD" \
      -odir "$PWD" \
      -hidir "$PWD" \
      -o "$PWD/$packageName" \
      "$src/Main.hs"
    HPCTIXFILE="$coverage_dir/$packageName.tix" DEBUG=1 "$PWD/$packageName"
    "${debugGhc}/bin/hpc" markup "$coverage_dir/$packageName.tix" \
      --hpcdir="$hpcdir" \
      --destdir="$coverage_dir/html"
    "${debugGhc}/bin/hpc" report "$coverage_dir/$packageName.tix" \
      --hpcdir="$hpcdir" | tee "$coverage_dir/summary.txt"
    touch "$out"
  ''
