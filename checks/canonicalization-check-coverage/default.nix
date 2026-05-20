{
  pkgs,
  ...
}:
let
  checkName = builtins.baseNameOf ./.;
  packageName = "canonicalization-check";
  packageDrv = import ../../packages/${packageName}/default.nix { inherit pkgs; };
  debugGhc = pkgs.haskellPackages.ghcWithPackages (_: packageDrv.passthru.haskellExecutableDepends);
in
pkgs.runCommand "${checkName}"
  {
    nativeBuildInputs = [
      debugGhc
      pkgs.coreutils
    ];
    src = ../../packages/${packageName};
  }
  ''
    coverage_dir="$PWD/coverage"
    hpcdir="$PWD/hpc"
    export HOME="$PWD"
    packageName="${packageName}"
    rm -rf "$coverage_dir" "$hpcdir"
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
