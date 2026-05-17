{
  pkgs,
  ...
}:
let
  checkName = builtins.baseNameOf ./.;
  packageName = "haskell-template";
  debugGhc = pkgs.haskellPackages.ghcWithPackages (ps: [
    ps.HUnit
    ps.aeson
    ps.base
    ps.bytestring
  ]);
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
    rm -rf "$PWD/workspace" "$coverage_dir" "$hpcdir"
    mkdir -p "$PWD/workspace" "$coverage_dir/html" "$hpcdir"
    cp -R --no-preserve=mode "$src"/. "$PWD/workspace"
    cd "$PWD/workspace"
    "${debugGhc}/bin/ghc" \
      -fhpc \
      -hpcdir "$hpcdir" \
      -outputdir "$PWD" \
      -odir "$PWD" \
      -hidir "$PWD" \
      -o "$PWD/$packageName" \
      Main.hs
    HPCTIXFILE="$coverage_dir/$packageName.tix" DEBUG=1 "$PWD/$packageName"
    "${debugGhc}/bin/hpc" markup "$coverage_dir/$packageName.tix" \
      --hpcdir="$hpcdir" \
      --destdir="$coverage_dir/html"
    "${debugGhc}/bin/hpc" report "$coverage_dir/$packageName.tix" \
      --hpcdir="$hpcdir" | tee "$coverage_dir/summary.txt"
    touch "$out"
  ''
