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
      pkgs.time
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
    main = PackageMain.runPackageTestsWithTimings "$workspace/test-timings.tsv"
    EOF
    "${testGhc}/bin/ghc" \
      -fhpc \
      -hpcdir "$workspace/hpc" \
      -prof \
      -fprof-auto \
      -rtsopts \
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
      ${pkgs.time}/bin/time -f %e -o "$workspace/total-seconds" \
      "$workspace/$packageName" +RTS -p -RTS
    mv "$workspace/$packageName.prof" "$out/profile-report.prof"
    printf 'profile-v1\ttotal-seconds\t%s\n' "$(cat "$workspace/total-seconds")" > "$out/profile-summary.tsv"
    cat "$workspace/test-timings.tsv" >> "$out/profile-summary.tsv"
    hpc markup "$workspace/coverage/${packageName}.tix" --hpcdir="$workspace/hpc" --destdir="$out/html"
    hpc report "$workspace/coverage/${packageName}.tix" --hpcdir="$workspace/hpc" | tee "$out/report.txt"
    coverageCounts="$(sed -n 's/.*expressions used (\([0-9][0-9]*\)\/\([0-9][0-9]*\)).*/\1 \2/p' "$out/report.txt")"
    read -r covered total <<< "$coverageCounts"
    test -n "$covered" && test -n "$total"
    printf 'coverage-v1\texpressions\t%s\t%s\n' "$covered" "$total" > "$out/coverage-summary.tsv"
  ''
