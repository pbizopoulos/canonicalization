{
  pkgs,
  ...
}:
let
  checkName = builtins.baseNameOf ./.;
  packageDrv = import (../.. + "/packages/${packageName}/default.nix") {
    inherit pkgs;
  };
  packageName = pkgs.lib.removeSuffix "-profile" checkName;
  profileGhc = pkgs.haskellPackages.ghcWithPackages (_: packageDrv.passthru.haskellExecutableDepends);
in
pkgs.runCommand checkName
  {
    nativeBuildInputs = [
      packageDrv
      pkgs.git
      pkgs.time
      profileGhc
    ];
    src = ../.. + "/packages/${packageName}";
  }
  ''
    export HOME="$PWD"
    workspace="$PWD/workspace"
    packageName="${packageName}"
    mkdir -p "$out" "$workspace"
    cd "$workspace"
    cat > "$workspace/TestMain.hs" <<EOF
    module TestMain (main) where
    import qualified Main as PackageMain
    import System.Environment (getEnv)
    main :: IO ()
    main = getEnv "PROFILE_TIMINGS_PATH" >>= PackageMain.runPackageTestsWithTimings
    EOF
    "${profileGhc}/bin/ghc" \
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
    PROFILE_TIMINGS_PATH="$out/test-timings.tsv" ${pkgs.time}/bin/time -f %e -o "$out/total-seconds" \
      "$workspace/$packageName" +RTS -p -RTS
    mv "$workspace/$packageName.prof" "$out/report.prof"
    printf 'profile-v1\ttotal-seconds\t%s\n' "$(cat "$out/total-seconds")" > "$out/profile-summary.tsv"
    cat "$out/test-timings.tsv" >> "$out/profile-summary.tsv"
  ''
