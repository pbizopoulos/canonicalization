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
      profileGhc
    ];
    src = ../.. + "/packages/${packageName}";
  }
  ''
    export HOME="$PWD"
    workspace="$PWD/workspace"
    packageName="${packageName}"
    mkdir -p "$workspace"
    cd "$workspace"
    cat > "$PWD/TestMain.hs" <<EOF
    module TestMain (main) where
    import qualified Main as PackageMain
    main :: IO ()
    main = PackageMain.runPackageTests
    EOF
    "${profileGhc}/bin/ghc" \
      -prof \
      -fprof-auto \
      -rtsopts \
      -O2 \
      -main-is TestMain.main \
      -i"$src" \
      -outputdir "$PWD" \
      -odir "$PWD" \
      -hidir "$PWD" \
      -o "$PWD/$packageName" \
      "$PWD/TestMain.hs" \
      "$src/Main.hs"
    "$PWD/$packageName" +RTS -p -RTS
    cat "$PWD/$packageName.prof"
    touch "$out"
  ''
