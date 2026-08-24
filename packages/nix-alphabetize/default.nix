{
  pkgs ? import <nixpkgs> { },
}:
let
  executableHaskellDepends = [
    pkgs.haskellPackages.HUnit
    pkgs.haskellPackages.QuickCheck
    pkgs.haskellPackages.base
    pkgs.haskellPackages.hnix
    pkgs.haskellPackages.prettyprinter
    pkgs.haskellPackages.process
    pkgs.haskellPackages.temporary
  ];
  ghc = pkgs.haskellPackages.ghcWithPackages (_: executableHaskellDepends);
in
pkgs.stdenv.mkDerivation rec {
  buildPhase = ''
    runHook preBuild
    ${ghc}/bin/ghc -O2 -Weverything -Werror -threaded -i. -o "$pname" Main.hs
    runHook postBuild
  '';
  installPhase = ''
    runHook preInstall
    install -Dm755 "$pname" "$out/bin/$pname"
    runHook postInstall
  '';
  meta.mainProgram = pname;
  nativeBuildInputs = [
    ghc
    pkgs.makeWrapper
  ];
  passthru.haskellExecutableDepends = executableHaskellDepends;
  pname = baseNameOf ./.;
  postInstall = ''
    wrapProgram "$out/bin/${pname}" --run "rm -f tmp/${pname}.tix" --set-default HPCTIXFILE tmp/${pname}.tix
    ${ghc}/bin/ghc -i. -e 'Main.runPackageTests' Main.hs
  '';
  src = ./.;
  version = "0.0.0";
}
