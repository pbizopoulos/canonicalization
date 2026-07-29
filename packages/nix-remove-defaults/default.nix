{
  pkgs ? import <nixpkgs> { },
}:
let
  executableHaskellDepends = [
    pkgs.haskellPackages.HUnit
    pkgs.haskellPackages.aeson
    pkgs.haskellPackages.base
    pkgs.haskellPackages.bytestring
    pkgs.haskellPackages.containers
    pkgs.haskellPackages.data-fix
    pkgs.haskellPackages.directory
    pkgs.haskellPackages.filepath
    pkgs.haskellPackages.hnix
    pkgs.haskellPackages.prettyprinter
    pkgs.haskellPackages.process
    pkgs.haskellPackages.scientific
    pkgs.haskellPackages.temporary
    pkgs.haskellPackages.text
  ];
  ghcForTests = pkgs.haskellPackages.ghcWithPackages (_: executableHaskellDepends);
in
pkgs.haskellPackages.mkDerivation rec {
  inherit executableHaskellDepends;
  executableToolDepends = [
    pkgs.makeWrapper
  ];
  mainProgram = pname;
  passthru.haskellExecutableDepends = executableHaskellDepends;
  pname = baseNameOf ./.;
  postInstall = ''
    wrapProgram "$out/bin/${pname}" --prefix PATH : ${
      pkgs.lib.makeBinPath [
        pkgs.nix
      ]
    } --run "rm -f tmp/${pname}.tix" --set-default HPCTIXFILE tmp/${pname}.tix
    ${ghcForTests}/bin/ghc -i. -e 'Main.runPackageTests' Main.hs
  '';
  src = ./.;
  version = "0.0.0";
}
