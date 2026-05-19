{
  pkgs ? import <nixpkgs> { },
}:
pkgs.haskellPackages.mkDerivation rec {
  executableHaskellDepends = [
    pkgs.haskellPackages.HUnit
    pkgs.haskellPackages.base
    pkgs.haskellPackages.containers
    pkgs.haskellPackages.data-fix
    pkgs.haskellPackages.directory
    pkgs.haskellPackages.filepath
    pkgs.haskellPackages.hnix
    pkgs.haskellPackages.prettyprinter
    pkgs.haskellPackages.regex-tdfa
    pkgs.haskellPackages.text
  ];
  executableToolDepends = [
    pkgs.makeWrapper
  ];
  mainProgram = pname;
  pname = baseNameOf ./.;
  postInstall = ''
    wrapProgram $out/bin/${pname} --run "rm -f tmp/${pname}.tix" --set-default HPCTIXFILE tmp/${pname}.tix
    DEBUG=1 "$out/bin/${pname}"
  '';
  src = ./.;
  version = "0.0.0";
}
