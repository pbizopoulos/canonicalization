{
  pkgs ? import <nixpkgs> { },
}:
pkgs.haskellPackages.mkDerivation rec {
  executableHaskellDepends = [
    pkgs.haskellPackages.HUnit
    pkgs.haskellPackages.aeson
    pkgs.haskellPackages.base
    pkgs.haskellPackages.bytestring
    pkgs.haskellPackages.hnix
    pkgs.haskellPackages.temporary
  ];
  executableToolDepends = [
    pkgs.makeWrapper
  ];
  mainProgram = pname;
  passthru.haskellExecutableDepends = executableHaskellDepends;
  pname = baseNameOf ./.;
  postInstall = ''
    wrapProgram $out/bin/${pname} --run "rm -f tmp/${pname}.tix" --set-default HPCTIXFILE tmp/${pname}.tix
    DEBUG=1 "$out/bin/${pname}"
  '';
  src = ./.;
  version = "0.0.0";
}
