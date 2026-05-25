{
  pkgs ? import <nixpkgs> { },
}:
pkgs.haskellPackages.mkDerivation rec {
  executableHaskellDepends = [
    pkgs.haskellPackages.HUnit
    pkgs.haskellPackages.QuickCheck
    pkgs.haskellPackages.base
    pkgs.haskellPackages.containers
    pkgs.haskellPackages.data-fix
    pkgs.haskellPackages.directory
    pkgs.haskellPackages.filepath
    pkgs.haskellPackages.hnix
    pkgs.haskellPackages.prettyprinter
    pkgs.haskellPackages.process
    pkgs.haskellPackages.regex-tdfa
    pkgs.haskellPackages.text
  ];
  executableToolDepends = [
    pkgs.makeWrapper
    pkgs.python3
  ];
  mainProgram = pname;
  passthru.haskellExecutableDepends = executableHaskellDepends;
  pname = baseNameOf ./.;
  postInstall = ''
    wrapProgram $out/bin/${pname} --prefix PATH : ${
      pkgs.lib.makeBinPath [
        pkgs.python3
      ]
    } --run "rm -f tmp/${pname}.tix" --set-default HPCTIXFILE tmp/${pname}.tix
    DEBUG=1 "$out/bin/${pname}"
  '';
  src = ./.;
  version = "0.0.0";
}
