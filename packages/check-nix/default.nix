{
  pkgs ? import <nixpkgs> { },
}:
pkgs.haskellPackages.callPackage (
  {
    HUnit,
    base,
    data-fix,
    hnix,
    mkDerivation,
    prettyprinter,
    temporary,
    text,
  }:
  mkDerivation rec {
    description = "Sorts attributes alphabetically, using dotted notation for attributes with sets or lists, and nested notation otherwise";
    executableHaskellDepends = [
      HUnit
      base
      data-fix
      hnix
      prettyprinter
      temporary
      text
    ];
    isExecutable = true;
    license = pkgs.lib.licenses.mit;
    mainProgram = "${pname}";
    pname = builtins.baseNameOf src;
    src = ./.;
    version = "0.0.0";
  }
) { }
