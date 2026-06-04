{
  pkgs ? import <nixpkgs> { },
}:
let
  pname = baseNameOf ./.;
in
pkgs.writeShellScriptBin pname ''
  exec ${pkgs.http-server}/bin/http-server ${./.} "$@"
''
