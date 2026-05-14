{
  pkgs ? import <nixpkgs> { },
}:
let
  pname = baseNameOf ./.;
in
pkgs.writeShellScriptBin pname ''
  if [ "$DEBUG" != "1" ]; then
    exec ${pkgs.http-server}/bin/http-server ${./.} "$@"
  fi
''
