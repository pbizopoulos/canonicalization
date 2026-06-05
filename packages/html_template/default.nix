{
  pkgs ? import <nixpkgs> { },
}:
pkgs.writeShellScriptBin (baseNameOf ./.) ''
  exec ${pkgs.http-server}/bin/http-server ${./.} "$@"
''
