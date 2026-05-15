{
  pkgs ? import <nixpkgs> { },
}:
let
  name = baseNameOf ./.;
in
pkgs.writeShellApplication {
  inherit name;
  checkPhase = ''
    "$target" | grep -F "Hello World"
  '';
  text = builtins.readFile ./main.sh;
}
