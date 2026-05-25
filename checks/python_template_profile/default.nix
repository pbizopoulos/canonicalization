{
  pkgs,
  ...
}:
let
  checkName = builtins.baseNameOf ./.;
  packageName = pkgs.lib.removeSuffix "_profile" checkName;
  pyPkgs = pkgs.python312Packages;
in
pkgs.runCommand "${checkName}"
  {
    nativeBuildInputs = [
      pkgs.coreutils
      pkgs.python312
      pyPkgs.hypothesis
      pyPkgs.pyinstrument
    ];
    src = ../../packages/${packageName};
  }
  ''
    export HOME="$(mktemp -d)"
    DEBUG=1 PYTHONWARNINGS=error pyinstrument "$src/main.py"
    touch "$out"
  ''
