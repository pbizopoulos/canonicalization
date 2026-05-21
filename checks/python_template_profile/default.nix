{
  pkgs,
  ...
}:
let
  checkName = builtins.baseNameOf ./.;
  packageName = "python_template";
  pyPkgs = pkgs.python312Packages;
in
pkgs.runCommand "${checkName}"
  {
    nativeBuildInputs = [
      pkgs.coreutils
      pkgs.python312
      pyPkgs.pyinstrument
    ];
    src = ../../packages/${packageName};
  }
  ''
    export HOME="$(mktemp -d)"
    DEBUG=1 PYTHONWARNINGS=error pyinstrument "$src/main.py"
    touch "$out"
  ''
