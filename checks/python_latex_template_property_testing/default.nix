{
  inputs,
  pkgs,
  ...
}:
let
  checkName = builtins.baseNameOf ./.;
  packageName = "python_latex_template";
  packageDrv = inputs.self.packages.${pkgs.stdenv.system}.${packageName};
  packagePythonDeps = packageDrv.propagatedBuildInputs or [ ];
  pythonEnv = pkgs.python3.withPackages (_: packagePythonDeps ++ [ pkgs.python3Packages.hypothesis ]);
in
pkgs.runCommand "${checkName}"
  {
    nativeBuildInputs = [
      pkgs.coreutils
      pythonEnv
    ];
    src = ../../packages/${packageName};
  }
  ''
    export HOME="$(mktemp -d)"
    export PYTHONWARNINGS=error
    PYTHONPATH="$src" python -m unittest -v main.PropertyTests
    touch "$out"
  ''
