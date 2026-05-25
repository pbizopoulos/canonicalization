{
  pkgs,
  ...
}:
let
  checkName = builtins.baseNameOf ./.;
  packageName = "python_latex_template";
  pythonDeps = [
    pkgs.python3Packages.matplotlib
    pkgs.python3Packages.pandas
  ];
  pythonEnv = pkgs.python3.withPackages (
    _:
    pythonDeps
    ++ [
      pkgs.python3Packages.hypothesis
      pkgs.python3Packages.pyinstrument
    ]
  );
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
    DEBUG=1 PYTHONWARNINGS=error pyinstrument "$src/main.py"
    touch "$out"
  ''
