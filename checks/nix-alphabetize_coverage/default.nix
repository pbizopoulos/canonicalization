{
  inputs,
  pkgs,
  ...
}: let
  checkName = baseNameOf ./.;
  packageDrv = inputs.self.packages.${pkgs.stdenv.system}.${packageName};
  packageName = pkgs.lib.removeSuffix "_coverage" checkName;
  pythonEnv = packageDrv.python.withPackages (_: packageDrv.propagatedBuildInputs ++ [packageDrv.python.pkgs.pytest packageDrv.python.pkgs.pytest-cov]);
in
  pkgs.runCommand checkName {
    nativeBuildInputs = [packageDrv pkgs.alejandra pythonEnv];
    src = ../../packages/nix-alphabetize;
  } ''
    export HOME="$(mktemp -d)"
    mkdir -p "$out/html"
    PACKAGE_E2E_EXECUTABLE="${packageDrv}/bin/${packageName}" python -m pytest -p no:cacheprovider --cov="$src" --cov-report "html:$out/html" "$src/main.py"
  ''
