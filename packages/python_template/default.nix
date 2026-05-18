{
  inputs,
  pkgs ? import <nixpkgs> { },
}:
let
  installationScript = inputs.agenix-shell.lib.installationScript pkgs.stdenv.system {
    secrets.secrets.file = ../../secrets/secrets.age;
  };
  pyPkgs = pkgs.python312Packages;
  python = pkgs.python312;
in
pyPkgs.buildPythonPackage rec {
  installCheckPhase = ''
    runHook preInstallCheck
    HOME="$(mktemp -d)" DEBUG=1 PYTHONWARNINGS=error coverage run --source="$src" -m pyinstrument "$src/main.py"
    coverage report
    runHook postInstallCheck
  '';
  installPhase = ''
    install -Dm644 main.py $out/${python.sitePackages}/${pname}.py
    install -Dm755 ./main.py $out/bin/${pname}
  '';
  meta.mainProgram = pname;
  nativeInstallCheckInputs = [
    pyPkgs.coverage
    pyPkgs.pyinstrument
  ];
  pname = baseNameOf ./.;
  pyproject = false;
  shellHook = ''
    source ${pkgs.lib.getExe installationScript}
    export $secrets
  '';
  src = ./.;
  strictDeps = true;
  version = "0.0.0";
}
