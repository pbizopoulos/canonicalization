{
  inputs,
  pkgs ? import <nixpkgs> { },
}:
let
  installationScript = inputs.agenix-shell.lib.installationScript pkgs.stdenv.system {
    secrets.secrets.file = ../../secrets/secrets.age;
  };
  python = pkgs.python312;
  pythonTestEnv = python.withPackages (_: [
    python.pkgs.hypothesis
  ]);
in
python.pkgs.buildPythonPackage rec {
  installCheckPhase = ''
    runHook preInstallCheck
    HOME="$(mktemp -d)"
    cd "$src"
    ${pythonTestEnv}/bin/python -m unittest main.py
    runHook postInstallCheck
  '';
  installPhase = ''
    install -Dm644 main.py $out/${python.sitePackages}/${pname}.py
    install -Dm755 main.py $out/bin/${pname}
    if [ -d prm ]; then
      cp -r prm/ $out/${python.sitePackages}/
      cp -r prm/ $out/bin/
    fi
  '';
  meta.mainProgram = pname;
  nativeBuildInputs = [
    python.pkgs.coverage
  ];
  passthru = {
    inherit python;
  };
  pname = baseNameOf ./.;
  propagatedBuildInputs = [
    python.pkgs.hypothesis
  ];
  pyproject = false;
  shellHook = ''
    source ${pkgs.lib.getExe installationScript}
    export $secrets
  '';
  src = ./.;
  strictDeps = true;
  version = "0.0.0";
}
