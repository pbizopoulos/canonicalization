{
  inputs,
  pkgs ? import <nixpkgs> { },
}:
let
  installationScript = inputs.agenix-shell.lib.installationScript pkgs.stdenv.system {
    secrets.secrets.file = ../../secrets/secrets.age;
  };
  python = pkgs.python312;
in
python.pkgs.buildPythonPackage rec {
  installPhase = ''
    install -Dm644 main.py $out/${python.sitePackages}/${pname}.py
    install -Dm755 main.py $out/bin/${pname}
    if [ -d prm ]; then
      cp -r prm/ $out/${python.sitePackages}/
      cp -r prm/ $out/bin/
    fi
  '';
  meta.mainProgram = pname;
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
