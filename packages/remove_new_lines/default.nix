{ inputs, pkgs, ... }:
let
  moduleName = builtins.replaceStrings [ "-" ] [ "_" ] pname;
  pname = baseNameOf ./.;
  python = pkgs.python3;
  pythonDeps = [ ];
in
python.pkgs.buildPythonPackage {
  inherit pname;
  installPhase = ''
    install -Dm644 main.py "$out/${python.sitePackages}/${moduleName}.py"
    install -Dm755 main.py "$out/bin/$pname"
    if [ -d prm ]; then
      cp -R prm/ "$out/${python.sitePackages}/"
      cp -R prm/ "$out/bin/"
    fi
  '';
  meta.mainProgram = pname;
  passthru.python = python;
  propagatedBuildInputs = pythonDeps;
  pyproject = false;
  src = inputs.self + "/packages/${pname}";
  strictDeps = true;
  version = "0.0.0";
}
