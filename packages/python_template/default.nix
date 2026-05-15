{
  pkgs ? import <nixpkgs> { },
}:
pkgs.python3Packages.buildPythonPackage rec {
  installCheckPhase = ''
    runHook preInstallCheck
    test -x "$out/bin/${pname}"
    HOME="$(mktemp -d)" DEBUG=1 PYTHONWARNINGS=error coverage run --source="$src" -m pyinstrument "$src/main.py"
    coverage report
    runHook postInstallCheck
  '';
  installPhase = ''
    install -Dm755 ./main.py $out/bin/${pname}
  '';
  meta.mainProgram = pname;
  nativeInstallCheckInputs = [
    pkgs.python3Packages.coverage
    pkgs.python3Packages.pyinstrument
  ];
  pname = baseNameOf ./.;
  pyproject = false;
  src = ./.;
  strictDeps = true;
  version = "0.0.0";
}
