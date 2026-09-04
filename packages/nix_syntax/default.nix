{ pkgs, ... }:
let
  nativeDeps = [ ];
  pname = baseNameOf ./.;
  python = pkgs.python3;
  pythonDeps = [ python.pkgs.tree-sitter-language-pack ];
  shellHook = "";
in
python.pkgs.buildPythonPackage {
  inherit pname;
  inherit shellHook;
  installPhase = ''
    install -Dm644 main.py "$out/${python.sitePackages}/$pname.py"
    install -Dm755 main.py "$out/bin/$pname"
    if [ -d prm ]; then
      cp -R prm/ "$out/${python.sitePackages}/"
      cp -R prm/ "$out/bin/"
    fi
  '';
  meta = {
    description = "Provide shared, lossless-enough Nix parsing and rewriting helpers.";
    mainProgram = pname;
  };
  nativeBuildInputs = nativeDeps;
  passthru.python = python;
  propagatedBuildInputs = pythonDeps;
  pyproject = false;
  src = ./.;
  strictDeps = true;
  version = "0.0.0";
}
