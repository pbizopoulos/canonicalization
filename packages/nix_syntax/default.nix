{
  pkgs ? import <nixpkgs> { },
}:
let
  python = pkgs.python3;
  pythonDeps = [ python.pkgs.tree-sitter-language-pack ];
in
python.pkgs.buildPythonPackage rec {
  installPhase = ''
    install -Dm644 main.py "$out/${python.sitePackages}/${pname}.py"
    install -Dm755 main.py "$out/bin/${pname}"
  '';
  meta.mainProgram = pname;
  nativeBuildInputs = [ pkgs.makeWrapper ];
  passthru.python = python;
  pname = baseNameOf ./.;
  postFixup = ''
    wrapProgram "$out/bin/${pname}" --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.alejandra ]}
  '';
  propagatedBuildInputs = pythonDeps;
  pyproject = false;
  src = ./.;
  strictDeps = true;
  version = "0.0.0";
}
