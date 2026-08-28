{pkgs ? import <nixpkgs> {}}: let
  nixSyntax = import ../nix_syntax/default.nix {inherit pkgs;};
  python = pkgs.python3;
  pythonDeps = [nixSyntax];
in
  python.pkgs.buildPythonPackage rec {
    installPhase = ''
      install -Dm644 main.py "$out/${python.sitePackages}/nix_remove_defaults.py"
      install -Dm755 main.py "$out/bin/$pname"
    '';
    meta.mainProgram = pname;
    nativeBuildInputs = [pkgs.makeWrapper];
    passthru.python = python;
    pname = baseNameOf ./.;
    postFixup = ''
      wrapProgram "$out/bin/$pname" --prefix PATH : ${pkgs.lib.makeBinPath [pkgs.alejandra pkgs.nix]}
    '';
    propagatedBuildInputs = pythonDeps;
    pyproject = false;
    src = ./.;
    strictDeps = true;
    version = "0.0.0";
  }
