{
  inputs,
  pkgs ? import <nixpkgs> {},
}: let
  python = pkgs.python3;
in
  python.pkgs.buildPythonPackage rec {
    installPhase = ''
      install -Dm644 main.py "$out/${python.sitePackages}/${pname}.py"
      install -Dm755 main.py "$out/bin/${pname}"
      if [ -d prm ]; then
        cp -R prm/ "$out/${python.sitePackages}/"
        cp -R prm/ "$out/bin/"
      fi
    '';
    meta = {
      description = "Remove new lines from explicitly selected text files.";
      mainProgram = pname;
    };
    passthru.python = python;
    pname = baseNameOf ./.;
    pyproject = false;
    shellHook = ''
      source ${
        pkgs.lib.getExe (
          inputs.agenix-shell.lib.installationScript pkgs.stdenv.system {
            secrets.secrets.file = ../../secrets/secrets.age;
          }
        )
      }
      export $secrets
    '';
    src = ./.;
    strictDeps = true;
    version = "0.0.0";
  }
