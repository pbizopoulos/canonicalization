{ inputs, pkgs, ... }:
let
  pname = builtins.replaceStrings [ "-" ] [ "_" ] (baseNameOf ./.);
  python = pkgs.python3;
in
python.pkgs.buildPythonPackage {
  inherit pname;
  installPhase = ''
    install -Dm644 main.py "$out/${python.sitePackages}/$pname/__init__.py"
    mkdir -p "$out/bin"
    printf '%s\n' '#!${python.interpreter}' "from $pname import main" 'main()' > "$out/bin/${baseNameOf ./.}"
    chmod 755 "$out/bin/${baseNameOf ./.}"
    if [ -d prm ]; then
      cp -R prm/ "$out/${python.sitePackages}/$pname/"
    fi
  '';
  meta = {
    description = "Canonicalize home repositories and manage flake repository layouts";
    mainProgram = baseNameOf ./.;
  };
  passthru.python = python;
  propagatedBuildInputs = [
    inputs.self.packages.${pkgs.stdenv.system}.nix_syntax
    pkgs.git
    pkgs.nix
  ];
  pyproject = false;
  src = ./.;
  strictDeps = true;
  version = "0.0.0";
}
