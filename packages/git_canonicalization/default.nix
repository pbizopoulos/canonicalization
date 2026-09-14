{ inputs, pkgs, ... }:
let
  nativeDeps = [ ];
  pname = baseNameOf ./.;
  python = pkgs.python3;
  pythonDeps = [
    inputs.self.packages.${pkgs.stdenv.system}.nix_syntax
    pkgs.git
    pkgs.nix
  ];
  shellHook = "";
in
python.pkgs.buildPythonPackage {
  inherit pname;
  inherit shellHook;
  installPhase = ''
    install -Dm644 main.py "$out/${python.sitePackages}/$pname/__init__.py"
    mkdir -p "$out/bin"
    printf '%s\n' '#!${python.interpreter}' "from $pname import main" 'main()' > "$out/bin/$pname"
    chmod 755 "$out/bin/$pname"
    if [ -d prm ]; then
      cp -R prm/ "$out/${python.sitePackages}/$pname/"
    fi
  '';
  meta = {
    description = "Canonicalize home repositories and manage canonical flake repositories.";
    mainProgram = pname;
  };
  nativeBuildInputs = nativeDeps;
  passthru = {
    inherit python;
    canonicalization.tests = [
      ".gitignore patterns are globally sorted."
      "Add and rm manage hosts as explicit resources."
      "Canonicalize is the convergence command."
      "Convergence derives checks and removes orphans."
      "Convergence preserves forgejo workflow."
      "Convergence preserves root and package scratch only."
      "Convergence stages untracked opaque package files."
      "Coverage default matches current template."
      "Domain resources in prm remain an unconstrained nix package."
      "Git clean arguments are profile specific."
      "Help command is equivalent to help option."
      "Home checkout converges origin and gitlink."
      "Home checkout rejects dirty or unpublished head."
      "Home initialization uses canonical ignore policy."
      "Host check falls back to regular vm."
      "Host check requires its host."
      "Host names use camel case."
      "Html styles and scripts are optional."
      "Meta description uses nix syntax."
      "Mv rejects cross resource and noncanonical paths."
      "Mv renames packages generated checks and hosts."
      "Orphan coverage check is not canonical."
      "Package named check is not a canonical coverage check."
      "Python default derives normalized test list."
      "Python default preserves custom attributes."
      "Python default requires static build fields."
      "Python package allows latex resources in prm."
      "Python scaffold escapes arbitrary description."
      "Python scaffold installs optional prm resources."
      "Remote paths and test names."
      "Removed status interfaces are rejected."
      "Repository layout error explains how to place unrestricted files."
      "Single force cleanup rejects nested git repository."
      "Standalone check is not canonical."
      "Subcommand help describes arguments and hides internal options."
      "Top level help is concise and conventional."
    ];
  };
  propagatedBuildInputs = pythonDeps;
  pyproject = false;
  src = ./.;
  strictDeps = true;
  version = "0.0.0";
}
