{ pkgs, ... }:
let
  nativeDeps = [ ];
  pname = baseNameOf ./.;
  python = pkgs.python3;
  pythonDeps = [ ];
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
    description = "Remove empty lines from explicitly selected text files.";
    mainProgram = pname;
  };
  nativeBuildInputs = nativeDeps;
  passthru = {
    inherit python;
    canonicalization.tests = [
      "Main processes explicit paths."
      "Process file skips binary files and symbolic links."
      "Remove empty lines preserves nonempty and invalid UTF-8 lines."
    ];
  };
  propagatedBuildInputs = pythonDeps;
  pyproject = false;
  src = ./.;
  strictDeps = true;
  version = "0.0.0";
}
