{ pkgs, ... }:
let
  pname = baseNameOf ./.;
  python = pkgs.python3;
  wordnetData = python.pkgs.buildPythonPackage {
    dontUnpack = true;
    installPhase = ''
      mkdir -p "$out/${python.sitePackages}/python_name_graph_data"
      touch "$out/${python.sitePackages}/python_name_graph_data/__init__.py"
      ln -s ${pkgs.nltk-data.wordnet}/corpora "$out/${python.sitePackages}/python_name_graph_data/corpora"
    '';
    pname = "python-name-graph-data";
    pyproject = false;
    version = "0.0.0";
  };
in
python.pkgs.buildPythonPackage {
  inherit pname;
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
    description = "Build a directed graph of lemmatized Python declaration tokens";
    mainProgram = pname;
  };
  nativeCheckInputs = [ pkgs.graphviz ];
  passthru.python = python;
  propagatedBuildInputs = [
    python.pkgs.nltk
    wordnetData
  ];
  pyproject = false;
  src = ./.;
  strictDeps = true;
  version = "0.0.0";
}
