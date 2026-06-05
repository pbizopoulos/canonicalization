{
  inputs,
  pkgs,
  ...
}:
let
  checkName = builtins.baseNameOf ./.;
  packageName = pkgs.lib.removeSuffix "_mutation_testing" checkName;
in
pkgs.runCommand "${checkName}"
  {
    nativeBuildInputs = [
      (pkgs.python312.withPackages (
        _: inputs.self.packages.${pkgs.stdenv.system}.${packageName}.propagatedBuildInputs
      ))
      inputs.self.packages.${pkgs.stdenv.system}.cosmic_ray
    ];
    src = ../../packages/${packageName};
  }
  ''
    export HOME="$PWD"
    workspace="$PWD/workspace"
    rm -rf "$workspace"
    mkdir -p "$workspace"
    cp -R --no-preserve=mode "$src"/. "$workspace"
    cd "$workspace"
    cat > cosmic-ray.toml <<'EOF'
    [cosmic-ray]
    module-path = "main.py"
    timeout = 10.0
    excluded-modules = []
    test-command = "python3 -m pytest -v main.py"
    [cosmic-ray.distributor]
    name = "local"
    EOF
    cosmic-ray init cosmic-ray.toml cosmic-ray.sqlite
    cosmic-ray exec cosmic-ray.toml cosmic-ray.sqlite
    cr-report cosmic-ray.sqlite
    touch "$out"
  ''
