{ pkgs, ... }:
let
  pname = baseNameOf ./.;
  providerPlugins = p: [
    p.hashicorp_external
    p.hashicorp_local
    p.hashicorp_null
    p.hetznercloud_hcloud
  ];
  runtimeDeps = [ pkgs.openssh ];
  tofu = pkgs.opentofu.withPlugins providerPlugins;
in
pkgs.writeShellApplication {
  meta.description = "A Terraform template package for deploying a host.";
  name = pname;
  runtimeInputs = runtimeDeps ++ [
    pkgs.git
    tofu
  ];
  text = ''
    repository_root=$(git rev-parse --show-toplevel)
    package_root="$repository_root/packages/${pname}"
    configuration_root="$package_root/prm"
    export TF_DATA_DIR="$package_root/tmp/.terraform"
    mkdir -p "$TF_DATA_DIR"
    tofu -chdir="$configuration_root" init -lockfile=readonly
    exec tofu -chdir="$configuration_root" apply "$@"
  '';
}
