{
  inputs,
  pkgs ? import <nixpkgs> { },
}:
let
  packageName = baseNameOf ./.;
in
pkgs.writeShellApplication {
  name = packageName;
  runtimeInputs = [
    pkgs.jq
    pkgs.openssh
    (pkgs.opentofu.withPlugins (p: [
      p.hashicorp_external
      p.hashicorp_local
      p.hashicorp_null
      p.hetznercloud_hcloud
    ]))
  ];
  text = ''
    # shellcheck disable=SC1091
    source ${
      pkgs.lib.getExe (
        inputs.agenix-shell.lib.installationScript pkgs.stdenv.system {
          secrets.secrets.file = ../../secrets/secrets.age;
        }
      )
    }
    # shellcheck disable=SC2086,SC2163,SC2154
    export $secrets
    workdir=$(mktemp -d)
    cp -r ${../..}/. "$workdir/"
    chmod -R u+w "$workdir"
    rm -rf "$workdir/packages/${packageName}/.terraform" "$workdir/packages/${packageName}/.terraform.lock.hcl"
    tofu -chdir="$workdir/packages/${packageName}" init -reconfigure
    tofu -chdir="$workdir/packages/${packageName}" apply
  '';
}
