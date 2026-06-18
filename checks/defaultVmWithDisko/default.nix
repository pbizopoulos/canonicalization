{
  inputs,
  pkgs,
  ...
}:
let
  host = pkgs.lib.removeSuffix "VmWithDisko" (builtins.baseNameOf ./.);
in
pkgs.runCommand (builtins.baseNameOf ./.) {
  buildInputs = [
    inputs.self.nixosConfigurations.${host}.config.system.build.vmWithDisko
  ];
} "touch $out"
