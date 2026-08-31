{ inputs, pkgs, ... }:
let
  host = pkgs.lib.removeSuffix "VmWithDisko" (baseNameOf ./.);
in
pkgs.runCommand (baseNameOf ./.)
  {
    buildInputs = [ inputs.self.nixosConfigurations.${host}.config.system.build.vmWithDisko ];
  }
  ''
    touch "$out"
  ''
