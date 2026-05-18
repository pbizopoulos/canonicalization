{
  inputs,
  pkgs,
  ...
}:
pkgs.runCommand "host_default"
  {
    nativeBuildInputs = [
      inputs.self.nixosConfigurations.default.config.system.build.vm
    ];
  }
  ''
    touch "$out"
  ''
