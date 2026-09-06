{
  inputs,
  modulesPath,
  pkgs,
  ...
}:
let
  opensshAuthorizedKeyFiles = [ ../../prm/developer.pub ];
in
{
  boot.loader.systemd-boot.enable = true;
  disko.devices = {
    disk.main = {
      content = {
        partitions = {
          esp = {
            content = {
              format = "vfat";
              mountpoint = "/boot";
              type = "filesystem";
            };
            end = "512M";
            type = "EF00";
          };
          nix = {
            content = {
              format = "ext4";
              mountpoint = "/nix";
              type = "filesystem";
            };
            size = "100%";
          };
          persistent = {
            content = {
              format = "ext4";
              mountpoint = "/persistent";
              type = "filesystem";
            };
            size = "40G";
          };
          swap = {
            content.type = "swap";
            size = "1G";
          };
        };
        type = "gpt";
      };
      device = "/dev/sda";
    };
    nodev."/" = {
      fsType = "tmpfs";
      mountOptions = [
        "defaults"
        "mode=755"
      ];
    };
  };
  fileSystems."/persistent".neededForBoot = true;
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    (modulesPath + "/profiles/qemu-guest.nix")
    inputs.agenix.nixosModules.age
    inputs.disko.nixosModules.disko
    inputs.preservation.nixosModules.default
  ];
  networking.hostName = baseNameOf ./.;
  nix = {
    gc.automatic = true;
    settings.experimental-features = [
      "flakes"
      "nix-command"
    ];
  };
  nixpkgs = {
    hostPlatform = "x86_64-linux";
    overlays = [
      (_: prev: {
        vmTools = prev.vmTools.override {
          kernelImage = "bzImage";
        };
      })
    ];
  };
  preservation = {
    enable = true;
    preserveAt."/persistent" = {
      directories = [
        {
          directory = "/etc/ssh";
          inInitrd = true;
        }
        {
          directory = "/var/lib/nixos";
          inInitrd = true;
        }
      ];
      files = [
        {
          file = "/etc/machine-id";
          inInitrd = true;
        }
      ];
      users.nixos.directories = [ "." ];
    };
  };
  programs.bash.promptInit = "";
  security.sudo.wheelNeedsPassword = false;
  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
    };
  };
  system.stateVersion = "25.11";
  systemd.suppressedSystemUnits = [ "systemd-machine-id-commit.service" ];
  users = {
    mutableUsers = false;
    users.nixos = {
      extraGroups = [ "wheel" ];
      isNormalUser = true;
      openssh.authorizedKeys.keyFiles = opensshAuthorizedKeyFiles;
    };
  };
  virtualisation.vmVariantWithDisko = {
    disko.devices.disk.main.content.partitions = {
      persistent.size = pkgs.lib.mkForce "1G";
      swap.size = pkgs.lib.mkForce "1M";
    };
    users.users.nixos.password = "password";
    virtualisation = {
      diskSize = 8 * 1024;
      graphics = false;
      memorySize = 4096;
    };
  };
}
