{
  config,
  inputs,
  modulesPath,
  pkgs,
  ...
}:
let
  hostName = baseNameOf ./.;
  opensshAuthorizedKeyFiles = [
    ./../../prm/developer.pub
  ];
  serviceName = "django_template";
  servicePackage = inputs.self.packages.${pkgs.stdenv.system}.django_template;
in
{
  age.secrets.secrets-env = {
    file = ../../secrets/secrets.age;
    group = serviceName;
    owner = serviceName;
  };
  boot = {
    initrd.systemd.enable = true;
    loader.systemd-boot.enable = true;
  };
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
  environment.systemPackages = [ ];
  fileSystems."/persistent".neededForBoot = true;
  imports = [
    inputs.agenix.nixosModules.age
    inputs.disko.nixosModules.disko
    inputs.preservation.nixosModules.default
    (modulesPath + "/installer/scan/not-detected.nix")
    (modulesPath + "/profiles/qemu-guest.nix")
  ];
  networking = {
    inherit hostName;
    firewall.allowedTCPPorts = [
      443
      80
    ];
  };
  nix = {
    gc.automatic = true;
    settings.experimental-features = [
      "flakes"
      "nix-command"
    ];
  };
  nixpkgs.hostPlatform = "x86_64-linux";
  preservation = {
    enable = true;
    preserveAt."/persistent" = {
      directories = [
        "/var/lib/acme"
        "/var/lib/postgresql"
        "/var/lib/${serviceName}"
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
    };
  };
  programs.bash.promptInit = "";
  security.sudo.wheelNeedsPassword = false;
  services = {
    nginx = {
      enable = true;
      virtualHosts.${hostName} = {
        default = true;
        enableACME = false;
        forceSSL = false;
        locations."/" = {
          proxyPass = "http://127.0.0.1:8000";
          recommendedProxySettings = true;
        };
      };
    };
    openssh = {
      enable = true;
      settings = {
        PasswordAuthentication = false;
        PermitRootLogin = "no";
      };
    };
    postgresql = {
      authentication = pkgs.lib.mkOverride 10 ''
        # type database user address method
        local all all peer
      '';
      enable = true;
      ensureDatabases = [
        serviceName
      ];
      ensureUsers = [
        {
          ensureDBOwnership = true;
          name = serviceName;
        }
      ];
    };
  };
  system.stateVersion = "25.11";
  systemd = {
    services.${serviceName} = {
      after = [
        "network.target"
        "postgresql.service"
      ];
      environment = {
        ALLOWED_HOSTS = "${hostName},127.0.0.1,localhost,[::1]";
        APP_NAME = "Django Starter";
        CSRF_COOKIE_SECURE = "0";
        CSRF_TRUSTED_ORIGINS = "http://${hostName}";
        DATABASE_ENGINE = "postgresql";
        DATABASE_NAME = serviceName;
        DB_HOST = "/run/postgresql";
        DB_PORT = "5432";
        DB_USER = serviceName;
        DEFAULT_FROM_EMAIL = "starter@example.com";
        EMAIL_BACKEND = "django.core.mail.backends.console.EmailBackend";
        HOST = "127.0.0.1";
        PORT = "8000";
        SECURE_HSTS_SECONDS = "0";
        SECURE_PROXY_SSL_HEADER = "0";
        SECURE_SSL_REDIRECT = "0";
        SESSION_COOKIE_SECURE = "0";
        STATIC_ROOT = "/var/lib/${serviceName}/staticfiles";
        SUPPORT_EMAIL = "support@example.com";
      };
      serviceConfig = {
        EnvironmentFile = config.age.secrets.secrets-env.path;
        ExecStart = "${servicePackage}/bin/${serviceName}";
        ExecStartPre = [
          "${servicePackage}/bin/${serviceName}-manage migrate --noinput"
        ];
        Group = serviceName;
        Restart = "always";
        RestartSec = 5;
        StateDirectory = serviceName;
        User = serviceName;
      };
      wantedBy = [
        "multi-user.target"
      ];
    };
    suppressedSystemUnits = [
      "systemd-machine-id-commit.service"
    ];
  };
  users = {
    allowNoPasswordLogin = false;
    groups.${serviceName} = { };
    mutableUsers = false;
    users = {
      ${serviceName} = {
        group = serviceName;
        home = "/var/lib/${serviceName}";
        isSystemUser = true;
      };
      nixos = {
        extraGroups = [
          "wheel"
        ];
        isNormalUser = true;
        openssh.authorizedKeys.keyFiles = opensshAuthorizedKeyFiles;
      };
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
