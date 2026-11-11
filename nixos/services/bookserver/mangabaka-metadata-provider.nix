{ config, lib, pkgs, ... }:

let
  cfg = config.services.mangabaka-metadata-provider;
in {
  options.services.mangabaka-metadata-provider = {
    enable = lib.mkEnableOption "MangaBaka metadata provider for BookOrbit";

    package = lib.mkPackageOption pkgs "mangabaka-metadata-provider" {};

    port = lib.mkOption {
      type = lib.types.port;
      default = 8080;
      description = "Port the provider listens on.";
    };

    openFirewall = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Open the port in the firewall (not needed if BookOrbit runs on the same host).";
    };

    environment = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = {};
      description = "Extra environment variables for the provider.";
    };

    environmentFile = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = null;
      description = "File with secret env vars (e.g. API keys), e.g. from sops-nix.";
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.mangabaka-provider = {
      description = "MangaBaka metadata provider";
      wantedBy = [ "multi-user.target" ];
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];

      environment = {
        PORT = toString cfg.port;   # adjust to whatever the binary actually reads
      } // cfg.environment;

      serviceConfig = {
        ExecStart = lib.getExe cfg.package;
        EnvironmentFile = lib.mkIf (cfg.environmentFile != null) cfg.environmentFile;
        Restart = "on-failure";

        # Equivalent of USER 65534 + scratch image
        DynamicUser = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
        PrivateDevices = true;
        NoNewPrivileges = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectControlGroups = true;
        RestrictAddressFamilies = [ "AF_INET" "AF_INET6" ];
        RestrictNamespaces = true;
        LockPersonality = true;
        MemoryDenyWriteExecute = true;
        SystemCallArchitectures = "native";
        CapabilityBoundingSet = "";
      };
    };

    networking.firewall.allowedTCPPorts = lib.mkIf cfg.openFirewall [ cfg.port ];
  };
}
