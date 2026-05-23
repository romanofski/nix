{ config, lib, pkgs, ... }:

let
  cfg = config.services.eufy-security-ws;
in {

  options.services.eufy-security-ws = {
    enable = lib.mkEnableOption "Eufy Security WebSocket server";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.callPackage ../../pkgs/eufy-security-ws.nix { };
      defaultText = lib.literalExpression "pkgs.eufy-security-ws";
      description = "The eufy-security-ws package to use.";
    };

    user = lib.mkOption {
      type = lib.types.str;
      default = "eufy-ws";
      description = "User to run the service as.";
    };

    group = lib.mkOption {
      type = lib.types.str;
      default = "eufy-ws";
      description = "Group to run the service as.";
    };

    host = lib.mkOption {
      type = lib.types.str;
      default = "127.0.0.1";
      description = "Address to bind the WebSocket server to.";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 3000;
      description = "Port for the WebSocket server.";
    };

    openFirewall = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Open the configured port in the firewall.";
    };

    configFile = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = null;
      example = "/run/secrets/eufy-security-ws-config.json";
      description = ''
        Path to a config file containing secrets:

        {
        username: your-eufy-email@example.com
        password: your-eufy-password
        }

        Strongly recommended over passing credentials in plain Nix config.
        Use sops-nix or agenix to manage this file.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [{
      assertion = cfg.configFile != null;
      message = ''
        services.eufy-security-ws.configFile must be set and contain
        username and password for your Eufy account.
      '';
    }];

    users.users.${cfg.user} = lib.mkIf (cfg.user == "eufy-ws") {
      isSystemUser = true;
      group = cfg.group;
      home = "/var/lib/eufy-security-ws";
      description = "eufy-security-ws service user";
    };

    users.groups.${cfg.group} = lib.mkIf (cfg.group == "eufy-ws") { };

    systemd.services.eufy-security-ws = {
      description = "Eufy Security WebSocket server";
      after       = [ "network-online.target" ];
      wants       = [ "network-online.target" ];
      wantedBy    = [ "multi-user.target" ];

      serviceConfig = {
        Type             = "simple";
        User             = cfg.user;
        Group            = cfg.group;
        ExecStart        = "${cfg.package}/bin/eufy-security-server --host ${cfg.host} --port ${toString cfg.port} --config ${cfg.configFile}";
        Restart          = "on-failure";
        RestartSec       = 10;
        StateDirectory   = "eufy-security-ws";
        WorkingDirectory = "/var/lib/eufy-security-ws";

        # Hardening
        NoNewPrivileges        = true;
        ProtectSystem          = "strict";
        ProtectHome            = true;
        PrivateTmp             = true;
        PrivateDevices         = true;
        ProtectKernelTunables  = true;
        ProtectKernelModules   = true;
        ProtectControlGroups   = true;
        RestrictSUIDSGID       = true;
        LockPersonality        = true;
        ReadWritePaths         = [ "/var/lib/eufy-security-ws" ];
      };
    };

    networking.firewall = lib.mkIf cfg.openFirewall {
      allowedTCPPorts = [ cfg.port ];
    };
  };

}
