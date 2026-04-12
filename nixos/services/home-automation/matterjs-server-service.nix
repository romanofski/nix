{ config, lib, pkgs, ... }:

let
  cfg = config.services.matterjs-server;
in {
  options.services.matterjs-server = {
    enable = lib.mkEnableOption "Matter.js server";

    storagePath = lib.mkOption {
      type = lib.types.str;
      default = "/var/lib/matterjs-server";
      description = "Path to store Matter server data";
    };

    primaryInterface = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Primary network interface to use";
    };

    vendorID = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Vendor ID. Falls back to 0xfff1";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 5580;
      description = "WebSocket port to listen on";
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.matterjs-server = {
      description = "Matter.js Server";
      wantedBy = [ "multi-user.target" ];
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];

      serviceConfig = {
        ExecStart = lib.concatStringsSep " " ([
          "${pkgs.matterjs-server}/bin/matter-server"
          "--storage-path ${cfg.storagePath}"
          "--port ${toString cfg.port}"
        ] ++ lib.optionals (cfg.primaryInterface != null) [
          "--primary-interface ${cfg.primaryInterface}"
        ] ++ lib.optionals (cfg.vendorID != null) [
          "--vendorid ${cfg.vendorID}"
        ]);
        StateDirectory = "matterjs-server";
        Restart = "on-failure";
        RestartSec = "5s";
      };
    };
  };
}
