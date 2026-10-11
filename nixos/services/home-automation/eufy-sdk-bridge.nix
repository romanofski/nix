{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.eufy-sdk-bridge;
  startScript = pkgs.writeShellScript "eufy-sdk-bridge-start" ''
    set -eu

    if [ -z "''${CREDENTIALS_DIRECTORY:-}" ]; then
    echo "[bridge-wrapper] CREDENTIALS_DIRECTORY is not set" >&2
    exit 1
    fi

    emailFile="$CREDENTIALS_DIRECTORY/eufy-email"
    passwordFile="$CREDENTIALS_DIRECTORY/eufy-password"

    if [ ! -r "$emailFile" ]; then
    echo "[bridge-wrapper] Cannot read email credential: $emailFile" >&2
    exit 1
    fi

    if [ ! -r "$passwordFile" ]; then
    echo "[bridge-wrapper] Cannot read password credential: $passwordFile" >&2
    exit 1
    fi

    EUFY_EMAIL="$(${pkgs.coreutils}/bin/cat "$emailFile")"
    EUFY_PASSWORD="$(${pkgs.coreutils}/bin/cat "$passwordFile")"

    if [ -z "$EUFY_EMAIL" ]; then
    echo "[bridge-wrapper] Email credential is empty" >&2
    exit 1
    fi

    if [ -z "$EUFY_PASSWORD" ]; then
    echo "[bridge-wrapper] Password credential is empty" >&2
    exit 1
    fi

    export EUFY_EMAIL
    export EUFY_PASSWORD

    # Diagnostic information without exposing the credentials.
    echo "[bridge-wrapper] Credentials loaded: email/password are non-empty"
    echo "[bridge-wrapper] Starting bridge on $BRIDGE_HOST:$BRIDGE_PORT"

    exec ${lib.getExe cfg.package}
  '';
in
{
  options.services.eufy-sdk-bridge = {
    enable = lib.mkEnableOption "Home Assistant Eufy SDK bridge";

    package = lib.mkOption {
      type = lib.types.package;
      description = "The ha-eufy-sdk-bridge package to run.";
    };

    user = lib.mkOption {
      type = lib.types.str;
      default = "eufy-sdk-bridge";
      description = "User under which the bridge runs.";
    };

    group = lib.mkOption {
      type = lib.types.str;
      default = "eufy-sdk-bridge";
      description = "Group under which the bridge runs.";
    };

    stateDirectory = lib.mkOption {
      type = lib.types.str;
      default = "eufy-sdk-bridge";
      description = ''
        StateDirectory name under /var/lib. This must be a directory name,
        not an absolute path.
      '';
    };

    host = lib.mkOption {
      type = lib.types.str;
      default = "127.0.0.1";
      description = "Address on which the Eufy bridge listens.";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 3000;
      description = "Port on which the Eufy bridge listens.";
    };

    openFirewall = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        Open the bridge TCP port in the firewall. This is normally
        unnecessary when Home Assistant and the bridge are on the same host.
      '';
    };

    credentials = {
      emailFile = lib.mkOption {
        type = lib.types.str;
        description = ''
          Path to a file containing the Eufy account email address.
          The file is passed to the service using systemd credentials.
        '';
      };

      passwordFile = lib.mkOption {
        type = lib.types.str;
        description = ''
          Path to a file containing the Eufy account password.
          The file is passed to the service using systemd credentials.
        '';
      };

      country = lib.mkOption {
        type = lib.types.str;
        example = "AU";
        description = ''
          Country in which the Eufy account was registered.
        '';
      };
    };

    go2rtc = {
      apiPort = lib.mkOption {
        type = lib.types.port;
        default = 1984;
        description = "go2rtc HTTP/API port.";
      };

      rtspPort = lib.mkOption {
        type = lib.types.port;
        default = 8554;
        description = "go2rtc RTSP port.";
      };

      webrtcPort = lib.mkOption {
        type = lib.types.port;
        default = 8555;
        description = "go2rtc WebRTC UDP port.";
      };

      openFirewall = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = ''
          Open go2rtc RTSP, API, and WebRTC ports in the firewall.
        '';
      };
    };

    environment = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = {};
      example = {
        LOG_LEVEL = "debug";
      };
      description = "Additional environment variables for the bridge.";
    };

    extraServiceConfig = lib.mkOption {
      type = lib.types.attrs;
      default = {};
      description = "Additional systemd serviceConfig settings.";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = !config.services.go2rtc.enable;
        message = ''
          services.eufy-sdk-bridge starts its own go2rtc instance.
          Disable services.go2rtc to avoid port conflicts.
        '';
      }
      {
        assertion =
          cfg.host == "127.0.0.1"
          || cfg.host == "::1"
          || cfg.host == "localhost"
          || cfg.openFirewall;
          message = ''
            services.eufy-sdk-bridge.host is not a loopback address, but
            services.eufy-sdk-bridge.openFirewall is false.

            Either use 127.0.0.1 or explicitly set openFirewall = true.
          '';
      }
    ];

    warnings = lib.optional
    (cfg.openFirewall && (
      cfg.host == "127.0.0.1"
      || cfg.host == "::1"
      || cfg.host == "localhost"
    ))
    ''
      services.eufy-sdk-bridge.openFirewall is enabled, but the bridge
      only listens on ${cfg.host}. Other machines will not be able to
      connect to it.
    '';

    users.groups = lib.mkIf (cfg.group == "eufy-sdk-bridge") {
      eufy-sdk-bridge = {};
    };

    users.users = lib.mkIf (cfg.user == "eufy-sdk-bridge") {
      eufy-sdk-bridge = {
        isSystemUser = true;
        group = cfg.group;
        home = "/var/lib/${cfg.stateDirectory}";
        createHome = false;
        description = "Eufy SDK bridge service user";
      };
    };

    systemd.services.eufy-sdk-bridge = {
      description = "Home Assistant Eufy SDK Bridge";

      wantedBy = [ "multi-user.target" ];
      wants = [ "network-online.target" ];
      after = [
        "network-online.target"
        "mosquitto.service"
      ];

      environment = {
        BRIDGE_HOST = cfg.host;
        BRIDGE_PORT = toString cfg.port;
        EUFY_COUNTRY = cfg.credentials.country;

        EUFY_SESSION =
          "/var/lib/${cfg.stateDirectory}/.eufy-session.json";

          GO2RTC_CONFIG =
            "/var/lib/${cfg.stateDirectory}/go2rtc.yaml";
      } // cfg.environment;

      serviceConfig = {
        Type = "simple";

        User = cfg.user;
        Group = cfg.group;

        StateDirectory = cfg.stateDirectory;
        StateDirectoryMode = "0700";

        WorkingDirectory = "/var/lib/${cfg.stateDirectory}";

        LoadCredential = [
          "eufy-email:${cfg.credentials.emailFile}"
          "eufy-password:${cfg.credentials.passwordFile}"
        ];

        ExecStart = startScript;

        Restart = "on-failure";
        RestartSec = "5s";

        # Ensure go2rtc is also stopped when the bridge service stops.
        KillMode = "control-group";
        TimeoutStopSec = "30s";

        UMask = "0077";

        NoNewPrivileges = true;
        PrivateTmp = true;
        PrivateDevices = true;

        ProtectSystem = "strict";
        ProtectHome = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectControlGroups = true;

        RestrictSUIDSGID = true;
        LockPersonality = true;

        ReadWritePaths = [
          "/var/lib/${cfg.stateDirectory}"
        ];
      } // cfg.extraServiceConfig;
    };

    networking.firewall.allowedTCPPorts =
      lib.optional cfg.openFirewall cfg.port
      ++ lib.optionals cfg.go2rtc.openFirewall [
        cfg.go2rtc.apiPort
        cfg.go2rtc.rtspPort
      ];

      networking.firewall.allowedUDPPorts =
        lib.optional cfg.go2rtc.openFirewall cfg.go2rtc.webrtcPort;
  };
}
