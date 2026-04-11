{ config,secrets, ... }:

let
  tailscaleDomain = "mystique.kamori-gila.ts.net";
in
  {
    networking.firewall.allowedTCPPorts = [ 80 443 ];

    users.users.hass.extraGroups = [ "dialout" ]; # ZBT-2

    services.caddy = {
      enable = true;

      virtualHosts."${tailscaleDomain}".extraConfig = ''
        handle {
          reverse_proxy http://localhost:8123
        }
      '';
    };

    services.avahi = {
      enable = true;
      nssmdns4 = true;
    };

    services.matter-server = {
      enable = true;
      extraArgs = ["--primary-interface" "wlp0s20f3" ];
    };
    services.openthread-border-router = {
      enable = true;
      backboneInterfaces = [ "wlp0s20f3" ];
      radio = {
        device = "/dev/ttyACM0";
        baudRate = 460800;
        flowControl = true;
      };
    };

    services.home-assistant = {
      enable = true;
      openFirewall = true;
      extraPackages = python3Packages: with python3Packages; [
        gtts
        pymiele
        aiohue
        ical
        gcal-sync
      ];
      extraComponents = [
        "default_config"
        "esphome"
        "forecast_solar"
        "glances"
        "google_translate"
        "history"
        "history_stats"
        "homeassistant_hardware"
        "homeassistant_sky_connect"
        "hue"
        "logbook"
        "matter"
        "met"
        "miele"
        "mobile_app"
        "moon"
        "openweathermap"
        "otbr"
        "persistent_notification"
        "radio_browser"
        "recorder"
        "statistics"
        "sun"
        "system_health"
        "thread"
        "zeroconf"
        "zha"
      ];

      config = {
        history = {};
        mobile_app = {};
        http = {
          use_x_forwarded_for = true;
          trusted_proxies = [
            "127.0.0.1"
            "::1"
          ];
        };
        automation = "!include automations.yaml";
        logger = {
          default = "warning";
          logs = {
            "homeassistant.components.automation" = "debug";
            "homeassistant.config" = "debug";
            "homeassistant.core" = "info";
            "homeassistant.helpers.entity_platform" = "debug";
          };
        };
        homeassistant = {
          name = "Home";
          latitude = secrets.latitude;
          longitude = secrets.longitude;
          elevation = secrets.elevation;
          unit_system = "metric";
          temperature_unit = "C";
          time_zone = "Australia/Brisbane";
        };
      };
    };
  }
