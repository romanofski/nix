{ config,secrets, ... }:

let
  tailscaleDomain = "mystique.kamori-gila.ts.net";
in
  {
    networking.firewall.allowedTCPPorts = [ 80 443 ];

    services.caddy = {
      enable = true;

      virtualHosts."${tailscaleDomain}".extraConfig = ''
        handle {
          reverse_proxy http://localhost:8123
        }
      '';
    };

    services.home-assistant = {
      enable = true;
      extraPackages = python3Packages: with python3Packages; [
        gtts
        pymiele
        aiohue
      ];
      extraComponents = [
        "default_config"
        "met"
        "openweathermap"
        "esphome"
        "google_translate"
        "radio_browser"
        "glances"
        "hue"
        "miele"
        "forecast_solar"
      ];

      config = {
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
