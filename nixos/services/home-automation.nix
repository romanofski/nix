{ pkgs,config,secrets, ... }:

let
  tailscaleDomain = "mystique.kamori-gila.ts.net";
  networkInterface = "enp0s20f0u3u3";
  vendorID = "4939";
  rainGaugeIds = ["213" "5"]; # also found 11 and 10 but didn't change
  rainGaugeSensors = map (id: {
    name = "Rain Gauge ${id}";
    state_topic = "rtl_433/Acurite-Rain/${id}";
    value_template = "{{ value_json.rain_mm | float }}";
    unit_of_measurement = "mm";
    device_class = "precipitation";
    unique_id = "rain_gauge_${id}";
  }) rainGaugeIds;
  sensorsDefinitions = [
    { 
      name = "Roof Cavity";
      id = "213";
    }
    { 
      name = "Living Room";
      id = "228";
    }
    { 
      name = "Master Bedroom Bathroom";
      id = "252";
    }
    {
      name = "Outdoor";
      id = "197";
    }
    {
      name = "Garage";
      id = "179";
    }
    {
      name = "Fionns Bedroom";
      id = "35";
    }
    {
      name = "Felicias Bedroom";
      id = "172";
    }
  ];
  mkBatterySensors = { name, id, ... }: [
    {
      name = "${name} Battery";
      state_topic = "rtl_433/Bresser-3CH/${id}";
      value_template = "{{ value_json.battery_ok }}";
      payload_on = "0"; # battery_ok=0 means LOW
      payload_off = "1"; # battery_ok=1 means OK
      device_class = "battery";
      unique_id = "bresser_${id}_battery";
      entity_category = "diagnostic";
    }
  ];
  mkMQTTSensors = { name, id }: let

    topic = "rtl_433/Bresser-3CH/${id}";
  in
  [
    {
      name = "${name} Temperature";
      state_topic = topic;
      value_template = "{{ value_json.temperature_C }}";
      unit_of_measurement = "°C";
      device_class = "temperature";
      unique_id = "nexus_th_${id}_temp";
      }
      {
      name = "${name} Humidity";
      state_topic = topic;
      value_template = "{{ value_json.humidity }}";
      unit_of_measurement = "%";
      device_class = "humidity";
      unique_id = "nexus_th_${id}_humidity";
      }
      {
      name = "${name} RSSI";
      state_topic = topic;
      value_template = "{{ value_json.rssi | round(1) }}";
      unit_of_measurement = "dBm";
      device_class = "signal_strength";
      unique_id = "bresser_${id}_rssi";
      entity_category = "diagnostic";
      }
      {
      name = "${name} SNR";
      state_topic = topic;
      value_template = "{{ value_json.snr | round(1) }}";
      unit_of_measurement = "dB";
      device_class = "signal_strength";
      unique_id = "bresser_${id}_snr";
      entity_category = "diagnostic";
      }
      {
      name = "${name} Noise";
      state_topic = topic;
      value_template = "{{ value_json.noise | round(1) }}";
      unit_of_measurement = "dBm";
      device_class = "signal_strength";
      unique_id = "bresser_${id}_noise";
      entity_category = "diagnostic";
      }
      ];
      in
  {
    networking.firewall.allowedTCPPorts = [
      443
      5580 # matter-server dashboard
    ];
    networking.firewall.allowedUDPPorts = [
      5353  # mDNS/ how phone finds thread border router
      5540  # matter protocol
      49154  # MeshCoP / Thread commissioning service port
    ];
    networking.firewall.trustedInterfaces = [ "wpan0" ];

    users.users.hass.extraGroups = [ "dialout" ]; # ZBT-2

    services.mosquitto = {
      enable = true;
      listeners = [
        {
          acl = [
            "topic readwrite #"
          ];
          omitPasswordAuth = true;
          settings.allow_anonymous = true;
        }
      ];
    };

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

    services.matter-server.enable = false;
    services.matterjs-server = {
      enable = true;
      primaryInterface = networkInterface;
      vendorID = vendorID;
    };
    services.openthread-border-router = {
      enable = true;
      backboneInterfaces = [ networkInterface ];
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
        "manual_mqtt"
        "matter"
        "met"
        "miele"
        "mobile_app"
        "moon"
        "mqtt"
        "mqtt_eventstream"
        "mqtt_json"
        "mqtt_room"
        "mqtt_statestream"
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
        logbook = {};
        http = {
          use_x_forwarded_for = true;
          trusted_proxies = [
            "127.0.0.1"
            "::1"
          ];
        };
        mqtt = {
          sensor = builtins.concatMap mkMQTTSensors sensorsDefinitions
          ++ rainGaugeSensors ++ [
            {
              name = "Power 1";
              state_topic = "rtl_433/Oregon-CM180i/18976";
              value_template = "{{ value_json.power1_W | int }}";
              unit_of_measurement = "W";
              device_class = "power";
              unique_id = "cm180i_power1";
            }
            {
              name = "Power 3";
              state_topic = "rtl_433/Oregon-CM180i/18976";
              value_template = "{{ value_json.power3_W | int }}";
              unit_of_measurement = "W";
              device_class = "power";
              unique_id = "cm180i_power3";
            }
          ];
          binary_sensor = builtins.concatMap mkBatterySensors sensorsDefinitions;
        };
        template = [
          {
            sensor = [
              {
                name = "Rain Gauge Average";
                unit_of_measurement = "mm";
                device_class = "precipitation";
                unique_id = "rain_gauge_average";
                state = ''
                  {{ [
                  ${builtins.concatStringsSep ",\n    " (map (id: "states('sensor.rain_gauge_${id}') | float") rainGaugeIds)}
                  ] | average }}
                '';
              }
            ];
          }
        ];

        utility_meter = {
          daily_rainfall = {
            source = "sensor.rain_gauge_average";
            cycle = "daily";
          };
          monthly_rainfall = {
            source = "sensor.rain_gauge_average";
            cycle = "monthly";
          };
        };
        sensor = [
          {
            platform = "template";
            sensors = {
              ambient_temperature_min = {
                friendly_name = "Ambient Temperature";
                unit_of_measurement = "°C";
                device_class = "temperature";
                value_template = ''
                  {{ [
                  states('sensor.roof_cavity_temperature') | float,
                  states('sensor.outdoor_temperature') | float
                  ] | min | round(1)
                  }}
                '';
              };
            };
          }
          {
            platform = "derivative";
            name = "Bathroom Humidity Rate";
            source = "sensor.master_bedroom_bathroom_humidity";
            time_window = "00:05:00";
            unit_time = "min";
            }
        ];
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
          latitude = secrets.homeassistant.latitude;
          longitude = secrets.homeassistant.longitude;
          elevation = secrets.homeassistant.elevation;
          unit_system = "metric";
          temperature_unit = "C";
          time_zone = "Australia/Brisbane";
        };
      };
    };
  }
