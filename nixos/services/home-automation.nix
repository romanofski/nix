{ pkgs,config,secrets, ... }:

let
  pythonEnv = pkgs.python3.withPackages (ps: [ ps.paho-mqtt ]);
  batteryPublisher = pkgs.writeScript "publish-bat0-mqtt" ''
    #!${pythonEnv}/bin/python3
    ${builtins.readFile ../scripts/battery_mqtt.py}
    '';
  batterySensors = [
      {
        name = "BAT0 Capacity";
        state_topic = "home/battery/bat0/capacity";
        unit_of_measurement = "%";
        device_class = "battery";
      }
      {
        name = "BAT0 Status";
        state_topic = "home/battery/bat0/status";
      }
      {
        name = "BAT0 Charge Type";
        state_topic = "home/battery/bat0/charge_type";
      }
      {
        name = "BAT0 Cycle Count";
        state_topic = "home/battery/bat0/cycle_count";
      }
  ];
  soilSensors = [
    { name = "Garden Bed"; id = "0fe2fa"; }
  ];

  mkSoilSensors = { name, id }: [
    {
      name = "${name} Moisture";
      state_topic = "rtl_433/Fineoffset-WH51/${id}";
      value_template = "{{ value_json.moisture }}";
      unit_of_measurement = "%";
      device_class = "moisture";
      unique_id = "soil_${id}_moisture";
      force_update = true;
    }
    {
      name = "${name} Battery";
      state_topic = "rtl_433/Fineoffset-WH51/${id}";
      value_template = "{{ value_json.battery_mV }}";
      unit_of_measurement = "mV";
      unique_id = "soil_${id}_battery_mv";
      entity_category = "diagnostic";
      force_update = true;
    }
    {
      name = "${name} SNR";
      state_topic = "rtl_433/Fineoffset-WH51/${id}";
      value_template = "{{ value_json.snr | round(1) }}";
      unit_of_measurement = "dB";
      unique_id = "soil_${id}_snr";
      entity_category = "diagnostic";
      force_update = true;
    }
    {
      name = "${name} RSSI";
      state_topic = "rtl_433/Fineoffset-WH51/${id}";
      value_template = "{{ value_json.rssi | round(1) }}";
      unit_of_measurement = "dBm";
      device_class = "signal_strength";
      unique_id = "soil_${id}_rssi";
      entity_category = "diagnostic";
      force_update = true;
    }
    {
      name = "${name} Battery";
      state_topic = "rtl_433/Fineoffset-WH51/${id}";
      value_template = ''
        {{ [[((value_json.battery_mV - 1200) / (1800 - 1200) * 100) | round, 0] | max, 100] | min }}
      '';
      unit_of_measurement = "%";
      device_class = "battery";
      unique_id = "soil_${id}_battery_pct";
      entity_category = "diagnostic";
      force_update = true;
    }
  ];

  tailscaleDomain = "mystique.kamori-gila.ts.net";
  networkInterface = "enp0s20f0u3u3";
  vendorID = "4939";
  makeMotionSensors = { name, code }: [
    {
      name = "${name} Motion";
      state_topic = "rtl_433/PIR";
      value_template = ''
        {{ "ON" if "{25}${code}" in value_json.codes else "OFF" }}
        '';
      payload_on = "ON";
      payload_off = "OFF";
      device_class = "motion";
      unique_id = "pir_${code}_motion";
      off_delay = 300;
    }
  ];
  motionSensors = [
    { name = "Computer Room"; code = "17ffa88"; }
  ];
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
      name = "Living Room";
      id = "228";
    }
    {
      name = "Main Toilet";
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
      unique_id = "bresser_${id}_temp";
      force_update = true;
    }
    {
      name = "${name} Humidity";
      state_topic = topic;
      value_template = "{{ value_json.humidity }}";
      unit_of_measurement = "%";
      device_class = "humidity";
      unique_id = "bresser_${id}_humidity";
      force_update = true;
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
  sops.secrets.username = {};
  sops.secrets.password = {};

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
    bluetoothAdapter = "0";
  };

  sops.templates."config.json" = {
    content = ''
      {
      "username": "${config.sops.placeholder.username}",
      "password": "${config.sops.placeholder.password}",
      "country": "AU",
      "language": "en",
      "p2pConnectionSetup": 2,
      "persistentDir": "${config.users.users.${config.services.eufy-security-ws.user}.home}"
      }
    '';
    owner = config.services.eufy-security-ws.user;
    mode  = "0600";
  };
  services.eufy-security-ws = {
    enable= true;
    configFile = config.sops.templates."config.json".path;
    port = 3080;
  };
  services.go2rtc = {
    enable = true;
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

  systemd.services.publish-bat0-mqtt = {
    description = "Publish BAT0 battery stats to MQTT";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${batteryPublisher}";
    };
  };
  systemd.timers.publish-bat0-mqtt = {
    description = "Timer for BAT0 MQTT publisher";
    wantedBy = ["timers.target"];
    timerConfig = {
      OnBootSec = "30s";
      OnUnitActiveSec = "30s";
      Unit = "publish-bat0-mqtt.service";
    };
  };

  # Ensure the directory exists with correct ownership
  systemd.tmpfiles.rules = [
    "d ${config.services.home-assistant.configDir}/www              0755 hass hass - -"
    "d ${config.services.home-assistant.configDir}/www/snapshots    0755 hass hass - -"
  ];

  services.home-assistant = {
    enable = true;
    openFirewall = true;
    extraPackages = python3Packages: with python3Packages; [
      gtts
      pymiele
      aiohue
      ical
      gcal-sync
      aioelectricitymaps
    ];
    customLovelaceModules = with pkgs.home-assistant-custom-lovelace-modules; [
      apexcharts-card
    ] ++ [
      (pkgs.callPackage ../pkgs/home-assistant-custom-lovelace-modules/power-flow-card-plus.nix {})
    ];
    customComponents = [
      (pkgs.callPackage ../pkgs/home-assistant-custom-components/eufy_security.nix {})
      (pkgs.callPackage ../pkgs/home-assistant-custom-components/webrtc.nix {})
      (pkgs.callPackage ../pkgs/home-assistant-custom-components/ha_bom_australia.nix {})
      (pkgs.callPackage ../pkgs/home-assistant-custom-components/sigenergy_local_modbus.nix {})
    ];
    extraComponents = [
      "default_config"
      "energy"
      "esphome"
      "forecast_solar"
      "frontend"
      "glances"
      "google_translate"
      "history"
      "history_stats"
      "homeassistant_hardware"
      "homeassistant_sky_connect"
      "hue"
      "logbook"
      "lovelace"
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
      "systemmonitor"
      "thread"
      "utility_meter"
      "zeroconf"
      "zha"
    ];

    config = {
      history = {};
      mobile_app = {};
      logbook = {};
      energy = {};
      http = {
        use_x_forwarded_for = true;
        trusted_proxies = [
          "127.0.0.1"
          "::1"
        ];
      };
      allowlist_external_dirs = [
        "/var/lib/hass/www/snapshots"
      ];
      mqtt = {
        sensor = builtins.concatMap mkMQTTSensors sensorsDefinitions
        ++ (builtins.concatMap mkSoilSensors soilSensors)
        ++ batterySensors
        ++ (pkgs.callPackage ./home-automation/ws90-sensors.nix {});
        binary_sensor = (builtins.concatMap mkBatterySensors sensorsDefinitions)
        ++ (builtins.concatMap makeMotionSensors motionSensors);
      };
      template = [
        {
          trigger = [{ trigger = "state"; entity_id = ["sensor.ws90_rain_since_9am"]; }];
          condition = [{
            condition = "template";
            value_template = ''
              {{ trigger.from_state is not none
                 and trigger.to_state is not none
                 and is_number(trigger.from_state.state)
                 and is_number(trigger.to_state.state)
                 and trigger.to_state.state | float > trigger.from_state.state | float }}
            '';
          }];
          sensor = [{
            name = "WS90 Last Rain Timestamp";
            unique_id = "ws90_last_rain_timestamp";
            device_class = "timestamp";
            state = "{{ now().isoformat() }}";
          }];
        }
        {
          trigger = [{ trigger = "state"; entity_id = ["sensor.ws90_rain_since_9am"]; }];
          condition = [{
            condition = "template";
            value_template = ''
              {% set threshold = states('input_number.ws90_rain_threshold_mm') | float(5) %}
              {{ trigger.from_state is not none
              and trigger.to_state is not none
              and is_number(trigger.from_state.state)
              and is_number(trigger.to_state.state)
              and trigger.to_state.state | float >= threshold
              and trigger.from_state.state | float < threshold }}
            '';
          }];
          sensor = [{
            name = "WS90 Last Heavy Rain Timestamp";
            unique_id = "ws90_last_heavy_rain_timestamp";
            device_class = "timestamp";
            state = "{{ now().isoformat() }}";
          }];
        }
        {
          trigger = [{ trigger = "time_pattern"; hours = "/1"; }];
          sensor = [{
            name = "Days Since Last Rain";
            unique_id = "ws90_days_since_rain";
            unit_of_measurement = "d";
            state_class = "measurement";
            state = ''
              {% set last = as_datetime(states('sensor.ws90_last_rain_timestamp'), none) %}
              {{ (now() - last).days if last is not none else none }}
            '';
          }];
        }
        {
          trigger = [{ trigger = "time_pattern"; hours = "/1"; }];
          sensor = [{
            name = "Days Since Last Heavy Rain";
            unique_id = "ws90_days_since_heavy_rain";
            unit_of_measurement = "d";
            state_class = "measurement";
            state = ''
              {% set last = as_datetime(states('sensor.ws90_last_rain_timestamp'), none) %}
              {{ (now() - last).days if last is not none else none }}
            '';
          }];
        }
        {
          trigger = [
            { trigger = "time"; at = "09:00:00"; id = "reset"; }
            { trigger = "state"; entity_id = ["sensor.ws90_rain_total"]; id = "update"; }
          ];
          sensor = [{
            name = "WS90 Rain Since 9am";
            unique_id = "ws90_rain_since_9am";
            unit_of_measurement = "mm";
            state_class = "measurement";
            state = ''
              {% set total = states('sensor.ws90_rain_total') %}
              {% if trigger.id == 'reset' %}
                0.0
              {% elif is_number(total) %}
                {% set baseline = this.attributes.get('baseline', total | float) | float %}
                {{ ([total | float - baseline, 0] | max) | round(1) }}
              {% else %}
                {{ this.state }}
              {% endif %}
            '';
            attributes = {
              baseline = ''
                {% set total = states('sensor.ws90_rain_total') %}
                {% if trigger.id == 'reset' and is_number(total) %}
                  {{ total | float }}
                {% else %}
                  {{ this.attributes.get('baseline', total | float(0)) | float }}
                {% endif %}
              '';
            };
          }];
        }
      ];

        sensor = [
          {
            platform = "derivative";
            name = "Master Bathroom Humidity Rate";
            source = "sensor.timmerflotte_temp_hmd_sensor_humidity_2";
            time_window = "00:05:00";
            unit_time = "min";
          }
          {
            platform = "derivative";
            name = "Main Bedroom Bathroom Humidity Rate";
            source = "sensor.timmerflotte_temp_hmd_sensor_humidity";
            time_window = "00:05:00";
            unit_time = "min";
          }
        ];
        automation = "!include automations.yaml";
        logger = {
          default = "warning";
          logs = {
            "homeassistant.components.automation" = "info";
            "homeassistant.config" = "info";
            "homeassistant.core" = "info";
            "homeassistant.helpers.entity_platform" = "info";
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
