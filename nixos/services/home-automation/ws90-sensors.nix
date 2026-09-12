{ lib, ... }:
let
  ws90Id = 90741;
  ws90Topic = "rtl_433/Fineoffset-WS90/${toString ws90Id}";

  mkSensor =
    { name
    , valueKey
    , unit ? null
    , deviceClass ? null
    , stateClass ? "measurement"
    , icon ? null
    , value_template ? "{{ value_json.${valueKey} }}"
    }:
    lib.filterAttrs (_: v: v != null) {
      inherit name;
      unique_id = "ws90_${toString ws90Id}_${valueKey}";
      state_topic = ws90Topic;
      value_template = value_template;
      unit_of_measurement = unit;
      device_class = deviceClass;
      state_class = stateClass;
      icon = icon;
      json_attributes_topic = ws90Topic;
      expire_after = 900;
    };
in
[
  (mkSensor { name = "WS90 Temperature"; valueKey = "temperature_C"; unit = "°C"; deviceClass = "temperature"; })
  (mkSensor { name = "WS90 Humidity"; valueKey = "humidity"; unit = "%"; deviceClass = "humidity"; })
  (mkSensor {
    name = "WS90 Wind Direction";
    valueKey = "wind_dir_deg";
    icon = "mdi:compass-outline";
    stateClass = null;
    value_template = ''
      {% set deg = value_json.wind_dir_deg | float %}
      {% set directions = ['N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW'] %}
      {{ directions[(((deg + 11.25) % 360) / 22.5) | int] }}
    '';})
  (mkSensor { name = "WS90 Wind Speed"; valueKey = "wind_avg_m_s"; unit = "m/s"; deviceClass = "wind_speed"; })
  (mkSensor { name = "WS90 Wind Gust"; valueKey = "wind_max_m_s"; unit = "m/s"; deviceClass = "wind_speed"; })
  (mkSensor { name = "WS90 UV Index"; valueKey = "uvi"; icon = "mdi:weather-sunny-alert"; })
  (mkSensor { name = "WS90 Illuminance"; valueKey = "light_lux"; unit = "lx"; deviceClass = "illuminance"; })
  (mkSensor { name = "WS90 Rain Total"; valueKey = "rain_mm"; unit = "mm"; deviceClass = "precipitation"; stateClass = "total_increasing"; })
  (mkSensor { name = "WS90 Supercap Voltage"; valueKey = "supercap_V"; unit = "V"; deviceClass = "voltage"; })
  (mkSensor { name = "WS90 Battery Voltage"; valueKey = "battery_mV"; unit = "mV"; deviceClass = "voltage"; })
  (mkSensor { name = "WS90 RSSI"; valueKey = "rssi"; unit = "dB"; deviceClass = "signal_strength"; stateClass = null; })
  (mkSensor { name = "WS90 SNR"; valueKey = "snr"; unit = "dB"; stateClass = null; icon = "mdi:signal"; })
]
