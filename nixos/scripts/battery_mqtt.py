"""Publish Lenovo Yoga BAT0 sysfs battery stats to MQTT for Home Assistant."""

import re
import sys
import paho.mqtt.publish as publish

MQTT_HOST = "localhost"
MQTT_PORT = 1883
TOPIC_PREFIX = "home/battery/bat0"

BATTERY_PATH = "/sys/class/power_supply/BAT0"


def safe_read(path: str) -> str:
    try:
        with open(path, "r") as f:
            return f.read().strip()
    except (FileNotFoundError, PermissionError):
        return "unknown"


def parse_charge_type(raw: str) -> str:
    match = re.search(r"\[([^\]]+)\]", raw)
    return match.group(1) if match else "unknown"


def main() -> int:
    capacity = safe_read(f"{BATTERY_PATH}/capacity")
    status = safe_read(f"{BATTERY_PATH}/status")
    charge_type_raw = safe_read(f"{BATTERY_PATH}/charge_types")
    charge_type = parse_charge_type(charge_type_raw) if charge_type_raw != "unknown" else "unknown"
    cycle_count = safe_read(f"{BATTERY_PATH}/cycle_count")

    messages = [
        (f"{TOPIC_PREFIX}/capacity", capacity, 0, False),
        (f"{TOPIC_PREFIX}/status", status, 0, False),
        (f"{TOPIC_PREFIX}/charge_type", charge_type, 0, False),
        (f"{TOPIC_PREFIX}/cycle_count", cycle_count, 0, False),
    ]

    try:
        publish.multiple(messages, hostname=MQTT_HOST, port=MQTT_PORT)
    except Exception as e:
        print(f"MQTT publish failed: {e}", file=sys.stderr)
        return 1

    return 0


if __name__ == "__main__":
    sys.exit(main())
