{ lib, buildHomeAssistantComponent, fetchPypi, fetchFromGitHub, home-assistant }:

buildHomeAssistantComponent rec {
  owner = "mega-yfue";
  domain = "eufy_sdk";
  version = "0.4.0";

  src = fetchFromGitHub {
    owner = "mega-yfue";
    repo = "ha-eufy-sdk";
    rev = "${version}";
    hash = "sha256-kCrjvlf45kjgmxZpOmpuGs1y/1ly0OFLJuvX4pqBUPg=";
  };

  dependencies = [
  ];

  meta = with lib; {
    description = "HA integration for Eufy";
    homepage = "https://github.com/mega-yfue/ha-eufy-sdk";
    license = licenses.mit;
  };
}
