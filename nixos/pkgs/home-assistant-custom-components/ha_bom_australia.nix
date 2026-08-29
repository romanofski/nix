{ lib, buildHomeAssistantComponent, fetchFromGitHub, home-assistant }:

buildHomeAssistantComponent rec {
  owner = "safepay";
  domain = "ha_bom_australia";
  version = "1.6.6";

  src = fetchFromGitHub {
    owner = owner;
    repo = domain;
    rev = "v${version}";
    hash = "sha256-kG0ugDnQvUVavrMFaMXOCPnjzihEdjLcCalXP08gY4M=";
  };

  dependencies = [
  ];

  meta = with lib; {
    description = "HA BOM Australia custom component";
    homepage = "https://github.com/${owner}/${domain}";
    license = licenses.mit;
  };
}
