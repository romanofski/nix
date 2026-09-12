{ lib, buildHomeAssistantComponent, fetchFromGitHub, home-assistant }:

buildHomeAssistantComponent rec {
  owner = "TypQxQ";
  domain = "sigen";
  version = "1.2.7.3";

  src = fetchFromGitHub {
    owner = owner;
    repo = "Sigenergy-Local-Modbus";
    rev = "v.${version}";
    hash = "sha256-fsCZ23+I0v3SPVGGd/6ONEPCiTZnPWbZSfA6MmiSWEI=";
  };

  dependencies = [
home-assistant.python3Packages.pymodbus
  ];

  meta = with lib; {
    description = "Sigenergy ESS Integration for Home Assistant";
    homepage = "https://github.com/${owner}/${domain}";
    license = licenses.mit;
  };
}
