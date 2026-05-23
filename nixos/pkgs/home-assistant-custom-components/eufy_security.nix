{ lib, buildHomeAssistantComponent, fetchFromGitHub, python3Packages }:

buildHomeAssistantComponent rec {
  owner = "fuatakgun";
  domain = "eufy_security";
  version = "8.2.4";

  src = fetchFromGitHub {
    owner = "fuatakgun";
    repo = "eufy_security";
    rev = "v${version}";
    hash = "sha256-Pn+ci4h016EX7dbJ7eh1lF0L0wkzw3g/AEWKN0QZpww=";
  };

  dependencies = with python3Packages; [
    websocket-client
    aiortsp
  ];

  meta = with lib; {
    description = "Eufy Security integration for Home Assistant";
    homepage = "https://github.com/fuatakgun/eufy_security";
    license = licenses.mit;
  };
}
