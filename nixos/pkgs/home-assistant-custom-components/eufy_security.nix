{ lib, buildHomeAssistantComponent, fetchPypi, fetchFromGitHub, home-assistant }:

let
  py = home-assistant.python3Packages;
  websocket-client = py.websocket-client.overridePythonAttrs (old: rec {
    version = "1.8.0";
    src = fetchPypi {
      pname = "websocket_client";
      inherit version;
      hash = "sha256-Mjnfn0TaYy+WASRygF1AojKBqZECfOEdL0Wm8krEw9o=";
    };
  });
in
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

  dependencies = [
    websocket-client
    py.aiortsp
  ];

  meta = with lib; {
    description = "Eufy Security integration for Home Assistant";
    homepage = "https://github.com/fuatakgun/eufy_security";
    license = licenses.mit;
  };
}
