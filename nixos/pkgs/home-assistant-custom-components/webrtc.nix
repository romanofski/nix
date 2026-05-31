{ lib, buildHomeAssistantComponent, fetchFromGitHub }:

buildHomeAssistantComponent rec {
  owner = "AlexxIT";
  domain = "webrtc";
  version = "3.6.1";  # check https://github.com/AlexxIT/WebRTC/releases

  src = fetchFromGitHub {
    owner = "AlexxIT";
    repo = "WebRTC";
    rev = "v${version}";
    hash = "sha256-/Rw95G7Ro0QvKZ7SNMIA/Q8Kr56QQqxos+t1xksuDJ0=";
  };

  meta = with lib; {
    description = "WebRTC Camera custom component for Home Assistant";
    homepage = "https://github.com/AlexxIT/WebRTC";
    license = licenses.mit;
  };
}
