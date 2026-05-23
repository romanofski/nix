{ lib, buildNpmPackage, fetchFromGitHub, nodejs }:

buildNpmPackage rec {
  pname = "eufy-security-ws";
  version = "2.1.0";  # check https://github.com/bropat/eufy-security-ws/releases

  src = fetchFromGitHub {
    owner = "bropat";
    repo  = "eufy-security-ws";
    rev   = "${version}";
    hash = "sha256-s+xOAAeA99Ujdc3VALnBN+69dTBqKCRFeElYeFKeZ3c=";
  };

  npmDepsHash = "sha256-Bms/lwNPvN703uGh6xI8/Qe2SJJiyq9LqllqAA7ZRR0=";

  inherit nodejs;

  # The package's "build" script compiles TypeScript
  npmBuildScript = "build";

  # No tests at install time
  dontNpmCheck = true;

  meta = with lib; {
    description = "WebSocket server wrapping eufy-security-client";
    homepage = "https://github.com/bropat/eufy-security-ws";
    license = licenses.mit;
    platforms = platforms.linux;
  };
}

