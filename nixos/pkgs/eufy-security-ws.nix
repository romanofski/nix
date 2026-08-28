{ lib, buildNpmPackage, fetchFromGitHub, nodejs_24 }:

buildNpmPackage rec {
  pname = "eufy-security-ws";
  version = "3.1.0";  # check https://github.com/bropat/eufy-security-ws/releases

  src = fetchFromGitHub {
    owner = "bropat";
    repo  = "eufy-security-ws";
    rev   = "${version}";
    hash = "sha256-xHsq497V0aOpEulAJBeZ+05cH0FhJPAein04TY0DT2o=";
  };

  npmDepsFetcherVersion = 2;
  npmDepsHash = "sha256-RDcegOYYlL8r2QC/TL0UyDEtIDBnjRvpylkLVLGmoSA=";
  makeCacheWritable = true;
  npmFlags = [ "--legacy-peer-deps" ];

  nodejs = nodejs_24;

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

