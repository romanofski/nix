{ lib, buildNpmPackage, fetchFromGitHub, nodejs_24 }:

buildNpmPackage rec {
  pname = "eufy-sdk";
  version = "0.4.0";  # check https://github.com/mega-yfue/eufy-sdk

  src = fetchFromGitHub {
    owner = "mega-yfue";
    repo  = "eufy-sdk";
    rev   = "v${version}";
    hash = "sha256-Os1Y0Nn+uJV+i99cxFa5Oj0f7nL9hLTpt8pC6uuXs8g=";
  };

  npmDepsFetcherVersion = 2;
  npmDepsHash = "sha256-sn2kTknwP8a4Zw20qSwgktX6FkXE+K+A8ay7tPjwA48=";
  makeCacheWritable = true;

  nodejs = nodejs_24;

  # The package's "build" script compiles TypeScript
  npmBuildScript = "build";

  # No tests at install time
  dontNpmCheck = true;

  meta = with lib; {
    description = "One typed client for the whole Anker eufy ecosystem — devices, realtime events, and live media.";
    homepage = "https://github.com/mega-yfue/eufy-sdk";
    license = licenses.asl20;
    platforms = platforms.linux;
  };
}

