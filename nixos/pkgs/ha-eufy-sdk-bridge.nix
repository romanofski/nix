{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
  nodejs_24,
  pkg-config,
  systemd,
  makeWrapper,
  ffmpeg,
  go2rtc,
  eufy-sdk,
}:

buildNpmPackage rec {
  pname = "ha-eufy-sdk-bridge";
  version = "0.4.0";

  src = fetchFromGitHub {
    owner = "mega-yfue";
    repo = "ha-eufy-sdk-bridge";
    tag = "${version}";
    hash = "sha256-9gTDuZdfjCsi1nsiN3IH6+7mB/S2EevTb1Eb+Ef4p9Q=";
  };

  npmDepsHash = "sha256-ISq/WxMHp7E0nVGYvULtNvg6oCb9WwTpfsFEAogZO90=";
  npmDepsFetcherVersion = 2;

  nodejs = nodejs_24;

  nativeBuildInputs = [
    pkg-config
    makeWrapper
  ];

  buildInputs = [
    systemd
  ];

  # Required by native Node modules such as usb/node-addon-api.
  env.CXXFLAGS = "-std=c++17";

  # The Docker image does not run npm build. The checked-in .mjs files are
  # executed directly.
  dontNpmBuild = true;
  dontNpmCheck = true;

  # Replace the SDK downloaded through npm with the separately packaged SDK.
  #
  # buildNpmPackage installs scoped packages under:
  #   $out/lib/node_modules/@scope/package
  preBuild = ''
    sdkPath="${eufy-sdk}/lib/node_modules/@mega-yfue/eufy-sdk"

    if [ ! -d "$sdkPath" ]; then
    echo "Packaged eufy SDK was not found at: $sdkPath" >&2
    echo "SDK contents:" >&2
    find ${eufy-sdk} -maxdepth 4 -type d >&2
    exit 1
    fi

    rm -rf node_modules/@mega-yfue/eufy-sdk
    mkdir -p node_modules/@mega-yfue
    ln -s "$sdkPath" node_modules/@mega-yfue/eufy-sdk

    patchShebangs bin/start.sh
  '';

  installPhase = ''
    runHook preInstall

    appDir="$out/lib/${pname}"

    mkdir -p "$appDir" "$out/bin"

    cp package.json package-lock.json "$appDir/"
    cp server.mjs streams.mjs go2rtc-config.mjs "$appDir/"
    cp -r src bin node_modules "$appDir/"

    chmod +x "$appDir/bin/start.sh"

    makeWrapper "$appDir/bin/start.sh" "$out/bin/eufy-sdk-bridge" \
    --set BRIDGE_APP_DIR "$appDir" \
    --prefix PATH : ${
      lib.makeBinPath [
        nodejs_24
        ffmpeg
        go2rtc
      ]
    }

    runHook postInstall
  '';

  postInstall = ''
    # npm sometimes leaves broken development-only links in .bin.
    find "$out/lib/${pname}/node_modules/.bin" \
    -xtype l -delete 2>/dev/null || true
  '';

  meta = {
    description = "Home Assistant bridge for the mega-yfue Eufy SDK";
    homepage = "https://github.com/mega-yfue/ha-eufy-sdk-bridge";
    license = lib.licenses.asl20;
    platforms = lib.platforms.linux;
    mainProgram = "eufy-sdk-bridge";
  };
}
