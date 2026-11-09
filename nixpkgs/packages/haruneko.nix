{ lib
, stdenv
, fetchFromGitHub
, buildNpmPackage
, electron_44
, makeWrapper
, makeDesktopItem
, copyDesktopItems
, nodejs_24
, python3
, ...
}:

let
  electron = electron_44;

  desktopItem = makeDesktopItem {
    name = "hakuneko";
    exec = "hakuneko";
    icon = "hakuneko";
    desktopName = "HakuNeko";
    genericName = "Manga & Anime Downloader";
    categories = [ "Network" "FileTransfer" ];
  };

  buildNpmPackage' = buildNpmPackage.override { nodejs = nodejs_24; };

  websocketRpcSrc = fetchFromGitHub {
    owner = "manga-download";
    repo = "websocket-rpc";
    tag = "v1.2.7";
    hash = "sha256-TZCmDN48IK7VpIunTyxZzWIPEaBgo2zcx9kqB7Jqt1I=";
  };
in
buildNpmPackage' rec {
  pname = "hakuneko";
  version = "unstable-2026-11-09";

  src = fetchFromGitHub {
    owner = "manga-download";
    repo = "haruneko";
    tag = "260903T1833";
    hash = "sha256-EZEncxWDJCHXVqiY1nZIqrzJOYh/4kCoTSy0a3b9Eqo=";
  };

  # Use the workspace-aware fetcher. This is the key to handling the monorepo structure.
  npmDepsFetcherVersion = 2;

  # The hash must match the final, clean lockfile.
  # If this fails, set it to lib.fakeHash and use the hash from the error.
  npmDepsHash = "sha256-kUDLIEcBoQhNfQqXkvz/JZMg0PAdU0NeSZ9rb2w7bos=";

  forceGitDeps = true;
  makeCacheWritable = true;

  # Tell the builder to focus on the Electron workspace.
  npmWorkspace = "app/electron";

  # This step ensures the lockfile is in place for the build phase
  # and neutralizes the postinstall script that would try to download Electron.
  postPatch = ''
    cp ${./package-lock.json} package-lock.json
    substituteInPlace package.json \
    --replace-fail '"postinstall": "npm --workspace=app/electron exec electron -- --no-sandbox --version"' \
    '"postinstall": ""'
    substituteInPlace app/electron/scripts/build-app.mjs \
    --replace-fail "await run('npm install --omit=dev', dirBuild);" ""
  '';

  # Prevent Electron from trying to download its binary in the sandbox.
  env.ELECTRON_SKIP_BINARY_DOWNLOAD = "1";

  nativeBuildInputs = [
    nodejs_24
    python3
    makeWrapper
    copyDesktopItems
  ];

  buildPhase = ''
    runHook preBuild

    # Build inside the source tree so `import 'vite'` resolves via ./node_modules
    cp -r ${websocketRpcSrc} .websocket-rpc-build
    chmod -R u+w .websocket-rpc-build
    (cd .websocket-rpc-build && ../node_modules/.bin/vite build)

    mkdir -p node_modules/websocket-rpc
    cp -r .websocket-rpc-build/dist node_modules/websocket-rpc/dist

    npm run build --workspace=app/electron

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin $out/share/haruneko $out/share/applications

    cp -r app/electron/build/. $out/share/haruneko/

    # Copy runtime deps plus their transitive closure.
    node ${./haruneko-copy-runtime-deps.mjs} node_modules $out/share/haruneko/node_modules commander websocket-rpc

    makeWrapper ${lib.getExe electron} $out/bin/hakuneko \
    --add-flags $out/share/haruneko \
    --set-default ELECTRON_IS_DEV 0

    cp ${desktopItem}/share/applications/hakuneko.desktop $out/share/applications/

    runHook postInstall
  '';

  meta = with lib; {
    description = "Manga, Anime & Novel Downloader";
    homepage = "https://github.com/manga-download/hakuneko";
    license = licenses.unlicense;
    platforms = electron.meta.platforms;
    mainProgram = "hakuneko";
  };
}
