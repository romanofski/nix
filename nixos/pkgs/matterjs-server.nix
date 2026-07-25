{ lib, buildNpmPackage, fetchFromGitHub, nodejs_22, pkg-config, systemd,
makeWrapper }:

buildNpmPackage rec {
  pname = "matterjs-server";
  version = "1.3.1";

  src = fetchFromGitHub {
    owner = "matter-js";
    repo = "matterjs-server";
    tag = "v${version}";
    hash = "sha256-HY1VEjqQ2C6Ym9PB2/g7M7TeAgOFsVYMX6jvpZLPgOI=";
  };

  npmDepsHash = "sha256-I/iY2YdsgvdLMkLRzML5VttmmDNUr/U38l+Wp8bu2B8=";

  nodejs = nodejs_22;

  nativeBuildInputs = [ pkg-config makeWrapper ];
  buildInputs = [ systemd ];

  # node-addon-api (pulled in by 'usb' native module) requires C++17;
  # node-gyp otherwise builds it with an older standard ->
  # std::string_view erros
  env.CXXFLAGS = "-std=c++17";
  npmBuildScript = "build";

  preBuild = ''
    patchShebangs $(readlink -f node_modules/.bin/matter-build)
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out/lib $out/bin
    cp -r node_modules $out/lib/
    cp -r packages $out/lib/
    makeWrapper ${nodejs_22}/bin/node $out/bin/matter-server \
      --add-flags "--enable-source-maps" \
      --add-flags "$out/lib/packages/matter-server/dist/esm/MatterServer.js"
    runHook postInstall
  '';

  postInstall = ''
    find $out/lib/node_modules/.bin -xtype l -delete
  '';

  meta = {
    description = "Matter server based on Matter.js";
    homepage = "https://github.com/matter-js/matterjs-server";
    license = lib.licenses.asl20;
    mainProgram = "matter-server";
  };
}
