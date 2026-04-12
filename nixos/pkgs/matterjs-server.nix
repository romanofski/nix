{ lib, buildNpmPackage, fetchFromGitHub, nodejs_22, pkg-config, systemd,
makeWrapper }:

buildNpmPackage rec {
  pname = "matterjs-server";
  version = "0.5.15";

  src = fetchFromGitHub {
    owner = "matter-js";
    repo = "matterjs-server";
    tag = "v${version}";
    hash = "sha256-Lc/Vr/mMiqDdWN4D2+jeBXhC5E+A+LrV+zl1/R7Xjhk=";
  };

  npmDepsHash = "sha256-rzkd8a7WgtTz1LMOOljjh/hG31TttTiqqZBSpjpG/lE=";

  nodejs = nodejs_22;

  nativeBuildInputs = [ pkg-config makeWrapper ];
  buildInputs = [ systemd ];

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
