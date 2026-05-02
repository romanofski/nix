{ lib, buildNpmPackage, fetchFromGitHub, nodejs_22, pkg-config, systemd,
makeWrapper }:

buildNpmPackage rec {
  pname = "matterjs-server";
  version = "0.6.4";

  src = fetchFromGitHub {
    owner = "matter-js";
    repo = "matterjs-server";
    tag = "v${version}";
    hash = "sha256-iHTc5PDlg4KvOY+oY9GU2l/pPNMrnmIyFqxjCL7w0kw=";
  };

  npmDepsHash = "sha256-Qveo8b92Y5y2AZR8wCiFbCCRyydandnRJghrHoWt464=";

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
