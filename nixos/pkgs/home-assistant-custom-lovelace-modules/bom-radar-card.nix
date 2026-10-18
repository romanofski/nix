{ lib, stdenvNoCC, fetchurl }:

stdenvNoCC.mkDerivation rec {
  pname = "bom-radar-card";

  # https://github.com/AshtonAU/bom-radar-card
  version = "1.15.0";

  src = fetchurl {
    url = "https://github.com/AshtonAU/bom-radar-card/releases/download/v${version}/${pname}.js";
    hash = "sha256-ndxhukTr7vXS8gvqb4KcKulodO1ugpuZv46TSqqtXHg=";
  };

  dontUnpack = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall
    install -Dm644 "$src" "$out/${pname}.js"
    runHook postInstall
  '';

  meta = with lib; {
    description = "Australian Bureau of Meteorology rain radar card for Home Assistant using native BOM WMTS tiles";
    homepage = "https://github.com/AshtonAU/bom-radar-card";
    license = licenses.mit;
    platforms = platforms.all;
  };
}

