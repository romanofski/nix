
{ lib, stdenvNoCC, fetchurl }:

stdenvNoCC.mkDerivation rec {
  pname = "lovelace-windrose-card";

  # https://github.com/aukedejong/lovelace-windrose-card
  version = "2.7.0";

  src = fetchurl {
    url = "https://github.com/aukedejong/lovelace-windrose-card/releases/download/v${version}/windrose-card.js";
    hash = "sha256-zpB+a2Xv1cFWeO0iF/ECKX4ldZnrVGMcQbworEzs41o=";
  };

  dontUnpack = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall
    install -Dm644 "$src" "$out/${pname}.js"
    runHook postInstall
  '';

  meta = with lib; {
    description = "A Home Assistant Lovelace custom card to show wind speed and direction data in a Windrose diagram.";
    homepage = "https://github.com/aukedejong/lovelace-windrose-card";
    license = licenses.mit;
    platforms = platforms.all;
  };
}

