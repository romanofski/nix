{ lib, stdenvNoCC, fetchurl }:

stdenvNoCC.mkDerivation rec {
  pname = "power-flow-card-plus";

  # Check https://github.com/flixlix/power-flow-card-plus/releases for the
  # current latest tag (it was v0.3.0 as of this writing) and bump this when
  # you want to update.
  version = "0.3.7";

  src = fetchurl {
    url = "https://github.com/flixlix/power-flow-card-plus/releases/download/v${version}/power-flow-card-plus.js";
    # Replace with the real hash — see step 2 below for how to get it.
    hash = "sha256-K+03HzNSkjySnMSfV/gCO+Qd9X5UkhvBhufpPyc7eK0=";
  };

  dontUnpack = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall
    install -Dm644 "$src" "$out/${pname}.js"
    runHook postInstall
  '';

  meta = with lib; {
    description = "A power distribution card inspired by the official Energy Distribution card for Home Assistant";
    homepage = "https://github.com/flixlix/power-flow-card-plus";
    license = licenses.mit; # double-check the repo's actual LICENSE file
    platforms = platforms.all;
  };
}

