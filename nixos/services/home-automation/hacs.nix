{ stdenv, fetchzip }:

stdenv.mkDerivation rec {
  pname = "hacs";
  version = "2.0.5";

  src = fetchzip {
    url = "https://github.com/hacs/integration/releases/download/${version}/hacs.zip";
    sha256 = "";
    stripRoot = false;
  };

  dontBuild = true;

  installPhase = ''
    mkdir -p $out/custom_components/hacs
    cp -r ./* $out/custom_components/hacs/
  '';

  meta = {
    description = "Home Assistant Community Store";
    homepage = "https://hacs.xyz/";
  };
}
