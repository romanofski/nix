{ stdenv, ghcWithPackages, makeWrapper, packages ? (pkgSet: []) }:

let
  gtfsscheduleEnv = ghcWithPackages (self: [ self.gtfsschedule ] ++ packages self);
in stdenv.mkDerivation {
  name = "gtfsschedule-with-packages-${gtfsscheduleEnv.version}";

  nativeBuildInputs = [ makeWrapper ];

  buildCommand = ''
    mkdir -p $out/bin $out/share
    makeWrapper ${gtfsscheduleEnv}/bin/gtfsschedule $out/bin/gtfsschedule \
      --set NIX_GHC "${gtfsscheduleEnv}/bin/ghc"
  '';

  # trivial derivation
  preferLocalBuild = true;
  allowSubstitutes = false;
}
