{ mkDerivation, base, bifunctors, bytestring, cassava, conduit
, conduit-extra, containers, directory, esqueleto, fetchgit
, HStringTemplate, http-client, http-conduit, http-types, ini, lens
, lib, lifted-base, monad-control, monad-logger, mtl, network
, old-locale, optparse-applicative, persistent, persistent-sqlite
, persistent-template, protocol-buffers, QuickCheck, resourcet
, silently, streaming-commons, system-filepath, tasty, tasty-hunit
, tasty-quickcheck, temporary, text, time, transformers
, transformers-base, unliftio-core, utf8-string, xdg-basedir
, zip-archive
}:
mkDerivation {
  pname = "gtfsschedule";
  version = "0.8.3.0";
  src = fetchgit {
    url = "https://github.com/romanofski/gtfsschedule.git";
    sha256 = "1fla28475sfxhbfsqvzmqwi7c7njlzah1l7hya2kgbsxvskv63pf";
    rev = "b65ffde20019130813d17d7f796d29de0434c0a1";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base bifunctors bytestring cassava conduit conduit-extra containers
    directory esqueleto HStringTemplate http-client http-conduit
    http-types ini lens monad-control monad-logger mtl old-locale
    optparse-applicative persistent persistent-sqlite
    persistent-template protocol-buffers resourcet system-filepath
    temporary text time transformers unliftio-core utf8-string
    xdg-basedir zip-archive
  ];
  executableHaskellDepends = [
    base bytestring http-conduit ini mtl optparse-applicative
    protocol-buffers text
  ];
  testHaskellDepends = [
    base bytestring conduit conduit-extra containers directory
    lifted-base monad-logger network persistent persistent-sqlite
    protocol-buffers QuickCheck resourcet silently streaming-commons
    tasty tasty-hunit tasty-quickcheck temporary text time transformers
    transformers-base
  ];
  homepage = "http://github.com/romanofski/gtfsschedule#readme";
  description = "Be on time for your next public transport service";
  license = lib.licenses.gpl3Only;
}
