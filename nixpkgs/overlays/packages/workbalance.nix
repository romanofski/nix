{ mkDerivation, fetchFromGitHub, attoparsec, base, shelly, lib, text, time }:
mkDerivation {
  pname = "workbalance";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "romanofski";
    repo = "workbalance";
    rev = "master";
    sha256 = "0njygkvx1203vjqzr1sb42m07kbg2fbj7a28f6m9r96gign03bn2";
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ attoparsec base shelly text time ];
  description = "Shows a work balance";
  license = lib.licenses.gpl3;
}
