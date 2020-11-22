{ mkDerivation, fetchFromGitHub, attoparsec, base, shelly, stdenv, text, time }:
mkDerivation {
  pname = "workbalance";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "romanofski";
    repo = "workbalance";
    rev = "master";
    sha256 = "1ls47msar9bwbn7ka5p8dkf1ql5zk371v9za6xkk2vjq83pvxdd4";
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ attoparsec base shelly text time ];
  description = "Shows a work balance";
  license = stdenv.lib.licenses.gpl3;
}
