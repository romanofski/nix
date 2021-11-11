{ mkDerivation, fetchFromGitHub, attoparsec, base, shelly, lib, text, time }:
mkDerivation {
  pname = "workbalance";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "romanofski";
    repo = "workbalance";
    rev = "master";
    sha256 = "sha256-BseMCei/2iSFpiG0pSCEa+NiIzIqfi4x75o0451tMnY=";
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ attoparsec base shelly text time ];
  description = "Shows a work balance";
  license = lib.licenses.gpl3;
}
