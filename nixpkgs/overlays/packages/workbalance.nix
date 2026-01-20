{ mkDerivation, fetchFromGitHub, attoparsec, base, shelly, lib, text, time }:
mkDerivation {
  pname = "workbalance";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "romanofski";
    repo = "workbalance";
    rev = "master";
    sha256 = "sha256-5SB+GN1Zj7YtO0+RBsBez9DDgqxLaL/PqRSIshug5x4=";
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ attoparsec base shelly text time ];
  description = "Shows a work balance";
  license = lib.licenses.gpl3;
}
