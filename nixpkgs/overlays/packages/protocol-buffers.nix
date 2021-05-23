{ mkDerivation, aeson, array, base, base16-bytestring, binary
, bytestring, containers, directory, fetchgit, filepath, mtl
, parsec, stdenv, syb, text, utf8-string, vector
}:
mkDerivation {
  pname = "protocol-buffers";
  version = "2.4.17";
  src = fetchgit {
    url = "https://github.com/k-bx/protocol-buffers.git";
    sha256 = "0lv3kdrx4rry6q1yvmqfyl8yjq2dfjz7sa4qi41kxj01gr1hx14b";
    rev = "01244442fc1667e0108410218f5d6d431139153f";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    aeson array base base16-bytestring binary bytestring containers
    directory filepath mtl parsec syb text utf8-string vector
  ];
  homepage = "https://github.com/k-bx/protocol-buffers";
  description = "Parse Google Protocol Buffer specifications";
  license = stdenv.lib.licenses.bsd3;
}
