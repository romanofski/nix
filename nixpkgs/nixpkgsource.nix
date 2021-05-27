{
  nixos-unstable = builtins.fetchTarball {
    # current nixos-unstable HEAD as of 15/08/2020
    url = "https://github.com/NixOS/nixpkgs/archive/ea7d4aa9b8225abd6147339f0d56675d6f1f0fd1.tar.gz";
    sha256 = "11bwgyglag40w2y50nq2gg4697ymdjprkgip75kb3hjrvvwpma53";
  };
}
