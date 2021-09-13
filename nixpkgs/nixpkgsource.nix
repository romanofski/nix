{
  nixos-unstable = builtins.fetchTarball {
    # current nixos-unstable HEAD as of 04/06/2021
    url = "https://github.com/NixOS/nixpkgs/archive/f6e9e908ccbcabb365cb5434a4a38dd8c996fc72.tar.gz";
    sha256 = "02khmh36c2qq4nf5nl8apgjhz8aszc9xva7my6jjncrfxm27aksy";
  };
}
