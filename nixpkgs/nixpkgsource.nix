{
  nixos-unstable = builtins.fetchTarball {
    # current nixos-unstable HEAD as of 13/11/2021
    url = "https://github.com/NixOS/nixpkgs/archive/0bffda19b8af722f8069d09d8b6a24594c80b352.tar.gz";
    sha256 = "sha256:1nm5jwjnfkx1vc3m63w2dh0qxsiin07p795mm607zdyk7vpgx5ib";
  };
}
