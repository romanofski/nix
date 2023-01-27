{
  nixos-unstable = builtins.fetchTarball {
    # current nixos-unstable HEAD as of 13/11/2021
    url = "https://github.com/NixOS/nixpkgs/archive/9b97ad7b4330aacda9b2343396eb3df8a853b4fc.tar.gz";
    sha256 = "sha256:0nci00az2v3g4rqra0sg1f006lw77xvq2fckkwmgw12rlpv3bym9";
  };
}
