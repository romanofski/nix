{
  nixos-unstable = builtins.fetchTarball {
    # current nixos-unstable HEAD as of 13/11/2021
    url = "https://github.com/NixOS/nixpkgs/archive/8f40f2f90b9c9032d1b824442cfbbe0dbabd0dbd.tar.gz";
    sha256 = "sha256:05mlzgkkjmg5nn9d9gdlwwsawxxgj20w5c6s7vjp69zh3r8mjf9h";
  };
}
