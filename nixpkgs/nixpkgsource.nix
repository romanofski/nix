{
  nixos-unstable = builtins.fetchTarball {
    # current nixos-unstable HEAD as of 13/11/2021
    url = "https://github.com/NixOS/nixpkgs/archive/41ff747f882914c1f8c233207ce280ac9d0c867f.tar.gz";
    sha256 = "sha256:1przm11d802bdrhxwsa620af9574fiqsl44yhqfci0arf5qsadij";
  };
}
