{
  nixos-unstable = builtins.fetchTarball {
    # current nixos-unstable HEAD as of 13/11/2021
    url = "https://github.com/NixOS/nixpkgs/archive/2fa862644fc15ecb525eb8cd0a60276f1c340c7c.tar.gz";
    sha256 = "00l884zydbrww2jxjvf62sm1y96jvys22jg9vb3fsznz2mbz41jb";
  };
}
