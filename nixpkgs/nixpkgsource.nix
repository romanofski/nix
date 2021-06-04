{
  nixos-unstable = builtins.fetchTarball {
    # current nixos-unstable HEAD as of 04/06/2021
    url = "https://github.com/NixOS/nixpkgs/archive/1c2986bbb806c57f9470bf3231d8da7250ab9091.tar.gz";
    sha256 = "0y1275nzlmsys5rk7ivzbdc8cpjs9cbk0wz6yh3i2c57b8nbd3ym";
  };
}
