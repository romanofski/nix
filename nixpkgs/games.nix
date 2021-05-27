{ pkgs, ... }:

let
  sources = import ./nixpkgsource.nix;
  pkgsUnstable = import sources.nixos-unstable { };
in {
  home.packages = [
    pkgs.endless-sky
    pkgs.minetest
    pkgs.dwarf-fortress-packages.dwarf-fortress-full
    pkgs.openttd
    pkgs.wine
    pkgsUnstable.openra
  ];
}
