{ pkgs, ... }:

let
  nixpkgs = import <nixos> { };
  pkgsUnstable = import <unstable> { };
in {
  nixpkgs.config.allowUnfree = true;
  home.packages = [
    pkgs.endless-sky
    pkgs.minetest
    pkgs.dwarf-fortress-packages.dwarf-fortress-full
    pkgs.openttd
    pkgs.wine
    pkgsUnstable.openra
  ];
}
