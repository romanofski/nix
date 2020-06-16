{ pkgs, ... }:

let
  pkgsUnstable = import <unstable> { };
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
