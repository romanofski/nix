{ pkgs, ... }:

let
  pkgsUnstable = import <unstable> { };
in {
  home.packages = [
    pkgs.endless-sky
    pkgs.minetest
    pkgs.openttd
    pkgs.wine
    pkgsUnstable.openra
  ];
}
