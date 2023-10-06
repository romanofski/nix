{ pkgs, ... }:

{
  home.packages = [
    pkgs.termonad
    pkgs.victor-mono
  ];
  xdg = {
    enable = true;
    configFile."termonad/termonad.hs".source = ./configs/termonad.hs;
  };
}
