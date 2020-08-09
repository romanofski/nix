{ pkgs, ... }:

{
  home.packages = [
    pkgs.termonad-with-packages
  ];
  xdg = {
    enable = true;
    configFile."termonad/termonad.hs".source = ./configs/termonad.hs;
  };
}
