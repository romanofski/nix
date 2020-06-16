{ pkgs, ... }:

{
  xdg = {
    enable = true;
    configFile."termonad/termonad.hs".source = ./configs/termonad.hs;
  };
}
