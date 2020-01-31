{ pkgs, ... }:

{
  xdg = {
    enable = true;
    configFile = [
      {
        target = "termonad/termonad.hs";
        text = builtins.readFile ./configs/termonad.hs;
      }
    ];
  };
}
