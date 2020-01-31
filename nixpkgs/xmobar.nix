{ pkgs, ... }:

{
  xdg = {
    enable = true;
    configFile = [
      {
        target = ".xmobarrc";
        text = builtins.readFile ./configs/xmobarrc;
      }
    ];
  };
}
