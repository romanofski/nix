{ pkgs, ... }:

{
  home.packages = [
    pkgs.xmobar
  ];

  xdg = {
    enable = true;
    configFile = {
      xmobarrc = {
        target = ".xmobarrc";
        text = builtins.readFile ./configs/xmobarrc;
      };
    };
  };
}
