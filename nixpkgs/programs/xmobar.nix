{ pkgs, ... }:

{
  home.packages = [
    pkgs.xmobar
    pkgs.haskellPackages.workbalance
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
