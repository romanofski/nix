{ pkgs, ... }:

{
  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = ./configs/xmonad.hs;
    extraPackages = haskellPackages: [
      haskellPackages.xmobar
    ];
  };
}
