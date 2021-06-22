{ pkgs, ... }:

{
  xresources.properties = {
    "Xft.antialias" = 1;
    "Xft.autohint" = 0;
    "Xft.hinting" = 1;
    "Xft.hintstyle" = "hintfull";
    "Xft.lcdfilter" = "lcddefault";
    "Xft.rgba" = "rgb";
    "Xcursor.theme" = "core";
    "Xautolock.time" = 20;
    "Xautolock.locker" = "xlock";
    "Xautolock.corners" = "+0-0";
    "Xautolock.cornerdelay" = 3;

    "XLock.foreground" = "White";
    "XLock.background" = "Gray20";
    "XLock.echokeys" = 1;
    "XLock.usefirst" = 1;
    "XLock.echokey" = "*";
    # 10 minutes
    "XLock.dpmsoff" = "600";

    "Xft.dpi" = 100;
  };
  xsession.enable = true;

  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = ./configs/xmonad.hs;
  };

  home.file.".xmonad/startup-hook" = {
    source = ./configs/startup-hook.sh;
    executable = true;
  };
  home.file.".xmobarrc".source = ./configs/xmobarrc;
}

