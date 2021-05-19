{pkgs, ...}:

{
  services.screen-locker = {
    enable = true;
    lockCmd = "${pkgs.xlockmore}/bin/xlock";
  };
}
