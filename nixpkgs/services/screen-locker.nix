{pkgs, ...}:

{
  services.screen-locker = {
    enable = false;
    lockCmd = "xlock";
  };
}
