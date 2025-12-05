{pkgs, ...}:

{
  xdg.autostart.enable = true;
  programs.keepassxc = {
    enable = true;
  };
}
