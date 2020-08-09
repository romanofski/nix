{pkgs, ...}:

{
  xdg = {
    enable = true;
    configFile."purebred/purebred.hs".source = ./configs/purebred.hs;
  };
}

