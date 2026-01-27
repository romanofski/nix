{ config, pkgs, ... }:
{
  services.calibre-server = {
    enable = true;
    openFirewall = true;
    libraries = [
      "/srv/Books"
    ];
  };
}
