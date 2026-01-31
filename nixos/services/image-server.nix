
{ config, pkgs, ... }:
{
  services.immich = {
    enable = true;
    openFirewall = true;
    host = "0.0.0.0";
    accelerationDevices = ["/dev/dri/renderD128"];
    mediaLocation = "/srv/images";
    redis.enable = true;
    database = {
      enable = true;
      createDB = true;
    };
  };
}
