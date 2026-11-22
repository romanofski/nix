
{ config, pkgs, pkgs-unstable, ... }:
{
  services.immich = {
    enable = true;
    openFirewall = false;
    host = "127.0.0.1";
    accelerationDevices = ["/dev/dri/renderD128"];
    mediaLocation = "/srv/images";
    redis.enable = true;
    package = pkgs-unstable.immich;
    database = {
      enable = true;
      createDB = true;
    };
  };
  systemd.services.immich-server = {
    unitConfig.RequiresMountsFor = [ "/srv/images" ];
  };
  environment.systemPackages = with pkgs; [
    immich-cli
    immich-go
  ];
}
