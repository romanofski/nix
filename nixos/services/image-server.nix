
{ config, pkgs, ... }:
{
  services.immich = {
    enable = true;
    openFirewall = true;
    host = "0.0.0.0";
  };
}
