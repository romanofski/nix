{ config, pkgs, ... }:
{
  services.jellyfin.enable = true;
  services.jellyfin.openFirewall  = true;
}
