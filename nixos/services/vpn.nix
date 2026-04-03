{ config, pkgs, ... }:
{
  services.tailscale = {
    enable = true;
    permitCertUid = "caddy";
  };
}
