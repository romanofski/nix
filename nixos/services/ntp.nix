{ config, pkgs, ... }:
{
  # sync clock with ntp
    services.chrony = {
      enable = true;
    };
}
