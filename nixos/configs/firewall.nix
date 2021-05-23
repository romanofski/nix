{ config, pkgs, ... }:

{
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 22 ];
    allowedUDPPortRanges = [ { from = 60000; to = 61000; } ];
  };
}
