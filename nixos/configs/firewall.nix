{ config, pkgs, ... }:

{
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [
      22
      3000 # korrosync
    ];
    # kde connect
    allowedTCPPortRanges = [ { from = 1714; to = 1764; }];
    allowedUDPPortRanges = [ { from = 60000; to = 61000; } { from = 1714; to = 1764; } ];
  };
}
