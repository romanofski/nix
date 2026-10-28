{ config, pkgs, ... }:

{
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [
      22
    ];
    # kde connect
    allowedTCPPortRanges = [ { from = 1714; to = 1764; }];
    allowedUDPPortRanges = [ { from = 1714; to = 1764; }]; # KDE Connect
  };
}
