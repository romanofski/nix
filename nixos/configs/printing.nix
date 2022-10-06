{ config, pkgs, ... }:

{
  # Enable CUPS to print documents.
  services.printing = {
    enable = true;
    browsing = true;
    drivers = [ (pkgs.callPackage printers/hll3230cdw.nix {}) ];
  };
}
