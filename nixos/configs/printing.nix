{ config, pkgs, ... }:

{
  # Enable CUPS to print documents.
  services.printing = {
    enable = true;
    browsing = true;
    drivers = [ pkgs.brlaser pkgs.foomatic_filters ];
  };
}
