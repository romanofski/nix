{ config, pkgs, ... }:
{
  services.upower = {
    enable = true;
  };

  powerManagement = {
    enable = true;
    powertop.enable = true;
  };
}
