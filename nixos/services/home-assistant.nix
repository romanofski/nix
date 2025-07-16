{ config, pkgs, ... }:
{
  services.home-assistant = {
    enable = true;
    config = {
      name = "Home";
      latitude = "-27.559160278504198";
      longitude = "152.87379130057226";
      elevation = "34";
      unit_system = "metric";
      time_zone = "Australia/Brisbane";
    };
  };
}
