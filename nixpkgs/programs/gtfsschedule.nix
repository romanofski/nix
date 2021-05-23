{ pkgs, ... }:

{
  home.packages = [
    pkgs.gtfsschedule-with-packages
  ];
  xdg = {
    enable = true;
    configFile."gtfs/config.cfg".source = ./configs/gtfsschedule.cfg;
  };
}
