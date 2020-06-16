{ pkgs, ... }:

{
  xdg = {
    enable = true;
    configFile."gtfs/config.cfg".source = ./configs/gtfsschedule.cfg;
  };
}
