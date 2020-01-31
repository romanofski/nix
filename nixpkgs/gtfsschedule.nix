{ pkgs, ... }:

let
  nixpkgs = import <nixos> { };
  pkgsUnstable = import <unstable> { };
in {
  xdg = {
    enable = true;
    configFile = [
      {
        target = "gtfs/config.cfg";
        text = builtins.readFile ./configs/gtfsschedule.cfg;
      }
    ];
  };
}
