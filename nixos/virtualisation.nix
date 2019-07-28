{ config, pkgs, ... }:

{
  virtualisation = {
    docker = {
      enable = true;
      storageDriver = "overlay2";
      autoPrune = {
        enable = true;
      };
    };
  };
  environment.systemPackages = with pkgs; [
    virtmanager
  ];
}
