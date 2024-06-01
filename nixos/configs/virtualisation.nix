{ config, pkgs, ... }:

{
  virtualisation = {
    libvirtd = {
      enable = true;
    };
    docker = {
      enable = true;
      storageDriver = "overlay2";
      autoPrune = {
        enable = true;
      };
    };
  };
  environment.systemPackages = with pkgs; [
    virt-manager
  ];
}
