# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

# NixOS-WSL specific options are documented on the NixOS-WSL repository:
# https://github.com/nix-community/NixOS-WSL

{ nixos-wsl, config, lib, pkgs, ... }:

{

  imports = [
    nixos-wsl.nixosModules.default
  ];
  wsl.enable = true;
  wsl.defaultUser = "rjoost";

    nix = {
      package = pkgs.nixVersions.stable;
      extraOptions = ''
        experimental-features = nix-command flakes
      '';
    };

    # Set your time zone.
    time.timeZone = "Australia/Brisbane";

    environment.systemPackages = with pkgs; [
      git
    ];

    users.groups.rjoost = {};
    users.users.rjoost = {
      createHome = true;
      extraGroups = ["wheel" "video" "audio" "disk" "networkmanager" "jellyfin"];
      group = "rjoost";
      home = "/home/rjoost";
      description = "Roman Joost";
      isNormalUser = true;
      shell = pkgs.zsh;
      packages = with pkgs; [
        zsh
      ];
    };
    programs.zsh.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.05"; # Did you read the comment?
}
