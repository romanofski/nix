# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./yubikey.nix
      ./printing.nix
      ./virtualisation.nix
      ./firewall.nix
      ./unfree.nix
      ./services/ntp.nix
      ./services/tor.nix
    ];

  networking.networkmanager.enable = true;
  networking.hostName = "krombopulos"; # Define your hostname.

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_AU.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Australia/Brisbane";

  # Autocompletion for zsh
  environment.pathsToLink = [ "/share/zsh "];

  environment.systemPackages = with pkgs; [
    nix-prefetch-scripts
    linuxPackages.tp_smapi
    home-manager
    git
  ];

  services.openssh.enable = true;
  services.dbus.packages = with pkgs; [ gnome3.dconf ];

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "us";
    xkbModel = "pc105";
    desktopManager.default = "none";
    desktopManager.xterm.enable = false;
    desktopManager.xfce.enable = true;
    displayManager.lightdm.enable = true;
    xkbOptions = "compose:ralt";

    windowManager.default = "xmonad";
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = haskellPackages : [haskellPackages.xmobar];
    };
  };

  # Enable touchpad support.
  services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
    users.users.rjoost = {
      createHome = true;
      extraGroups = ["wheel" "video" "audio" "disk" "networkmanager"];
      group = "rjoost";
      home = "/home/rjoost";
      isNormalUser = true;
      uid = 1000;
      shell = pkgs.zsh;
    };

    users.groups.rjoost.gid = 1000;

    # This value determines the NixOS release with which your system is to be
      # compatible, in order to avoid breaking some software such as database
    # servers. You should change this only after NixOS release notes say you
    # should.
    system.stateVersion = "19.03"; # Did you read the comment?

    nix.gc.automatic = true;
    nix.gc.dates = "weekly";
    nix.gc.options = "--delete-older-than 14d";
}
