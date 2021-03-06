# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
    <nixos-hardware/lenovo/thinkpad/t480s>
    ./hardware-configuration.nix
    ./configs/yubikey.nix
    ./configs/printing.nix
    ./configs/virtualisation.nix
    ./configs/firewall.nix
    ./configs/unfree.nix
    ./services/ntp.nix
    ./services/tor.nix
    ./services/upower.nix
    ./services/gvfs.nix
    ];

    systemd.tmpfiles.rules = [ "d /tmp 1777 root root 10d"];

    networking.networkmanager.enable = true;
    networking.networkmanager.dns = "systemd-resolved";
    networking.hostName = "krombopulos"; # Define your hostname.

    services.resolved.enable = true;
    services.resolved.dnssec = "false";

    # Select internationalisation properties.
    console.keyMap = "us";
    console.font = "Lat2-Terminus16";
    i18n.defaultLocale = "en_AU.UTF-8";

    # Set your time zone.
    time.timeZone = "Australia/Brisbane";

    # Autocompletion for zsh
    environment.pathsToLink = [ "/share/zsh "];
    # Use this path as otherwise $PATH on SSH is set to /usr/bin et.al
    environment.sessionVariables = { PATH="/run/current-system/sw/bin"; };

    environment.systemPackages = with pkgs; [
      nix-prefetch-scripts
      linuxPackages.tp_smapi
      git
    ];

    programs.mosh.enable = true;
    services.openssh.enable = true;
    services.dbus.packages = with pkgs; [ gnome3.dconf ];

    # Enable the X11 windowing system.
    services.xserver = {
      enable = true;
      layout = "us";
      xkbModel = "pc105";
      desktopManager.xterm.enable = false;
      desktopManager.xfce = {
        enable = true;
        noDesktop = true;
        enableXfwm = true;
      };
      displayManager.defaultSession = "xfce";
      displayManager.lightdm.enable = true;
      xkbOptions = "compose:ralt";
      exportConfiguration = true;
      videoDrivers = ["intel" "vesa"];
      deviceSection = ''
        Option "TearFree" "true"
      '';
      serverFlagsSection = ''
        Option "OffTime" "10"
        Option "SuspendTime" "5"
        Option "StandbyTime" "3"
      '';

    };
    fonts.fonts = [
      pkgs.dejavu_fonts
      pkgs.fira
      pkgs.fira-code
      pkgs.fira-mono
    ];

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
    nix.trustedUsers = ["root" "rjoost"];
}
