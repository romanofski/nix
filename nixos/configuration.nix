# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./configs/yubikey.nix
    ./configs/printing.nix
    ./configs/firewall.nix
    ./configs/unfree.nix
    ./services/ntp.nix
    ./services/tor.nix
    ./services/upower.nix
    ./services/gvfs.nix
    ./services/mysql.nix
    ./services/backup.nix
    ];

    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;

    # Setup keyfile
    boot.initrd.secrets = {
      "/crypto_keyfile.bin" = null;
    };

    # Enable swap on luks
    boot.initrd.luks.devices."luks-e0a30253-b542-40c1-889a-94b809b6b651".device = "/dev/disk/by-uuid/e0a30253-b542-40c1-889a-94b809b6b651";
    boot.initrd.luks.devices."luks-e0a30253-b542-40c1-889a-94b809b6b651".keyFile = "/crypto_keyfile.bin";

    systemd.tmpfiles.rules = [ "d /tmp 1777 root root 10d"];

    nix = {
      package = pkgs.nixVersions.stable;
      extraOptions = ''
        experimental-features = nix-command flakes
      '';
    };
    networking.networkmanager = {
      enable = true;
      dns = "systemd-resolved";
      plugins = with pkgs; [
        networkmanager-openvpn
      ];
    };
    networking.hostName = "krombopulos"; # Define your hostname.

    services.avahi = {
      enable = true;
      nssmdns4 = true;
      openFirewall = true;
    };
    services.resolved.enable = true;
    services.resolved.dnssec = "false";

    services.blueman.enable = true;

    services.logind.settings.Login = {
      HandleLidSwitchDocked = "ignore";
      idleAction = "ignore";
      KillUserProcesses = false;
    };

    # Select internationalisation properties.
    console.keyMap = "us";
    console.font = "Lat2-Terminus16";

    i18n.defaultLocale = "en_AU.UTF-8";
    i18n.extraLocaleSettings = {
      LC_ADDRESS = "en_AU.UTF-8";
      LC_IDENTIFICATION = "en_AU.UTF-8";
      LC_MEASUREMENT = "en_AU.UTF-8";
      LC_MONETARY = "en_AU.UTF-8";
      LC_NAME = "en_AU.UTF-8";
      LC_NUMERIC = "en_AU.UTF-8";
      LC_PAPER = "en_AU.UTF-8";
      LC_TELEPHONE = "en_AU.UTF-8";
      LC_TIME = "en_AU.UTF-8";
    };

    # Set your time zone.
    time.timeZone = "Australia/Brisbane";

    # Autocompletion for zsh
    environment.pathsToLink = [ "/share/zsh "];
    # Use this path as otherwise $PATH on SSH is set to /usr/bin et.al
    environment.sessionVariables = { PATH="/run/current-system/sw/bin"; };

    environment.systemPackages = with pkgs; [
      opentabletdriver
      nix-prefetch-scripts
      linuxPackages.tp_smapi
      git
      borgbackup
      jellyfin
      jellyfin-web
      jellyfin-ffmpeg
      # used also by pipewire - pactl
      pkgs.pulseaudio
      btrfs-progs
    ];

    programs.mosh.enable = true;
    programs.dconf.enable = true;
    services.openssh.enable = true;
    services.dbus.packages = with pkgs; [
      pkgs.dconf
      # used by pinentry-gnome3
      pkgs.gcr
    ];

    # Enable touchpad support.
    services.libinput = {
      enable = true;
      touchpad = {
        accelProfile = "flat";
        accelSpeed = "0.6";
        naturalScrolling = false;
        tapping = true;
      };
    };

    programs.niri.enable = true;
    services.greetd = {
      enable = true;
      settings = {
        default_session.command = ''
          ${pkgs.greetd.tuigreet}/bin/tuigreet \
          --time \
          --asterisks \
          --user-menu \
          --cmd "${pkgs.nixgl.nixGLIntel}/bin/nixGLIntel ${config.programs.niri.package}/bin/niri-session"
        '';
      };
    };

    environment.etc."greetd/environments".text = ''
      niri
      zsh
    '';

    fonts.packages = [
      pkgs.dejavu_fonts
      pkgs.fira
      pkgs.fira-code
      pkgs.fira-mono
    ];

    services.fwupd.enable = true;

    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      # If you want to use JACK applications, uncomment this
      #jack.enable = true;

      # use the example session manager (no others are packaged yet so this is enabled by default,
      # no need to redefine it in your config for now)
      #media-session.enable = true;
    };

    # opentabletdriver
    nixpkgs.config.permittedInsecurePackages = [
      "dotnet-runtime-6.0.36"
      "dotnet-sdk-wrapped-6.0.428"
      "dotnet-sdk-6.0.428"
    ];

    # Define a user account. Don't forget to set a password with ‘passwd’.
    users.groups = {
      rjoost = { };
    };

    users.users.rjoost = {
      createHome = true;
      extraGroups = ["wheel" "video" "audio" "disk" "networkmanager" "jellyfin" "calibre-server"];
      group = "rjoost";
      home = "/home/rjoost";
      description = "Roman Joost";
      isNormalUser = true;
      uid = 1000;
      shell = pkgs.zsh;
      packages = with pkgs; [
        zsh
      ];
    };

    users.users.immich.extraGroups = [ "video" "render" ];

    programs.zsh.enable = true;

    # users.groups.rjoost.gid = 1000;

    # This value determines the NixOS release from which the default
    # settings for stateful data, like file locations and database versions
    # on your system were taken. It‘s perfectly fine and recommended to leave
    # this value at the release version of the first install of this system.
    # Before changing this value read the documentation for this option
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
    system.stateVersion = "23.05"; # Did you read the comment?

    nix.gc.automatic = true;
    nix.gc.dates = "weekly";
    nix.gc.options = "--delete-older-than 14d";
    nix.settings.trusted-users = ["root" "rjoost"];
    nix.settings.max-jobs = lib.mkDefault 8;
}
