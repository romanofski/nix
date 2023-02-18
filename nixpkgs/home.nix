{ pkgs, lib, ... }:

let
  sources = import ./nixpkgsource.nix;
  pkgsUnstable = import sources.nixos-unstable { };
  secrets = import ./secrets.nix;
  homemanagerRelease = lib.fileContents ../release;
in with secrets; {

  nixpkgs.overlays = [
    (self: super:

    {
      haskellPackages = super.haskellPackages.override {
        overrides = hself: hsuper: {
          workbalance = hsuper.callPackage ./overlays/packages/workbalance.nix {};
          gtfsschedule = hsuper.callPackage ./overlays/packages/gtfsschedule.nix {};
        };
      };
      gtfsschedule-with-packages = super.callPackage ./overlays/packages/gtfsschedule-with-packages.nix {
        inherit (self.haskellPackages) ghcWithPackages;
      };
    })
  ];

  imports = [
    ./essential-packages.nix
    ./emacs.nix
    ./programs/vim.nix
    ./programs/gtfsschedule.nix
    ./programs/termonad.nix
    ./programs/zsh.nix
    ./programs/dunst.nix
    ./programs/notmuch.nix
    ./programs/git.nix
    ./programs/xsession.nix
    ./programs/purebred.nix
    ./programs/gtk.nix
    ./programs/tmux.nix
    ./programs/autorandr.nix
    ./programs/ghci.nix
    ./programs/mcfly.nix
    ./services/gpg-agent.nix
    ./services/screen-locker.nix
    ./services/syncthing.nix
  ];

  home.packages = [
    pkgs.weechat
    pkgs.nix-index
  ];

  nixpkgs.config.allowUnfree = true;


  services.getmail.enable = true;
  services.redshift = {
    enable = true;
    latitude = "-27";
    longitude = "152";
  };

  accounts.email = {
    accounts = {
      bromeco = {
        realName = "${realName}";
        address = "${email}";
        userName = "${email}";
        primary = true;
        flavor = "plain";
        smtp = {
          host = mailhost;
          tls.enable = true;
          tls.useStartTls = true;
        };
        passwordCommand = "${pkgs.pass}/bin/pass flyingcircus-bromeco/roman@bromeco-password";
        imap = {
          host = mailhost;
          tls = {
            enable = true;
            useStartTls = true;
          };
        };
        msmtp = {
          enable = true;
        };
        notmuch = {
          enable = true;
        };
        getmail = {
          enable = true;
          mailboxes = ["ALL"];
          destinationCommand = "${pkgs.maildrop}/bin/maildrop";
          delete = true;
        };
      };
    };
  };

  home = {
    stateVersion = "22.11";
    username = "rjoost";
    homeDirectory = "/home/rjoost";
    sessionVariables = {
      EDITOR = "vim";
    };
  };

  programs.home-manager = {
    enable = true;
    path = "https://github.com/nix-community/home-manager/archive/release-" + homemanagerRelease + ".tar.gz";
  };

  programs.msmtp = {
    enable = true;
    extraConfig = ''
      defaults
      syslog on
      domain bromeco.de
      port 587
      tls_starttls on
    '';
  };
}
