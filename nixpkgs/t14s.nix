{ pkgs, ... }:

let
  secrets = import ./secrets.nix;
  pkgSrc = builtins.fetchTarball {
    # current nixos-unstable HEAD as of 15/08/2020
    url = "https://github.com/NixOS/nixpkgs/archive/32b46dd897ab2143a609988a04d87452f0bbef59.tar.gz";
    sha256 = "1gzfrpjnr1bz9zljsyg3a4zrhk8r927sz761mrgcg56dwinkhpjk";
  };
  unstable = import pkgSrc {
  };
in with secrets; {

  nixpkgs.overlays = [
    (self: super:

    {
      python3Packages = super.python3Packages.override {
        overrides = pself: psuper: {
          dbus-python = psuper.dbus-python.overridePythonAttrs(old: rec {
            version = "1.2.16";
            src = psuper.fetchPypi {
              pname = "dbus-python";
              inherit version;
              sha256 = "196m5rk3qzw5nkmgzjl7wmq0v7vpwfhh8bz2sapdi5f9hqfqy8qi";
            };
          });
        };
      };
    })
  ];

  imports = [
    ./emacs.nix
    ./programs/vim.nix
    ./programs/gtfsschedule.nix
    ./programs/termonad.nix
    ./programs/zsh.nix
    ./programs/dunst.nix
    ./programs/notmuch.nix
    ./programs/xsession.nix
    ./programs/purebred.nix
    ./programs/gtk.nix
    ./programs/tmux.nix
    ./programs/ghci.nix
    ./services/gpg-agent.nix
    ./services/screen-locker.nix
  ];

  nixpkgs.config.allowUnfree = true;

  home.packages = [
    pkgs.feh
    pkgs.xmobar
    pkgs.nix-index
  ];

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "rjoost";
  home.homeDirectory = "/home/rjoost";

  fonts.fontconfig = {
    enable = true;
  };

  accounts.email = {
    accounts = {
      clipchamp = {
        realName = "${realName}";
        address = "${email}";
        userName = "roman.joost";
        primary = true;
        flavor = "gmail.com";
        smtp.host = "smtp.gmail.com";
        smtp.tls.enable = true;
        smtp.tls.useStartTls = true;
        passwordCommand = "${pkgs.pass}/bin/pass clipchamp/smtp-gmail";
        msmtp = {
          enable = true;
        };
        notmuch = {
          enable = true;
        };
      };
    };
  };

  home.sessionVariables = {
    EDITOR = "vim";
  };

  programs.home-manager = {
    enable = true;
  };
  programs.msmtp = {
    enable = true;
    extraConfig = ''
      defaults
      syslog on
      domain clipchamp.com
      port 587
    '';
  };

}
