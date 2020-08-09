{ pkgs, ... }:

let
  pkgsUnstable = import <unstable> {
    overlays = [
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
  };
  secrets = import ./secrets.nix;
in with secrets; {
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
    ./programs/ghci.nix
    ./services/gpg-agent.nix
    ./services/screen-locker.nix
  ];

  nixpkgs.config.allowUnfree = true;

  home.packages = [
    pkgs.google-chrome
  ];

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
