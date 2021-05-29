{ pkgs, ... }:

let
  secrets = import ./secrets.nix;
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

    (self: super:

    {
      haskellPackages = super.haskellPackages.override {
        overrides = hself: hsuper: {
          workbalance = hsuper.callPackage ./overlays/packages/workbalance.nix {};
          gtfsschedule = hsuper.callPackage ./overlays/packages/gtfsschedule.nix {};
          # latest release is b0rked with Cabal version
          protocol-buffers = hsuper.callPackage ./overlays/packages/protocol-buffers.nix {};
          # tests currently b0rked
          # *** Exception: <stdout>: hPutChar: resource vanished (Broken pipe)
          # Test suite doctests: FAIL
          termonad = pkgs.haskell.lib.dontCheck hsuper.termonad;
        };
      };
      gtfsschedule-with-packages = super.callPackage ./overlays/packages/gtfsschedule-with-packages.nix {
        inherit (self.haskellPackages) ghcWithPackages;
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
    ./programs/xmobar.nix
    ./programs/offlineimap.nix
    ./services/gpg-agent.nix
    ./services/screen-locker.nix
    ./services/hamster.nix
    ./services/syncthing.nix
  ];

  nixpkgs.config.allowUnfree = true;

  home.packages = [
    pkgs.feh
    pkgs.nix-index
    pkgs.elinks
    pkgs.glibcLocales
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
        userName = "roman.joost@clipchamp.com";
        primary = true;
        flavor = "gmail.com";
        smtp.host = "smtp.gmail.com";
        smtp.tls.enable = true;
        smtp.tls.useStartTls = true;
        passwordCommand = "${pkgs.pass}/bin/pass clipchamp/offlineimap-gmail";
        msmtp = {
          enable = true;
        };
        offlineimap = {
          enable = true;
          postSyncHookCommand = ''
            ${pkgs.notmuch}/bin/notmuch --config ~/.config/notmuch/notmuchrc new
          '';
          extraConfig = {
            account = {
              autorefresh = 5;
              synclabels = true;
            };
            local = {
              sync_deletes = false;
            };
            remote = {
              sslcacertfile = "/etc/pki/tls/certs/ca-bundle.crt";
            };
          };
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
