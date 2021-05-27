{ pkgs, lib, ... }:

let
  sources = import ./nixpkgsource.nix;
  pkgsUnstable = import sources.nixos-unstable { };
  secrets = import ./secrets.nix;
in with secrets; {

  nixpkgs.overlays = [
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
    ./services/gpg-agent.nix
    ./services/screen-locker.nix
    ./services/syncthing.nix
  ];

  home.packages = [
    pkgs.weechat
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
        smtp.host = mailhost;
        smtp.tls.enable = true;
        smtp.tls.useStartTls = true;
        passwordCommand = "${pkgs.pass}/bin/pass bromeco";
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

  home.sessionVariables = {
    EDITOR = "vim";
  };

  programs.home-manager = {
    enable = true;
    path = "https://github.com/nix-community/home-manager/archive/release-" + lib.fileContents ../release + ".tar.gz";
  };

  programs.msmtp = {
    enable = true;
    extraConfig = ''
      defaults
      syslog on
      domain bromeco.de
      port 587
    '';
  };
}
