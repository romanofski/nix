{ pkgs, ... }:

let
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

  nix = {
    package = pkgs.nix;
    settings.experimental-features = ["nix-command" "flakes"];
  };

  imports = [
    ./emacs.nix
    ./programs/vim.nix
    ./programs/zsh.nix
    ./programs/notmuch.nix
    ./programs/purebred.nix
    # currently uses dconf which is stuffy
    #./programs/gtk.nix
    ./programs/tmux.nix
    ./programs/ghci.nix
    ./services/gpg-agent.nix
    ./services/emacs.nix
  ];

  programs.git = import ./programs/git.nix { pkgs = pkgs; useGCM =true; };

  nixpkgs.config.allowUnfree = true;
  home.stateVersion = "23.05"; # Please read the comment before changing.


  home.packages = with pkgs; [
    bzip2
    elinks
    feh
    gcc
    git-lfs
    glibcLocales
    haskellPackages.workbalance
    nix-index
    socat
    tmux
    iproute2
    nodejs_22
    yarn-berry
    bazel_7
    gnumake
    (google-cloud-sdk.withExtraComponents ([
      google-cloud-sdk.components.cloud-datastore-emulator
      google-cloud-sdk.components.app-engine-python
    ]))
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

  programs.zsh.loginExtra = ''
    export DISPLAY=$(ip route|awk '/^default/{print $3}'):0.0
    export DOCKER_HOST=unix://$XDG_RUNTIME_DIR/podman/podman.sock
    export DOCKER_COMMAND=podman
  '';
}
