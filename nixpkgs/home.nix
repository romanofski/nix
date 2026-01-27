{ secrets, aispamclassifier, purebred, korrosync, pkgs, lib, config, ... }:

{

  nixpkgs.overlays = [
    (self: super:

    {
      getmail6 = super.getmail6.overrideAttrs (oldAttrs: {
        meta = oldAttrs.meta // {
          mainProgram = "getmail";
        };
      });
      haskellPackages = super.haskellPackages.override {
        overrides = hself: hsuper: {
          workbalance = hsuper.callPackage ./overlays/packages/workbalance.nix {};
          # gtfsschedule = hsuper.callPackage ./overlays/packages/gtfsschedule.nix {};
          # protocol-buffers = hsuper.callPackage ./overlays/packages/protocol-buffers.nix {};
        };
      };
      # gtfsschedule-with-packages = super.callPackage ./overlays/packages/gtfsschedule-with-packages.nix {
        #  inherit (self.haskellPackages) ghcWithPackages;
        # };

    })
  ];

  imports = [
    ./essential-packages.nix
    ./emacs.nix
    ./programs/vim.nix
    # ./programs/gtfsschedule.nix
    ./programs/terminal.nix
    ./programs/zsh.nix
    ./programs/notmuch.nix
    ./programs/xsession.nix
    ./programs/purebred.nix
    ./programs/gtk.nix
    ./programs/tmux.nix
    ./programs/maildrop.nix
    ./programs/ghci.nix
    ./programs/mcfly.nix
    ./programs/niri.nix
    ./programs/notification.nix
    ./programs/passwordmanagement.nix
    ./services/gpg-agent.nix
    ./services/filesync.nix
    ./services/phoneconnect.nix
  ];

  services.gnome-keyring.enable = true;

  home.packages = [
    pkgs.weechat
    pkgs.nix-index
    pkgs.unrar
    pkgs.chromium
    pkgs.digikam
    pkgs.freecad
    pkgs.simple-scan
    pkgs.android-studio
    pkgs.jq
    aispamclassifier.packages.x86_64-linux.default
    purebred.packages.x86_64-linux.default
    # niri
    pkgs.wireplumber
    pkgs.xwayland-satellite
    pkgs.swaybg # wallpaper
    pkgs.poppler-utils # pdftotext
    pkgs.signal-desktop
    (pkgs.writeShellScriptBin "viewpdfwrapper"
    ''
      #!/bin/bash

      if [ -n "$SSH_CONNECTION" ]; then
      # When connected remotely
      ${pkgs.poppler-utils}/bin/pdftotext "$@" - | less
      else
      # When locally on Wayland
      ${pkgs.evince}/bin/evince "$@"
      fi
    ''
    )
  ];

  programs.git = import ./programs/git.nix { secrets = secrets; pkgs = pkgs; useGCM = false; };
  programs.home-manager.enable = true;

  nixpkgs.config.allowUnfree = true;

  xdg.mime.enable = true;
  xdg.mime.desktopFileUtilsPackage = pkgs.desktop-file-utils;
  xdg.desktopEntries = {
    viewpdfwrapper = {
      name = "viewpdfwrapper";
      exec = "viewpdfwrapper %f";
      type = "Application";
      terminal = false;
      mimeType = ["application/pdf"];
    };
    FirefoxGL = {
      name = "Firefox (GL)";
      type = "Application";
      icon = "firefox";
      exec = "${pkgs.nixgl.nixGLIntel}/bin/nixGLIntel firefox --name firefox %U";
      genericName = "Web Browser";
      mimeType = [
        "text/html"
        "text/xml"
        "application/xhtml+xml"
        "application/vnd.mozilla.xul+xml"
        "x-scheme-handler/http"
        "x-scheme-handler/https"
      ];
    };
  };
  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "application/pdf" = ["viewpdfwrapper.desktop"];
      "text/html" = ["firefox.desktop"];
      "x-scheme-handler/http" = ["firefox.desktop"];
      "x-scheme-handler/https" = ["firefox.desktop"];
    };
  };

  services.getmail.enable = true;
  services.redshift = {
    enable = true;
    latitude = "-27";
    longitude = "152";
  };

  accounts.email = {
    accounts = {
      bromeco = {
        realName = "${secrets.realName}";
        address = "${secrets.email}";
        userName = "${secrets.email}";
        primary = true;
        flavor = "plain";
        smtp = {
          host = secrets.mailhost;
          tls.enable = true;
          tls.useStartTls = true;
        };
        passwordCommand = "${pkgs.pass}/bin/pass flyingcircus-bromeco/roman@bromeco-password";
        imap = {
          host = secrets.mailhost;
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

  systemd.user.services = {
    aispamclassifier = {
      Unit = { Description = "AI Spam classifier service"; };
      Service = {
        ExecStart = "${aispamclassifier.packages.x86_64-linux.default}/bin/server";
        Environment = "AISPAMCLASSIFIER_MODEL=/home/rjoost/works/aispamclassifier/bert-spam-classifier-final";
      };
      Install = { WantedBy = [ "default.target" ]; };
    };
    korrosync = {
      Unit = { Description = "KOReader Sync Server"; };
      Service = {
        ExecStart = "${korrosync}/bin/korrosync";
        Environment = [
          "KORROSYNC_DB_PATH=${config.home.homeDirectory}/works/korrosync"
        ];
        Restart = "on-failure";
        RestartSec = "5s";
      };
      Install = { WantedBy = [ "default.target" ]; };
    };
  };
}
