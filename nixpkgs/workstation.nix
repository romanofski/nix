{ pkgs, ... }:

let
  pkgsUnstable = import (
    pkgs.fetchFromGitHub {
      owner = "nixos";
      repo = "nixpkgs";
      rev = "89a650a593dc00dc34570e34190dbba36f70d889";
      sha256 = "028kliy2b1p9wcvf3hhfffi4q16hc3bfbb2nqcv6gps1a4i8vr40";
    }
  ) { };
  mailhost = "mail.googlemail.com";
  realName = "Róman Joost";
  email = "roman.joost@superloop.com";
in {
  imports = [
    ./emacs.nix
    ./vim.nix
    ./xmonad.nix
  ];

  home.packages = [
    pkgs.gsettings_desktop_schemas
    pkgs.ibm-plex
    pkgs.noto-fonts-emoji
    pkgs.fontconfig
    pkgs.xmobar
    pkgs.yarn
    pkgs.haskellPackages.termonad
    pkgs.nodejs-slim-10_x
  ];

  services.syncthing = {
    enable = true;
  };
  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
  };
  services.screen-locker = {
    enable = true;
    lockCmd = "/usr/bin/xautolock -locknow";
  };
  services.redshift = {
    enable = true;
    latitude = "-27";
    longitude = "152";
  };

  fonts.fontconfig = {
    enable = true;
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
      };
    };
  };

  home.sessionVariables = {
    EDITOR = "vim";
  };

  programs.home-manager = {
    enable = true;
  };
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    sessionVariables = {
      SHELL = "${pkgs.zsh}/bin/zsh";
    };
    shellAliases = {
      c = "cd ..";
    };
    loginExtra = ''
      export NIX_PATH=$HOME/.nix-defexpr/channels:$NIX_PATH
      #User specific environment and startup programs
      export LOCALE_ARCHIVE="$(readlink ~/.nix-profile/lib/locale)/locale-archive"

      if [ -e /home/roman.joost/.nix-profile/etc/profile.d/nix.sh ]; then . /home/roman.joost/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
      # home manager
      if [ -e /home/roman.joost/.nix-profile/etc/profile.d/hm-session-vars.sh ]; then . /home/roman.joost/.nix-profile/etc/profile.d/hm-session-vars.sh; fi # added by Nix installer
    '';
    oh-my-zsh = {
      enable = true;
      plugins = [ "git" "sudo"];
      theme = "flazz";
    };
  };
  programs.notmuch = {
    enable = true;
    new = {
      tags=["unread" "inbox"];
    };
    search={
      excludeTags=["deleted" "spam"];
    };
    maildir = {
      synchronizeFlags=false;
    };
  };
  programs.msmtp = {
    enable = true;
    extraConfig = ''
      defaults
      syslog on
      domain bromeco.de
    '';
  };
  programs.git = {
    enable = false;
    extraConfig = {
      push = {
        pushOption = "roman.joost:160:aef0999309b5d2c2d93f612c2a7abf00";
      };
    };
    aliases = {
	    lg = "log --graph --pretty=format:'%Cred%h%Creset - %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative";
	    ll = "!git st; git --no-pager log --pretty='format:%Cred%cr %Cblue%h %Cgreen%an:%Creset %s%C(yellow)%d' -n20; echo";
	    # Nice list of branches by age
	    bb = "for-each-ref --sort=-committerdate --format='%1B[36m%(committerdate:relative) %1B[33m%(refname:short)%1B[0;m' refs/heads/";
	    info = "config --list";
    };
    ignores = [
      "dist"
      ".cabal-sandbox"
      "cabal.sandbox.config"
      ".stack-work"
      "*.o"
      "*.hi"
    ];
    signing = {
      key = "D02BC6E095A0446267E1F43C0133D0C73A765B52";
    };
    userEmail = "${email}";
    userName = "${realName}";
  };

  gtk = {
    enable = true;
    theme = {
      package = pkgs.gnome3.gnome_themes_standard;
      name = "Adwaita";
    };
  };
  xresources.properties = {
    "Xft.antialias" = 1;
    "Xft.autohint" = 0;
    "Xft.hinting" = 1;
    "Xft.hintstyle" = "hintfull";
    "Xft.lcdfilter" = "lcddefault";
    "Xft.rgba" = "rgb";
    "Xcursor.theme" = "core";
    "Xautolock.time" = 20;
    "Xautolock.locker" = "xlock";
    "Xautolock.corners" = "+0-0";
    "Xautolock.cornerdelay" = 3;

    "XLock.foreground" = "White";
    "XLock.background" = "Gray20";
    "XLock.echokeys" = 1;
    "XLock.usefirst" = 1;
    "XLock.echokey" = "*";

    "Xft.dpi" = 100;
  };
  home.file = [
    {
      target = ".tmux.conf";
      text = builtins.readFile ./configs/tmux.conf;
    }
  ];
  xdg = {
    enable = true;
    configFile = [
      {
        target = "purebred/purebred.hs";
        text = builtins.readFile ./configs/purebred.hs;
      }
      {
        target = "gtfs/config.cfg";
        text = builtins.readFile ./configs/gtfsschedule.cfg;
      }
    ];
  };
  xsession.enable = true;
  systemd.user.services = {
    hamster-service = {
      Unit = {
        Description = "Hamster Time Tracking service";
      };
      Service = {
        Type = "simple";
        ExecStart="/home/roman.joost/works/hamster/src/hamster-service";
      };
    };
    hamster-windows-service = {
      Unit = {
        Description = "Hamster Time Tracking windows service";
      };
      Service = {
        Type = "simple";
        ExecStart="/home/roman.joost/works/hamster/src/hamster-windows-service";
      };
    };
  };
}
