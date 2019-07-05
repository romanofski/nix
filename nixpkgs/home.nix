{ pkgs, ... }:

let
  pkgsUnstable = import <unstable> { };
  mailhost = "mail.gocept.net";
  realName = "Róman Joost";
  email = "roman@bromeco.de";
in {
  imports = [
    ./emacs.nix
    ./vim.nix
    ./games.nix
  ];

  home.packages = [
    pkgs.emacs-all-the-icons-fonts
    pkgs.ansible
    pkgs.arandr
    pkgs.aspellDicts.de
    pkgs.aspellDicts.en
    pkgs.bind
    pkgs.binutils
    pkgs.blueman
    pkgs.bluez
    pkgs.bluez-tools
    pkgs.ctags
    pkgs.elinks
    pkgs.evince
    pkgs.feh
    pkgs.file
    pkgs.fira
    pkgs.fira-code
    pkgs.fira-mono
    pkgs.fira-code-symbols
    pkgs.gdb
    pkgs.gimp
    pkgs.git
    pkgs.gnome3.cheese
    pkgs.gnupg
    pkgs.gsettings_desktop_schemas
    pkgs.ibm-plex
    pkgs.inkscape
    pkgs.ispell
    pkgs.killall
    pkgs.libnotify
    pkgs.libreoffice-fresh
    pkgs.lsof
    pkgs.maildrop
    pkgs.noto-fonts-emoji
    pkgs.parallel
    pkgs.pass
    pkgs.pavucontrol
    pkgs.pinentry
    pkgs.poppler_utils
    pkgs.powertop
    pkgs.python36Packages.syncthing-gtk
    pkgs.silver-searcher
    pkgs.sshpass
    pkgs.tmux
    pkgs.torbrowser
    pkgs.unzip
    pkgs.wget
    pkgs.xautolock
    pkgs.xlockmore
    pkgs.xorg.xbacklight
    pkgsUnstable.firefox
  ];

  services.getmail.enable = true;
  services.syncthing = {
    enable = true;
  };
  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
  };
  services.screen-locker = {
    enable = true;
    lockCmd = "${pkgs.xautlock}/bin/xautolock -locknow";
  };
  services.redshift = {
    enable = true;
    latitude = "-27";
    longitude = "152";
  };
  services.dunst = {
    enable = true;
    iconTheme = {
      package = pkgs.gnome3.gnome_themes_standard;
      name = "Adwaita";
    };
    settings = {
      global = {
        geometry = "500x5-30+50";
        transparency = 10;
        font = "Roboto 13";
        padding = 15;
        horizontal_padding = 17;
        word_wrap = true;
        follow = "keyboard";
        format = "%s %p %I\n%b";
        markup = "full";
      };

      urgency_low = {
        timeout = 5;
      };

      urgency_normal = {
        timeout = 10;
      };

      urgency_critical = {
        timeout = 15;
      };
    };
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
  };
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    shellAliases = {
      c = "cd ..";
    };
    oh-my-zsh = {
      enable = true;
      plugins = [ "git" "sudo"];
      theme = "pygmalion";
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
    enable = true;
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
  fonts.fontconfig = {
    enable = true;
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
      text = builtins.readFile ./tmux.conf;
    }
  ];
  xdg = {
    enable = true;
    configFile = [
      {
        target = "purebred/purebred.hs";
        text = builtins.readFile ./purebred.hs;
      }
    ];
  };
}
