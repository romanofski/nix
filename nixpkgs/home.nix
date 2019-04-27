{ pkgs, ... }:

let
  pkgsUnstable = import (
    pkgs.fetchFromGitHub {
      owner = "nixos";
      repo = "nixpkgs";
      rev = "056b9d0085ed02c4524bf206d0112685327f0eef";
      sha256 = "0rz2m765hbwram55bk20hbn4qby6sg1xfcharp87yq4h8b8m7w5c";
    }
  ) { };
  opts = import ./opts.nix;
  mailhost = "mail.gocept.net";
  realName = "Róman Joost";
in {
  home.packages = [
    pkgs.ansible
    pkgs.arandr
    pkgs.aspellDicts.en
    pkgs.bind
    pkgs.binutils
    pkgs.blueman
    pkgs.bluez
    pkgs.bluez-tools
    pkgs.ctags
    pkgs.elinks
    pkgs.emacs
    pkgs.evince
    pkgs.feh
    pkgs.file
    pkgs.gdb
    pkgs.gimp
    pkgs.git
    pkgs.gnome3.cheese
    pkgs.gnupg
    pkgs.gsettings_desktop_schemas
    pkgs.inkscape
    pkgs.ispell
    pkgs.killall
    pkgs.libreoffice-fresh
    pkgs.lsof
    pkgs.maildrop
    pkgs.minetest
    pkgs.noto-fonts-emoji
    pkgs.openjdk8
    pkgs.openttd
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
    pkgs.vim
    pkgs.wget
    pkgs.wine
    pkgs.xlockmore
    pkgs.xorg.xbacklight
    pkgsUnstable.openra
    pkgsUnstable.firefox
  ];

  services.syncthing = {
    enable = true;
  };
  services.gpg-agent.enable = true;
  services.getmail = {
    enable = true;
    retriever = {
      type = "SimpleIMAPSSLRetriever";
      server = mailhost;
      username = opts.email;
      mailboxes = ["ALL"];
      passwordCommand = "${pkgs.pass}/bin/pass bromeco";
    };
    destination = {
      type = "MDA_external";
      path = "${pkgs.maildrop}/bin/maildrop";
    };
    options = {
      delete = true;
    };
  };

  accounts.email = {
    accounts = {
      bromeco = {
        realName = "${realName}";
        address = "${opts.email}";
        userName = "${opts.email}";
        primary = true;
        flavor = "plain";
        smtp.host = mailhost;
        smtp.tls.enable = true;
        smtp.tls.useStartTls = true;
        passwordCommand = "${pkgs.pass}/bin/pass bromeco";
        msmtp = {
          enable = true;
        };
        notmuch = {
          enable = true;
        };
      };
    };
  };

  programs.zsh = {
    enable = true;
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
    userEmail = "${opts.email}";
    userName = "${realName}";
  };
}
