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
      username = opts.username;
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
        realName = "Róman Joost";
        address = "roman@bromeco.de";
        userName = "${opts.username}";
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
}
