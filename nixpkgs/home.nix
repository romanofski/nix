{ pkgs, ... }:

let
  pkgsUnstable = import (
    pkgs.fetchFromGitHub {
      owner = "nixos";
      repo = "nixpkgs";
      rev = "178836c9bb65c8eb98ebec865fd8a7370436e1dd";
      sha256 = "14w2hc0bsb7dclzkwgmvhzlxrn763zyb0gdnwh12jv08xmqjvda0";
    }
  ) { };
in {
home.packages = [
  pkgs.arandr
  pkgs.aspellDicts.en
  pkgs.ctags
  pkgs.elinks
  pkgs.emacs
  pkgs.evince
  pkgs.feh
  pkgs.file
  pkgs.gimp
  pkgs.git
  pkgs.gnome3.cheese
  pkgs.gnupg
  pkgs.gsettings_desktop_schemas
  pkgs.ispell
  pkgs.killall
  pkgs.libreoffice-fresh
  pkgs.lsof
  pkgs.minetest
  pkgs.minetest
  pkgs.noto-fonts-emoji
  pkgs.pavucontrol
  pkgs.pinentry
  pkgs.poppler_utils
  pkgs.powertop
  pkgs.python35
  pkgs.python36Packages.syncthing-gtk
  pkgs.silver-searcher
  pkgs.tmux
  pkgs.tor-browser-bundle-bin
  pkgs.unzip
  pkgs.vim
  pkgs.wget
  pkgs.xlockmore
  pkgs.xorg.xbacklight
  pkgsUnstable.firefox
  pkgsUnstable.termonad-with-packages
];

services.syncthing = {
  enable = true;
};
}
