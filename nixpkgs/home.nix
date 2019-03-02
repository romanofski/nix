{ pkgs, ... }:

let
  pkgsUnstable = import (
    pkgs.fetchFromGitHub {
      owner = "nixos";
      repo = "nixpkgs";
      rev = "86db60f3f3b6d68c61c4a4c54bc4a5bb175a76d8";
      sha256 = "1k5xinimh3ha9020b3z2kzg6b3pjpzc8r2n9cqcnszkzv278g8ck";
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
  pkgs.gdb
  pkgs.gnome3.cheese
  pkgs.gnupg
  pkgs.gsettings_desktop_schemas
  pkgs.inkscape
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
  pkgs.wine
  pkgs.xlockmore
  pkgs.xorg.xbacklight
  pkgsUnstable.firefox
  pkgsUnstable.termonad-with-packages
];

services.syncthing = {
  enable = true;
};
}
