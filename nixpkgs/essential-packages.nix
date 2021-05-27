{ pkgs, ... }:

let 
sources = import ./nixpkgsource.nix;
pkgsUnstable = import sources.nixos-unstable { };
in 
{
  home.packages = [
    pkgs.a2ps
    pkgs.acpilight
    pkgs.antiword
    pkgs.arandr
    pkgs.bind
    pkgs.binutils
    pkgs.blueman
    pkgs.bluez
    pkgs.bluez-tools
    pkgs.cachix
    pkgs.ctags
    pkgs.elinks
    pkgs.evince
    pkgs.feh
    pkgs.ffmpeg
    pkgs.file
    pkgs.gimp
    pkgs.git
    pkgs.gnome3.cheese
    pkgs.gnupg
    pkgs.haskellPackages.xmobar
    pkgs.ibm-plex
    pkgs.inkscape
    pkgs.ispell
    pkgs.killall
    pkgs.libreoffice-fresh
    pkgs.lsof
    pkgs.noto-fonts-emoji
    pkgs.pandoc
    pkgs.parallel
    pkgs.pass
    pkgs.pavucontrol
    pkgs.pinentry
    pkgs.poppler_utils
    pkgs.powertop
    pkgs.silver-searcher
    pkgs.sshpass
    pkgs.tmux
    pkgs.unzip
    pkgs.urlscan
    pkgs.usbutils
    pkgs.wget
    pkgs.xlockmore
    pkgs.xss-lock
    pkgsUnstable.firefox
    pkgsUnstable.hamster
    pkgsUnstable.xfce4-14.thunar
    pkgsUnstable.xfce4-14.thunar-volman
  ];
}
