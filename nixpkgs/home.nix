{ pkgs, ... }:

let
  pkgsUnstable = import (
    pkgs.fetchFromGitHub {
      owner = "nixos";
      repo = "nixpkgs";
      rev = "201d739b0ffbebceb444864d1856babcd1a666a8";
      sha256 = "0mfkzmylglpw84w85zs3djpspcx45bg3s62hk4j44dxl2p0fvggj";
    }
  ) { };
in {
home.packages = [
  pkgs.aspellDicts.en
  pkgs.ctags
  pkgs.elinks
  pkgs.feh
  pkgs.file
  pkgs.ghc
  pkgs.gimp
  pkgs.gsettings_desktop_schemas
  pkgs.haskellPackages.ghcid
  pkgs.haskellPackages.hasktags
  pkgs.haskellPackages.hindent
  pkgs.haskellPackages.hlint
  pkgs.ispell
  pkgs.killall
  pkgs.lsof
  pkgs.minetest
  pkgs.noto-fonts-emoji
  pkgs.pavucontrol
  pkgs.pinentry
  pkgs.poppler_utils
  pkgs.python35
  pkgs.python36Packages.syncthing-gtk
  pkgs.tmux
  pkgs.tor-browser-bundle-bin
  pkgs.unzip
  pkgs.xlockmore
  pkgs.xorg.xbacklight
  pkgs.gnome3.cheese
  pkgsUnstable.cabal-install
  # Was not installable:
  #
  # Configuring heterocephalus-1.0.5.2...
  # CallStack (from HasCallStack):
  #   die', called at libraries/Cabal/Cabal/Distribution/Simple/Configure.hs:950:20 in Cabal-2.4.0.1:Distribution.Simple.Configure
  #   configureFinalizedPackage, called at libraries/Cabal/Cabal/Distribution/Simple/Configure.hs:460:12 in Cabal-2.4.0.1:Distribution.Simple.Configure
  #   configure, called at libraries/Cabal/Cabal/Distribution/Simple.hs:596:20 in Cabal-2.4.0.1:Distribution.Simple
  #   confHook, called at libraries/Cabal/Cabal/Distribution/Simple/UserHooks.hs:67:5 in Cabal-2.4.0.1:Distribution.Simple.UserHooks
  #   configureAction, called at libraries/Cabal/Cabal/Distribution/Simple.hs:178:19 in Cabal-2.4.0.1:Distribution.Simple
  #   defaultMainHelper, called at libraries/Cabal/Cabal/Distribution/Simple.hs:115:27 in Cabal-2.4.0.1:Distribution.Simple
  #   defaultMain, called at Setup.hs:2:8 in main:Main
  # Setup: Encountered missing dependencies:
  # containers ==0.5.*
  #
  # Installed via nix-env from termonad git checkout HEAD
  # pkgsUnstable.termonad-with-packages
];

services.syncthing = {
  enable = true;
};
}
