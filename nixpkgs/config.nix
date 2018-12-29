{ pkgs }:

let unstable = import <unstable> { };
in
{ packageOverrides = super: let self = super.pkgs; in with self; rec {
## My one-shot install environment.
    my-meta-env = buildEnv {
      name = "meta-env";
      paths = [
        haskell-env
      ];
    };

    ## Haskell environment
    my-haskell-env = buildEnv {
      name = "haskell-env";
      paths = with pkgs.haskellPackages; [
        unstable.cabal-install
        ghcid
        hlint
        hindent
        hasktags
        ctags
        ispell
        ghc
      ];
    };

    my-media = buildEnv {
      name = "media";
      paths = [
        pavucontrol
      ];
    };

    my-games = buildEnv {
      name = "games";
      paths = [
        minetest
      ];
    };

    my-wm = buildEnv {
      name = "wm";
      paths = [
        xlockmore
        xorg.xbacklight
        pinentry
        tor-browser-bundle-bin
        noto-fonts-emoji
        feh
        termonad-with-packages
        gsettings_desktop_schemas
        gimp
        python36Packages.syncthing-gtk
      ];
    };

    my-console = buildEnv {
      name = "console";
      paths = [
        tmux
        poppler_utils
        aspellDicts.en
        file
        python35
        lsof
        killall
        unzip
      ];
    };
  };
}
