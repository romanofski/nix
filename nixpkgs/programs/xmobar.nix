{ pkgs, ... }:

{
  nixpkgs.overlays = [
    (self: super:

    {
      haskellPackages = super.haskellPackages.override {
        overrides = hself: hsuper: {
          workbalance = hsuper.callPackage ../overlays/packages/workbalance.nix {};
        };
      };
    })

  ];

  home.packages = [
    pkgs.xmobar
    pkgs.haskellPackages.workbalance
  ];

  xdg = {
    enable = true;
    configFile = {
      xmobarrc = {
        target = ".xmobarrc";
        text = builtins.readFile ./configs/xmobarrc;
      };
    };
  };
}
