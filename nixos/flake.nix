{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    nixpkgs-otbr.url = "github:NixOS/nixpkgs/pull/502388/head";
    nixgl.url = "github:nix-community/nixGL";
    korrosync.url = "github:szaffarano/korrosync";
    sops-nix = {
      url = "github:Mic92/sops-nix";
    };
  };
  outputs = { self, nixpkgs, nixos-hardware, nixpkgs-otbr, nixgl, korrosync, sops-nix }@attrs:
  let
    system = "x86_64-linux";
    korroPkg = korrosync.packages.${system}.default;
    korrosyncNoTests = korroPkg.overrideAttrs (old: { doCheck = false; });
  in {
    nixosConfigurations.krombopulos = nixpkgs.lib.nixosSystem {
      specialArgs = attrs;
      modules = [
        sops-nix.nixosModules.sops
        ./configuration.nix
        nixos-hardware.nixosModules.lenovo-thinkpad-t480s
        ./services/vpn.nix
      ] ++ [
        ({
          nixpkgs.overlays = [ nixgl.overlay ];
        })
      ];
    };
    nixosConfigurations.yoga = nixpkgs.lib.nixosSystem {
      specialArgs = {
        korrosync = korrosyncNoTests;
      } // { inherit nixpkgs-otbr; };
      modules = [
        sops-nix.nixosModules.sops
        ./yoga.nix
        ./services/bookserver.nix
        ./services/media-server.nix
        ./services/image-server.nix
        ./services/vpn.nix
        ./services/home-automation.nix
        ./services/home-automation/matterjs-server-service.nix
        ./services/rtl2832.nix
        ({ pkgs, ... }: {
          nixpkgs.overlays = [
            (final: prev: {
              matterjs-server = final.callPackage ./pkgs/matterjs-server.nix {};
            })
          ];
        })
        ({nixpkgs-otbr, ...}: {
          imports = [
            (nixpkgs-otbr.outPath +
            "/nixos/modules/services/home-automation/openthread-border-router.nix")
          ];
          nixpkgs.overlays = [
            (final: prev: {
              openthread-border-router =
                nixpkgs-otbr.legacyPackages.x86_64-linux.openthread-border-router;
            })
          ];
        })
      ];
    };
  };
}
