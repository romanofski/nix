{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    nixpkgs-otbr.url = "github:NixOS/nixpkgs/pull/502388/head";
    nixgl.url = "github:nix-community/nixGL";
    korrosync.url = "github:szaffarano/korrosync";
    secrets.url = "git+ssh://rjoost@krombopulos.lan:/home/rjoost/works/configs/nixsecrets";
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = { self, nixpkgs, nixos-hardware, nixpkgs-otbr, nixgl, korrosync, secrets, sops-nix }@attrs:
  let
    system = "x86_64-linux";
    korroPkg = korrosync.packages.${system}.default;
    korrosyncNoTests = korroPkg.overrideAttrs (old: { doCheck = false; });
  in {
    nixosConfigurations.krombopulos = nixpkgs.lib.nixosSystem {
      specialArgs = attrs;
      modules = [
        ./configuration.nix
        nixos-hardware.nixosModules.lenovo-thinkpad-t480s
      ] ++ [
        ({
          nixpkgs.overlays = [ nixgl.overlay ];
        })
      ];
    };
    nixosConfigurations.yoga = nixpkgs.lib.nixosSystem {
      specialArgs = {
        korrosync = korrosyncNoTests;
        secrets = secrets.yogaSecrets;
      } // { inherit nixpkgs-otbr; };
      modules = [
        ./yoga.nix
        ./services/bookserver.nix
        ./services/media-server.nix
        ./services/image-server.nix
        ./services/vpn.nix
        ./services/home-automation.nix
        ./services/home-automation/matterjs-server-service.nix
        ./services/home-automation/eufy-security-ws-service.nix
        ./services/rtl2832.nix
        sops-nix.nixosModules.sops
        ({ pkgs, ... }: {
          nixpkgs.overlays = [
            (final: prev: {
              matterjs-server = final.callPackage ./pkgs/matterjs-server.nix {};
              eufy-security-ws = final.callPackage ./pkgs/eufy-security-ws.nix {};
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
