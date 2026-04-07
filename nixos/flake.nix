{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    nixpkgs-otbr.url = "github:NixOS/nixpkgs/pull/502388/head";
    nixgl.url = "github:nix-community/nixGL";
    korrosync.url = "github:szaffarano/korrosync";
    secrets.url = "git+ssh://rjoost@krombopulos.lan:/home/rjoost/works/configs/nixsecrets";
  };
  outputs = { self, nixpkgs, nixos-hardware, nixpkgs-otbr, nixgl, korrosync, secrets }@attrs:
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
        secrets = secrets.homeSecrets; 
      } // { inherit nixpkgs-otbr; };
      modules = [
        ./yoga.nix
        ./services/bookserver.nix
        ./services/media-server.nix
        ./services/image-server.nix
        ./services/vpn.nix
        ./services/home-automation.nix
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
