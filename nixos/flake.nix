{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    nixgl.url = "github:nix-community/nixGL";
    korrosync.url = "github:szaffarano/korrosync";
  };
  outputs = { self, nixpkgs, nixos-hardware, nixgl, korrosync }@attrs:
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
      specialArgs = { korrosync = korrosyncNoTests; };
      modules = [
        ./yoga.nix
        ./services/bookserver.nix
        ./services/media-server.nix
        ./services/image-server.nix
      ];
    };
  };
}
