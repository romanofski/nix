{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    nixgl.url = "github:nix-community/nixGL";
  };
  outputs = { self, nixpkgs, nixos-hardware, nixgl, ... }@attrs: {
    nixosConfigurations.krombopulos = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
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
      system = "x86_64-linux";
      specialArgs = attrs;
      modules = [
        ./yoga.nix
      ];
    };
  };
}
