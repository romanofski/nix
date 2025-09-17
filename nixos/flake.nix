{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
  };
  outputs = { self, nixpkgs, nixos-hardware, ... }@attrs: {
    nixosConfigurations.krombopulos = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = attrs;
      modules = [
        ./configuration.nix
        nixos-hardware.nixosModules.lenovo-thinkpad-t480s
      ];
    };
  };
}
