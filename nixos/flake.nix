{
  inputs = {
    #nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    nixpkgs.url = "github:NixOS/nixpkgs/785d183e69c0899387751928fbdcc316b9aac97e";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    nixos-wsl.url = "github:nix-community/NixOS-WSL/release-25.05";
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
    nixosConfigurations.wsl = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = attrs;
      modules = [
        ./wsl.nix
      ];
    };
  };
}
