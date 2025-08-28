{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    secrets.url = "path:/home/rjoost/works/configs/nixsecrets";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nix-treesitter.url = "github:ratson/nix-treesitter";
  };

  outputs = inputs@{ self, nixpkgs, secrets, home-manager, nix-treesitter }: {
    homeConfigurations = {
      "rjoost@work" = home-manager.lib.homeManagerConfiguration {
        pkgs = import nixpkgs { system = "x86_64-linux"; };
        modules = [
          ./t14s.nix
        ];
        extraSpecialArgs = {inherit inputs; secrets =
          secrets.workSecrets;};
      };
    };
  };
}
