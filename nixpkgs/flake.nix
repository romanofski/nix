{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    secrets.url = "git+ssh://krombopulos.lan:/home/rjoost/works/configs/nixsecrets";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ self, nixpkgs, secrets, home-manager }:
  let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
  in {
    homeConfigurations = {
      "rjoost@work" = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          ./t14s.nix
        ];
        extraSpecialArgs = {inherit inputs; secrets =
          secrets.workSecrets;};
      };
      "rjoost@home" = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          ./home.nix
        ];
        extraSpecialArgs = {inherit inputs; secrets =
          secrets.homeSecrets;};
      };
    };
  };
}
