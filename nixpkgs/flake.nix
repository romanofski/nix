{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    secrets.url = "git+ssh://rjoost@krombopulos.lan:/home/rjoost/works/configs/nixsecrets";
    aispamclassifier.url = "github:romanofski/aispamclassifier";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nixgl.url = "github:nix-community/nixGL";
  };

  outputs = inputs@{ self, nixpkgs, secrets, aispamclassifier, home-manager, nixgl }:
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
        ] ++ [
          ({
            nixpkgs.overlays = [ nixgl.overlay ];

          })
        ];
        extraSpecialArgs = {inherit inputs; aispamclassifier =
          aispamclassifier; secrets =
            secrets.homeSecrets; };
      };
    };
  };
}
