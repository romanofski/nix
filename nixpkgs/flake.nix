{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    secrets.url = "git+ssh://rjoost@krombopulos.lan:/home/rjoost/works/configs/nixsecrets";
    aispamclassifier.url = "github:romanofski/aispamclassifier";
    purebred.url = "github:purebred-mua/purebred/fix/muttaliasparsing";
    home-manager.url = "github:nix-community/home-manager/release-25.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nixgl.url = "github:nix-community/nixGL";
    korrosync.url = "github:szaffarano/korrosync";
  };

  outputs = inputs@{ self, nixpkgs, secrets, aispamclassifier, home-manager, nixgl, purebred, korrosync }:
  let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
    korroPkg = korrosync.packages.${system}.default;
    korrosyncNoTests = korroPkg.overrideAttrs (old: { doCheck = false; });
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
        extraSpecialArgs = {inherit inputs;
        aispamclassifier = aispamclassifier;
        secrets = secrets.homeSecrets;
        purebred = purebred;
        korrosync = korrosyncNoTests;
        };
      };
    };
  };
}
