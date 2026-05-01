{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    nixpkgsUnstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    aispamclassifier.url = "github:romanofski/aispamclassifier";
    emacs.url = "github:nix-community/emacs-overlay";
    purebred.url = "github:purebred-mua/purebred/fix/muttaliasparsing";
    home-manager.url = "github:nix-community/home-manager/release-25.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nixgl.url = "github:nix-community/nixGL";
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ self, nixpkgs, nixpkgsUnstable, aispamclassifier, emacs, home-manager, nixgl, purebred, sops-nix }:
  let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
  in {
    homeConfigurations = {
      "rjoost@work" = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          ./t14s.nix
          ./services/llm.nix
          ({ nixpkgs.overlays = [ emacs.overlays.default ]; })
        ];
        extraSpecialArgs = {
          inherit inputs;
          nixpkgsUnstable = import nixpkgsUnstable {
            inherit system;
            config.allowUnfree = true;
          };
          secrets = secrets.workSecrets;
        };
      };
      "rjoost@yoga" = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          ./yoga.nix
        ];
        extraSpecialArgs = {inherit inputs; secrets =
          secrets.yogaSecrets ;};
      };
      "rjoost@home" = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          sops-nix.nixosModules.sops
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
        };
      };
    };
  };
}
