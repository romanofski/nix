{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    nixgl.url = "github:nix-community/nixGL";
    secrets.url = "git+ssh://rjoost@krombopulos.lan:/home/rjoost/works/configs/nixsecrets";
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = { self, nixpkgs, nixpkgs-unstable, nixos-hardware, nixgl, secrets, sops-nix }@attrs:
  let
    system = "x86_64-linux";
    pkgs-unstable = import nixpkgs-unstable { inherit system; config = {}; };
  in {
    nixosConfigurations.krombopulos = nixpkgs.lib.nixosSystem {
      specialArgs = attrs;
      modules = [
        ./configuration.nix
        nixos-hardware.nixosModules.lenovo-thinkpad-t480s
        ./services/vpn.nix
      ] ++ [
        ({
          nixpkgs.overlays = [ nixgl.overlay ];
        })
      ];
    };
    nixosConfigurations.yoga = nixpkgs.lib.nixosSystem {
      inherit system;
      specialArgs = {
        inherit attrs pkgs-unstable;
        secrets = secrets.yogaSecrets;
      };
      modules = [
        ./yoga.nix
        ./services/bookserver.nix
        ./services/media-server.nix
        ./services/image-server.nix
        ./services/vpn.nix
        ./services/httpd.nix
        ./services/home-automation.nix
        ./services/home-automation/eufy-security-ws-service.nix
        ./services/rtl2832.nix
        sops-nix.nixosModules.sops
        ({ pkgs, ... }: {
          imports = [
            "${nixpkgs-unstable}/nixos/modules/services/web-apps/bookorbit.nix"
            "${nixpkgs-unstable}/nixos/modules/services/home-automation/matterjs-server.nix"
          ];
          nixpkgs.overlays = [
            (final: prev: {
              eufy-security-ws = final.callPackage ./pkgs/eufy-security-ws.nix {};
              bookorbit = pkgs-unstable.bookorbit;
              matterjs-server = pkgs-unstable.matterjs-server;
            })
          ];
        })
      ];
    };
  };
}
