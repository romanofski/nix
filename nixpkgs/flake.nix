{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    nixpkgsUnstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    secrets.url = "git+ssh://rjoost@krombopulos.lan:/home/rjoost/works/configs/nixsecrets";
    aispamclassifier.url = "github:romanofski/aispamclassifier";
    emacs.url = "github:nix-community/emacs-overlay";
    purebred.url = "github:purebred-mua/purebred/master";
    home-manager.url = "github:nix-community/home-manager/release-25.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nixgl.url = "github:nix-community/nixGL";
  };

  outputs = inputs@{ self, nixpkgs, nixpkgsUnstable, secrets, aispamclassifier, emacs, home-manager, nixgl, purebred}:
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
          ./home.nix
          ./emacs.nix
          ./programs/vim.nix
          ./programs/terminal.nix
          ./programs/zsh.nix
          ./programs/notmuch.nix
          ./programs/xsession.nix
          ./programs/purebred.nix
          ./programs/gtk.nix
          ./programs/tmux.nix
          ./programs/maildrop.nix
          ./programs/ghci.nix
          ./programs/mcfly.nix
          ./programs/niri.nix
          ./programs/notification.nix
          ./programs/passwordmanagement.nix
          ./programs/maildrop.nix
          ./services/gpg-agent.nix
          ./services/filesync.nix
          ./services/phoneconnect.nix
        ] ++ [
          ({
            nixpkgs.overlays = [ nixgl.overlay emacs.overlays.default ];
          })
        ];
        extraSpecialArgs = {
          inherit inputs;
          nixpkgsUnstable = import nixpkgsUnstable {
            inherit system;
            config.allowUnfree = true;
          };
          aispamclassifier = aispamclassifier;
          secrets = secrets.homeSecrets;
          purebred = purebred;
        };
      };
    };
  };
}
