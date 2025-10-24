{ pkgs, ... }:

{
  programs.vscode = {
    enable = true;
    profiles = {
      default = {
        extensions = with pkgs.vscode-extensions; [
          github.copilot
          asvetliakov.vscode-neovim
          ms-python.python
          ms-pyright.pyright
          bazelbuild.vscode-bazel
          jnoortheen.nix-ide
        ];
        userSettings = builtins.fromJSON (builtins.readFile ./configs/vscode.json);
        };
      };
    };
  home.packages = [
    pkgs.emacs-all-the-icons-fonts
    pkgs.nixfmt-classic
    pkgs.shellcheck
    pkgs.nodePackages.prettier
    pkgs.ripgrep
    pkgs.jetbrains-mono
    pkgs.fira-code
    pkgs.pyright
  ];
}
