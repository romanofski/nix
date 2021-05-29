{ pkgs, ... }:

let
  config = ./vim/config.vim;
  sources = import ../nixpkgsource.nix;
  pkgsUnstable = import sources.nixos-unstable { };
in {
  programs.neovim = {
    enable = true;
    plugins = with pkgsUnstable.vimPlugins; [
      bufexplorer
      colors-solarized
      ctrlp-vim
      vim-nix
      vim-airline
      vim-surround
    ];
    vimAlias = true;
    withNodeJs = true;
    withPython3 = true;
    extraConfig = builtins.readFile config + "\n";
  };
}
