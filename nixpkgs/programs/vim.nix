{ pkgs, ... }:

let
  config = ./vim/config.vim;
in {
  programs.neovim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [
      coc-nvim
      nvim-treesitter
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
