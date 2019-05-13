{ pkgs, ... }:

let
  config = ./vim/config.vim;
in {
  programs.vim.enable = true;
  programs.vim.plugins = [
    "bufexplorer"
    "colors-solarized"
    "ctrlp-vim"
    "vim-airline"
  ];
  programs.vim.settings = {
    background = "dark";
    tabstop = 2;
    shiftwidth = 4;
    hidden = true;
    expandtab = true;
    smartcase = true;
  };
  programs.vim.extraConfig = builtins.readFile config + "\n";
}
