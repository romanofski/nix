{ pkgs, ... }:

let
  config = ./vim/config.vim;
in {
  programs.vim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [
      bufexplorer
      colors-solarized
      ctrlp-vim
      vim-airline
      vim-surround
    ];
    settings = {
      background = "dark";
      tabstop = 2;
      shiftwidth = 4;
      hidden = true;
      expandtab = true;
      smartcase = true;
    };
    extraConfig = builtins.readFile config + "\n";
  };
}
