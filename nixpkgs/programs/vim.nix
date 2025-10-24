{ pkgs, ... }:

let
  config = ./vim/config.vim;
  luaConfig = ./vim/luaConfig.lua;
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
    extraLuaConfig = builtins.readFile luaConfig;
    # extraConfig = builtins.readFile config + "\n";
  };
}
