{ pkgs, ... }:

{
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    shellAliases = {
      c = "cd ..";
    };
    oh-my-zsh = {
      enable = true;
      plugins = [ "git" "sudo"];
      theme = "pygmalion";
    };
  };
}
