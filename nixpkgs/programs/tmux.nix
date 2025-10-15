{pkgs, ...}:

{
  programs.tmux = {
    enable = true;
    prefix = "C-a";
    shell = "${pkgs.zsh}/bin/zsh";
    keyMode = "vi";
    extraConfig = ''
unbind " "
bind " " next-window
unbind BSpace
bind BSpace previous-window
    '';
    plugins = with pkgs; [
      tmuxPlugins.tmux-powerline
               ];
  };
}
