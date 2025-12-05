{ pkgs, ... }:

{
  home.packages = [
    pkgs.victor-mono
  ];
  programs.alacritty = {
    enable = true;
    theme = "solarized_dark";
    settings = {
      general = {
        live_config_reload = true;
      };
      font = {
        normal = { family = "Victor Mono Medium"; style = "Regular";};
        size = 9;
      };
    };
  };
}
