{ pkgs, ... }:

{
  programs.alacritty = {
    enable = true;
    settings = {
      general = {
        live_config_reload = true;
      };
      window = {
        opacity = 0.8;
        blur = true;
      };
      font = {
        normal = { family = "Victor Mono Medium"; style = "Regular";};
        size = 9;
      };
    };
  };
}
