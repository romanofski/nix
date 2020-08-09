{ pkgs, ... }:

{
  services.dunst = {
    enable = true;
    iconTheme = {
      package = pkgs.gnome3.gnome_themes_standard;
      name = "Adwaita";
    };
    settings = {
      global = {
        geometry = "500x5-30+50";
        transparency = 10;
        font = "Roboto 13";
        padding = 15;
        horizontal_padding = 17;
        word_wrap = true;
        follow = "keyboard";
        format = "%s %p %I\n%b";
        markup = "full";
      };

      urgency_low = {
        timeout = 5;
      };

      urgency_normal = {
        timeout = 10;
      };

      urgency_critical = {
        timeout = 15;
      };
    };
  };
}
