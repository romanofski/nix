{pkgs, ...}:

{
  gtk = {
    enable = true;
    theme = {
      package = pkgs.gnome3.gnome_themes_standard;
      name = "Adwaita";
    };
  };
}
