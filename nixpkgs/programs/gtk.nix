{pkgs, ...}:

{
  gtk = {
    enable = true;
    theme = {
      package = pkgs.gnome3.gnome-themes-extra;
      name = "Adwaita";
    };
  };
}
