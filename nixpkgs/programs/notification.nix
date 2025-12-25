{ pkgs, ... }:

{

  services.mako = {
    enable = true;
    settings = {
      actions = true;
      anchor = "top-right";
      background-color = "#303446";
      text-color = "#c6d0f5";
      border-color = "#babbf1";
      progress-color = "over #414559";

      "urgency=high" = {
        border-color = "#ef9f76";
        default-timeout = 0;
      };
      "urgency=normal" = {
        default-timeout = 10;
      };
      "urgency=low" = {
        default-timeout = 5;
      };

      border-radius = 0;
      default-timeout = 0;
      font = "monospace 10";
      height = 100;
      width = 300;
      icons = true;
      ignore-timeout = false;
      layer = "top";
      margin = 10;
      markup = true;

      # Section example
      "actionable=true" = {
        anchor = "top-left";
      };
    };
  };
}
