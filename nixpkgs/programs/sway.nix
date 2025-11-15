
{ pkgs, ... }:

{
  programs.alacritty.enable = true; # Super+T in the default setting (terminal)
  programs.fuzzel.enable = true; # Super+D in the default setting (app launcher)
  services.polkit-gnome.enable = true; # polkit
  programs.swaylock.enable = true; # Super+Alt+L in the default setting (screen locker)
  services.mako.enable = true; # notification daemon
  services.swayidle.enable = true; # idle management daemon
  wayland.windowManager.sway = {
    enable = true;
    config = {
      bars = [];
      fonts = { names = [ "Fira Code Medium" ]; size = 14.0;};
      gaps = { outer = 0; inner = 10; };
      output = { "eDP-1" = {
        # background = "$wallpaper fill";
      }; };
      left = "h";
      down = "n";
      up = "e";
      right = "i";
      modifier = "Alt";
      keybindings =
        {
          "Alt+j" = "workspace 1";
          "Alt+l" = "workspace 2";
          "Alt+u" = "workspace 3";
          "Alt+y" = "workspace 4";
          "Alt+Shift+j" = "move container to workspace number 1";
          "Alt+Shift+l" = "move container to workspace number 2";
          "Alt+Shift+u" = "move container to workspace number 3";
          "Alt+Shift+y" = "move container to workspace number 4";
          "Alt+t" = "exec alacritty";
          "Alt+c" = "clipmenu";
          "Alt+Shift+c" = "kill";
          "Alt+space" = "exec ${pkgs.rofi}/bin/rofi -show drun";
          # "Alt+y" = "output eDP-1 disable";
          # "Alt+Shift+y" = "output eDP-1 enable";
          "Alt+n" = "workspace next";
          "Alt+e" = "workspace prev";
          "Alt+Shift+q" = "exit";
          "Alt+Shift+r" = "restart";
          "Alt+p" = "focus parent";
          "Alt+Shift+p" = "focus child";
          "Mod4+h" = "focus left";
          "Mod4+n" = "focus down";
          "Mod4+e" = "focus up";
          "Mod4+i" = "focus right";
          "Alt+r" = "mode resize";
          "Alt+g" = "mode gaps";
          "Alt+o" = "exec emacsclient --eval '(org-clock-in-last)'";
          "Alt+Shift+o" = "exec emacsclient --eval '(org-clock-out)'";
          "Alt+Shift+e" = "exec emacsclient -c";
          "Mod4+Shift+h" = "move left";
          "Mod4+Shift+n" = "move down";
          "Mod4+Shift+e" = "move up";
          "Mod4+Shift+i" = "move right";
          "Alt+s" = "move scratchpad";
          "Alt+Shift+s" = "scratchpad show";
          "Alt+f" = "fullscreen toggle";
          "Alt+x" = "layout toggle all";
          "Alt+v" = "split v";
          "Alt+Shift+v" = "split h";
          "XF86MonBrightnessUp" = "exec ${pkgs.brightnessctl}/bin/brightnessctl set '+10%'";
          "XF86MonBrightnessDown" = "exec ${pkgs.brightnessctl}/bin/brightnessctl set '10%-'";
          "XF86AudioRaiseVolume" =  "exec --no-startup-id ${pkgs.pulseaudio-ctl}/bin/pulseaudio-ctl up";
          "XF86AudioLowerVolume" =  "exec --no-startup-id ${pkgs.pulseaudio-ctl}/bin/pulseaudio-ctl down";
          "XF86AudioMute" =  "exec --no-startup-id ${pkgs.pulseaudio-ctl}/bin/pulseaudio-ctl mute";
        };
        modes = {
          resize = {
            h = "resize shrink width 2";
            n = "resize grow height 2";
            e = "resize shrink height 2";
            i = "resize grow width 2";
            Escape = "mode default";
          };
          gaps = {
            h = "gaps inner current set 0";
            n = "gaps inner current minus 5";
            e = "gaps inner current plus 5";
            i = "gaps inner current set 50";
            Escape = "mode default";
          };
        };
        startup = [
          # Fix for slow GTK applications; see https://github.com/swaywm/sway/wiki#gtk-applications-take-20-seconds-to-start
          { command = "exec systemctl --user import-environment"; }
          { command = "exec dbus-update-activation-environment --systemd DISPLAY WAYLAND_DISPLAY SWAYSOCK"; }
          { command = "exec swaybg -i ~/Pictures/Perspective_view_of_Korolev_crater.jpg -o '*' -m fill"; }
          { command = "exec waybar"; }
          { command = "exec ${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1"; }
        ];
        window.border = 10;
    };
    extraConfig = ''
      set $wallpaper /home/rjoost/Pictures/Perspective_view_of_Korolev_crater.jpg

      set $background #2E3440
      set $foreground #E5E9F0

      set $color0 #2E3440
      set $color1 #88C0D0
      set $color2 #BF616A
      set $color3 #5E81AC
      set $color4 #EBCB8B
      set $color5 #A3BE8C
      set $color6 #D08770
      set $color7 #E5E9F0
      set $color8 #4C566A
      set $color9 #88C0D0
      set $color10 #BF616A
      set $color11 #5E81AC
      set $color12 #EBCB8B
      set $color13 #A3BE8C
      set $color14 #D08770
      set $color15 #8FBCBB

      client.focused $color0 $background $foreground $color7 $background

      # Clamshell mode
      set $laptop eDP-1
      bindswitch --reload --locked lid:on output $laptop disable
      bindswitch --reload --locked lid:off output $laptop enable
      # Touchpad gestures
      bindgesture swipe:right workspace prev
      bindgesture swipe:left workspace next
      bindgesture pinch:inward+up move up
      bindgesture pinch:inward+down move down
      bindgesture pinch:inward+left move left
      bindgesture pinch:inward+right move right
    '';
  };
}
