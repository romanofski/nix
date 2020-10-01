{ pkgs, ... }:

{
  programs.autorandr = {
    enable = true;
    profiles = {
      "docked" = {
        fingerprint = {
          DP2-2 = "00ffffffffffff00220e703400000000341d0104a53c22783a4815a756529c270f5054a10800d1c081c0a9c0b3009500810081800101023a801871382d40582c45";
          DP2-3 = "00ffffffffffff00220e703400000000101e0104a53c22783a4815a756529c270f5054a10800d1c081c0a9c0b3009500810081800101023a801871382d40582c45";
          eDP1 = "00ffffffffffff000daec91400000000081a0104951f11780228659759548e271e505400000001010101010101010101010101010101b43b804a71383440503c680";
        };
        config = {
          eDP1.enable = false;
          DP2-2 = {
            enable = true;
            primary = true;
            position = "1920x0";
            mode = "1920x1080";
            rate = "60.00";
          }; 
          DP2-3 = {
            enable = true;
            position = "3840x0";
            mode = "1920x1080";
            rate = "60.00";
          };
        };
      };
    };
  };
}
