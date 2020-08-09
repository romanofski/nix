{config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    yubikey-personalization
  ];

  # kudos to: https://rzetterberg.github.io/yubikey-gpg-nixos.html
  services.udev.packages = with pkgs; [
    yubikey-personalization
  ];
}
