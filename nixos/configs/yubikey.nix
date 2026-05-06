{config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    pam_u2f
  ];

  # kudos to: https://rzetterberg.github.io/yubikey-gpg-nixos.html
  services.udev.packages = with pkgs; [
    yubiky-personalization
  ];
  security.pam.services = {
    login.u2fAuth = true;
    sudo.u2fAuth = true;
  };
}
