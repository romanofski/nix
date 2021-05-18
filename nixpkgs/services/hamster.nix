{pkgs, ... }:

{
  home.packages = [
    pkgs.hamster
  ];

  systemd.user.services = {
    hamster-service = {
      Unit = { Description = "Hamster time tracker D-Bus service"; };
      Service = { ExecStart = "${pkgs.hamster}/libexec/hamster/hamster-service"; };
      Install = { WantedBy = [ "default.target" ]; };
    };
    hamster-windows-service = {
      Unit = { Description = "Hamster time tracker windows service"; };
      Service = { ExecStart = "${pkgs.hamster}/libexec/hamster/hamster-windows-service"; };
      Install = { WantedBy = [ "default.target" ]; };
    };
  };
}
