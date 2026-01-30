{ config, korrosync, ... }:

let
  statedir = "/srv/books/State";
  bookdir = "/srv/books/Repo";
in
{
  services.calibre-server = {
    enable = true;
    openFirewall = true;
    libraries = [
      bookdir
    ];
  };

  systemd.services = {
    korrosync = {
      enable = true;
      description = "KOReader Sync Server";
      serviceConfig = {
        User = config.users.users.rjoost.name;
        Group = config.users.users.rjoost.group;
        ReadWritePaths = statedir;
        ExecStart = "${korrosync}/bin/korrosync";
        Environment = [
          "KORROSYNC_DB_PATH=${statedir}/progress"
        ];
        Restart = "on-failure";
        RestartSec = "5s";
      };
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
    };
  };
}
