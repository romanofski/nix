{ pkgs, ... }:
{
  services.borgbackup = {
    jobs = {
      immichBackup = {
        doInit = false;
        repo = "/mnt/immich-borg";
        encryption.mode = "none";
        paths = [
          "/srv/images/library"
          "/srv/images/upload"
          "/srv/images/profile"
          "/srv/images/backups"
        ];
        prune.keep = {
          daily = 7;
          weekly = 4;
          monthly = 6;
        };

        startAt = "daily";

        compression = "lz4";
      };
    };
  };

  # Require source and destination mounts before running backups
  systemd.services."borgbackup-job-immichBackup" = {
    onFailure = [ "immich-backup-failure-notify.service" ];
    unitConfig.RequiresMountsFor = [
      "/srv/images"
      "/mnt/immich-borg"
    ];
  };

  systemd.services.immich-backup-failure-notify = {
    description = "Publish Immich backup failure notification";
    wants = [ "mosquitto.service" ];
    after = [ "mosquitto.service" ];

    serviceConfig = {
      Type = "oneshot";
      DynamicUser = true;
      NoNewPrivileges = true;
      ProtectSystem = "strict";
      ProtectHome = true;
      PrivateTmp = true;
      TimeoutStartSec = "30";
    };

    script = ''
      ${pkgs.mosquitto}/bin/mosquitto_pub \
      -h 127.0.0.1 \
      -p 1883 \
      -q 1 \
      -t 'home/backup/immich/failure' \
      -m "Yoga: the Immich backup job failed. Check: journalctl -u borgbackup-job-immichBackup.service"
    '';
  };
}
