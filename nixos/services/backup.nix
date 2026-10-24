{ ... }:
{
  services.borgbackup = {
    jobs = {
      immichBackup = {
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
    unitConfig.RequiresMountsFor = [
      "/srv/images"
      "/mnt/immich-borg"
    ];
  };
}
