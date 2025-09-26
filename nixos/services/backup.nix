{ config, pkgs, ... }:
{
  services.borgbackup = {
    jobs.immichBackup = {
      repo = "/mnt/immich-borg";
      encryption.mode = "none";
      paths = [
        "/var/lib/immich/library"
        "/var/lib/immich/upload"
        "/var/lib/immich/profile"
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

  # Trigger backup after the disk is plugged in
  # The names are hardcoded - perhaps there is a better way to refer
  # to those mount points but I couldn't come up with a better
  # solution for now.
  systemd.services."trigger-immich-backup-onplugin" = {
    wantedBy = [ "mnt-immich\\x2dborg.mount" ];
    after = [ "mnt-immich\\x2borg.mount" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${pkgs.systemd}/bin/systemctl start borgbackup-job-immichBackup.service";
    };
  };
}
