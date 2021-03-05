{ pkgs, ... }:

{
  programs.offlineimap = {
    enable = true;
    pythonFile = ''
      import subprocess

      def get_pass(service, cmd):
          return subprocess.check_output(cmd, ).strip()
    '';
  };

  systemd.user.services.offlineimap = {
    Unit = { Description = "offlineimap sync"; };
    Service = {
      ExecStart = "${pkgs.offlineimap}/bin/offlineimap -u basic -o";
    };
  };
  systemd.user.timers.offlineimap = {
    Unit = {
      Description = "offlineimap sync timer";
    };
    Timer = {
      OnCalendar = "*:0/5";
      Unit = "offlineimap.service";
    };
    Install = {
      WantedBy = ["timers.target"];
    };
  };
}

