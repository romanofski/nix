
{ pkgs,config,secrets, ... }:

 {
    # RTL2832
    hardware.rtl-sdr.enable = true;
    boot.blacklistedKernelModules = [ "dvb_usb_rtl28xxu" ];
    environment.systemPackages = with pkgs; [
      rtl_433
    ];

    # First, the rtl_433 config file
    environment.etc."rtl_433/rtl_433.conf".text = ''
      frequency 433.92M
      convert si
      report_meta time:iso:tz:local

      output mqtt://localhost:1883,retain=1,events=rtl_433[/model][/id]
    '';

    # The systemd service
    systemd.services.rtl_433 = {
      description = "rtl_433 SDR receiver";
      after = [ "network.target" "mosquitto.service" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        ExecStart = "${pkgs.rtl_433}/bin/rtl_433 -c /etc/rtl_433/rtl_433.conf -F log";
        Restart = "on-failure";
        RestartSec = "10s";
        SupplementaryGroups = [ "plugdev" ];
      };
    };

  }
