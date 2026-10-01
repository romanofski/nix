
{ config, pkgs, ... }:
let
  tailscaleDomain = "mystique.kamori-gila.ts.net";
in {
  sops.secrets.matterjs_dashboard_pw = {};

  sops.templates."caddy.env" = {
     content = ''
       MATTER_PASSWORD_HASH=${config.sops.placeholder.matterjs_dashboard_pw}
     '';
     owner = "caddy";
     group = "caddy";
     mode = "0400";
   };

  networking.firewall.allowedTCPPorts = [
    443
  ];
  services.caddy = {
    enable = true;
    environmentFile = config.sops.templates."caddy.env".path;

    virtualHosts = {
       "ha.home.arpa".extraConfig = ''
         tls internal
         reverse_proxy 127.0.0.1:8123
       '';

       "matter.home.arpa".extraConfig = ''
         tls internal
         basic_auth {
           admin {$MATTER_PASSWORD_HASH}
         }
         reverse_proxy 127.0.0.1:5580
       '';

       "photos.home.arpa".extraConfig = ''
         tls internal
         reverse_proxy 127.0.0.1:2283
       '';

       "books.home.arpa".extraConfig = ''
         tls internal
         reverse_proxy 127.0.0.1:3000
       '';

       "movies.home.arpa".extraConfig = ''
         tls internal
         reverse_proxy 127.0.0.1:8096
       '';

      "${tailscaleDomain}".extraConfig = ''
        handle                                  {
        reverse_proxy http://localhost:8123
        }
      '';
    };
  };

}
