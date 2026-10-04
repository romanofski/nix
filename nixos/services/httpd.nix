
{ config, pkgs, lib, ... }:
let
  tailscaleDomain = "mystique.kamori-gila.ts.net";
  acmeDomains = [
    "photos.home.romanofski.de"
    "ha.home.romanofski.de"
    "matter.home.romanofski.de"
    "movies.home.romanofski.de"
    "books.home.romanofski.de"
  ];

  acmeCert = domain: {
    inherit domain;
    dnsProvider = "pdns";
    environmentFile = config.sops.templates."acme.env".path;
    group = "caddy";
    reloadServices = [ "caddy.service" ];
  };

  certDir = domain: "/var/lib/acme/${domain}";
  tlsFor = domain: ''
    tls ${certDir domain}/fullchain.pem ${certDir domain}/key.pem
  '';
in {
  sops.secrets.matterjs_dashboard_pw = {};
  sops.secrets.cert_api_base_url = {};
  sops.secrets.cert_provider_key = {};

  sops.templates."caddy.env" = {
     content = ''
       MATTER_PASSWORD_HASH=${config.sops.placeholder.matterjs_dashboard_pw}
     '';
     owner = "caddy";
     group = "caddy";
     mode = "0400";
   };
  sops.templates."acme.env" = {
     content = ''
       PDNS_API_URL=${config.sops.placeholder.cert_api_base_url}
       PDNS_API_KEY=${config.sops.placeholder.cert_provider_key}
       PDNS_SERVER_NAME=localhost
     '';
     owner = "caddy";
     group = "caddy";
     mode = "0400";
   };

  networking.firewall.allowedTCPPorts = [
    443
  ];

  security.acme = {
    acceptTerms = true;

    defaults.email = "acme@romanofski.de";

    certs = lib.genAttrs acmeDomains acmeCert;
  };

  services.caddy = {
    enable = true;
    environmentFile = config.sops.templates."caddy.env".path;

    virtualHosts = {
       "ha.home.romanofski.de".extraConfig =
         tlsFor "ha.home.romanofski.de" + ''
         reverse_proxy 127.0.0.1:8123
       '';

       "matter.home.romanofski.de".extraConfig =
         tlsFor "matter.home.romanofski.de" + ''
         basic_auth {
           admin {$MATTER_PASSWORD_HASH}
         }
         reverse_proxy 127.0.0.1:5580
       '';

       "photos.home.romanofski.de".extraConfig =
         tlsFor "photos.home.romanofski.de" + ''
         reverse_proxy 127.0.0.1:2283
       '';

       "books.home.romanofski.de".extraConfig =
         tlsFor "books.home.romanofski.de" + ''
         reverse_proxy 127.0.0.1:3000
       '';

       "movies.home.romanofski.de".extraConfig =
         tlsFor "movies.home.romanofski.de" + ''
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
