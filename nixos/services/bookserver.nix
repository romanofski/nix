{ config, korrosync, ... }:

let
  statedir = "/srv/books/State";
  bookdir = "/srv/books/Repo";
in
{
  sops.secrets.bookorbit_jwt_secret = {
    owner = config.services.bookorbit.user;
    group = config.services.bookorbit.group;
    mode = "0400";
  };
  sops.secrets.bookorbit_setup_bootstrap_token = {
    owner = config.services.bookorbit.user;
    group = config.services.bookorbit.group;
    mode = "0400";
  };
  sops.templates."bookorbit.env".content = ''
    JWT_SECRET=${config.sops.placeholder.bookorbit_jwt_secret}
    SETUP_BOOTSTRAP_TOKEN=${config.sops.placeholder.bookorbit_setup_bootstrap_token}
    BOOKS_HOST_PATH=${bookdir}
  '';

  services.bookorbit = {
    enable = true;
    openFirewall = true;
    environmentFile = config.sops.templates."bookorbit.env".path;
  };
}
