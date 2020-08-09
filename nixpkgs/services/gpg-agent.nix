{pkgs, ... }:

{
  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    # 2h
    defaultCacheTtl = 7200;
    defaultCacheTtlSsh = 7200;
  };
}
