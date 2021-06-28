{pkgs, ... }:

{
  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    # 1year
    defaultCacheTtl = 34560000;
    maxCacheTtl = 34560000;
    defaultCacheTtlSsh = 34560000;
  };
}
