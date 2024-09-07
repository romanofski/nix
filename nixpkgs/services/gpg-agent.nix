{pkgs, ... }:

{
  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    enableZshIntegration = true;
    # see services.dbus.packages in system configuration too
    pinentryPackage = pkgs.pinentry-gnome3;
    # 1year
    defaultCacheTtl = 34560000;
    maxCacheTtl = 34560000;
    defaultCacheTtlSsh = 34560000;
  };
}
