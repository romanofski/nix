{ secrets, pkgs, ... }:

{

  nix = {
    package = pkgs.nix;
    settings.experimental-features = ["nix-command" "flakes"];
  };

  imports = [
    ./programs/vim.nix
    ./programs/zsh.nix
    ./programs/tmux.nix
  ];

  programs.git = import ./programs/git.nix { secrets = secrets; pkgs = pkgs; useGCM =true; };

  nixpkgs.config.allowUnfree = true;
  home.stateVersion = "23.05"; # Please read the comment before changing.


  home.packages = with pkgs; [
    bzip2
    elinks
    feh
    gcc
    git-lfs
    glibcLocales
    dig
  ];

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "rjoost";
  home.homeDirectory = "/home/rjoost";

  fonts.fontconfig = {
    enable = true;
  };

  accounts.email = {
    accounts = {
      bromeco = {
        realName = "${secrets.realName}";
        address = "${secrets.email}";
        userName = "${secrets.email}";
        primary = true;
        flavor = "plain";
        smtp = {
          host = secrets.mailhost;
          tls.enable = true;
          tls.useStartTls = true;
        };
        passwordCommand = "${pkgs.pass}/bin/pass flyingcircus-bromeco/roman@bromeco-password";
        imap = {
          host = secrets.mailhost;
          tls = {
            enable = true;
            useStartTls = true;
          };
        };
        msmtp = {
          enable = true;
        };
        notmuch = {
          enable = true;
        };
        getmail = {
          enable = true;
          mailboxes = ["ALL"];
          destinationCommand = "${pkgs.maildrop}/bin/maildrop";
          delete = true;
        };
      };
    };
  };

  home.sessionVariables = {
    EDITOR = "vim";
  };

  programs.home-manager = {
    enable = true;
  };
  programs.msmtp = {
    enable = true;
    extraConfig = ''
      defaults
      syslog on
      domain bromeco.de
      port 587
      tls_starttls on
    '';
  };
}
