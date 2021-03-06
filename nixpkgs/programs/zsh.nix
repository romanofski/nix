{ pkgs, ... }:

let
  secrets = import ../secrets.nix;
in with secrets; {
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    shellAliases = {
      c = "cd ..";
    };
    loginExtra = ''
      if [ -O "$HOME/.nix-profile/etc/profile.d/nix.sh" ]
      then
      . /home/rjoost/.nix-profile/etc/profile.d/nix.sh
      fi
      [ -z "$LOCALE_ARCHIVE" ] && LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive"
      if env | grep -q ^LOCALE_ARCHIVE=
      then
        echo "LOCALE_ARCHIVE already set by OS"
      else
        export LOCALE_ARCHIVE
      fi
    '';
    oh-my-zsh = {
      enable = true;
      plugins = [ "git" "sudo"];
      theme = "${zshtheme}";
    };
  };
}
