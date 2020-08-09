{ pkgs, ... }:

{
  home.file.".ghc/ghci.conf".text = ''
    :set prompt "λ: "
    :set -XOverloadedStrings
  '';
}
