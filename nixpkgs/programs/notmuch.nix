{ pkgs, ... }:

{
  programs.notmuch = {
    enable = true;
    new = {
      tags=["unread" "inbox"];
    };
    search={
      excludeTags=["deleted" "spam"];
    };
    maildir = {
      synchronizeFlags=false;
    };
  };
}
