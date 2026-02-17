{ secrets, pkgs, useGCM ? false }:

let
  gcm = {
    credential = {
      helper = "/mnt/c/Program\\ Files/Git/mingw64/bin/git-credential-manager.exe";
    };
  };
  settings = {
    aliases = {
      lg = "log --graph --pretty=format:'%Cred%h%Creset - %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative";
      ll = "!git st; git --no-pager log --pretty='format:%Cred%cr %Cblue%h %Cgreen%an:%Creset %s%C(yellow)%d' -n20; echo";
      # Nice list of branches by age
      bb = "for-each-ref --sort=-committerdate --format='%1B[36m%(committerdate:relative) %1B[33m%(refname:short)%1B[0;m' refs/heads/";
      info = "config --list";
    };
    user = {
      email = "${secrets.email}";
      name = "${secrets.realName}";
    };
    http.version = "HTTP/1.1";
  };
in
{
  enable = true;
  ignores = [
    "dist"
    ".cabal-sandbox"
    "cabal.sandbox.config"
    ".stack-work"
    "*.o"
    "*.hi"
  ];
  signing = {
    key = "D02BC6E095A0446267E1F43C0133D0C73A765B52";
  };
  settings = if useGCM then settings // gcm else settings;
  lfs.enable = true;
}

