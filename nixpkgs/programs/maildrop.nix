{secrets, pkgs, config, ...}:

let
  regexEscape = s:
  builtins.replaceStrings
  [ "\\" "." "+" "*" "?" "^" "$" "(" ")" "[" "]" "{" "}" "|" ]
  [ "\\\\" "\\." "\\+" "\\*" "\\?" "\\^" "\\$" "\\(" "\\)" "\\[" "\\]" "\\{" "\\}" "\\|" ]
  s;
  mkListRegexp = xs: builtins.concatStringsSep "|" (map regexEscape xs);
  whitelistRe = if secrets.mailfilter.whitelist == [] then "(?!)"
  else "${mkListRegexp secrets.mailfilter.whitelist}";
  blacklistRe = if secrets.mailfilter.blacklist == [] then "(?!)"
  else "${mkListRegexp secrets.mailfilter.blacklist}";
in {
  home.file.".maildrop/variables.inc" = {
    text = ''
      WHITELIST="(${whitelistRe})"
      BLACKLIST="(${blacklistRe})"
    '';
  };
  home.file.".HMmailfilter" = {
    text = ''
      import PATH
      DEFAULT="$HOME/Maildir"
      PATH=$HOME/.nix-profile/bin:$HOME/.local/bin:$PATH
      NOTMUCH_CONFIG="$HOME/.config/notmuch/notmuchrc"

      include "$HOME/.maildrop/variables.inc"

      logfile "$HOME/.maildrop/mailfilter.log"
      log "---- new message ----"

      if ( /^X-getmail-retrieved-from-mailbox:[[:space:]]*(Junk|Spam|Trash)[[:space:]]*$/ )
      {
      log "Matched Trash mailbox"
      to "| notmuch insert +spam"
      exit
      }
      if (/^To:$/:H)
      {
      log "Matched empty To:"
      to "| notmuch insert +spam -inbox"
      exit
      }

      if (/^From:.*@eq.edu.au$/:H)
      {
      log "Matched school sender"
      to "| notmuch insert +inbox +school"
      exit
      }

      # anything clearly labelled as spam
      if ( /^X-Spam:[[:space:]]*Yes$/:H )
      {
      log "Matched X-Spam"
      to "| notmuch insert +spam"
      exit
      }


      # Anything addressed to these go right in the bin
      if ( /^To:.*$BLACKLIST/:H || /^Cc:.*$BLACKLIST/:H || /^Delivered-To:.*$BLACKLIST/:H \
      || /^X-Original-To:.*$BLACKLIST/:H || /^Envelope-To:.*$BLACKLIST/:H )
      {
      log "Matched blacklist"
      to "| notmuch insert +spam +inbox"
      exit
      }

      # whitelisted addresses - anything there we don't treat as spam
      if ( /^To:.*$WHITELIST/:H )
      {
      log "Matched whitelist"
      to "| notmuch insert +inbox"
      exit
      }
      else
      {
      log "Didn't match whitelist"
      to "| notmuch insert +spam +inbox"
      exit
      }

      to  "| notmuch insert"
    '';
    # https://github.com/nix-community/home-manager/issues/3090
    onChange = ''
      rm -f ${config.home.homeDirectory}/.mailfilter
      cp ${config.home.homeDirectory}/.HMmailfilter ${config.home.homeDirectory}/.mailfilter
      chmod 600 ${config.home.homeDirectory}/.mailfilter
    '';
  };
}
