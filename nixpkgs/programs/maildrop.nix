{secrets, pkgs, config, ...}:

let
  mapped = builtins.map (x: "hasaddr(${x})") secrets.mailfilter.whitelist;
  whitelist = builtins.concatStringsSep " || " mapped;
in {
  home.file.".HMmailfilter" = {
    text = ''
      import PATH
      DEFAULT="$HOME/Maildir"
      PATH=$HOME/.nix-profile/bin:$HOME/.local/bin:$PATH
      NOTMUCH_CONFIG="$HOME/.config/notmuch/notmuchrc"

      if (/^(From|To):.*@gnome.org/:h)
      {
      to "| notmuch insert +list -inbox"
      }

      # Handle spam
      if (/^From: messages-noreply@linkedin.com/:H)
      {
      to "| notmuch insert +spam -inbox"
      }
      if (/^X-getmail-retrieved-from-mailbox:.*Trash$/)
      {
      to "| notmuch insert +spam +inbox"
      }
      if (/^To: onitburger@bromeco.de$/:H)
      {
      to "| notmuch insert +spam -inbox"
      }
      if (/^To:$/:H)
      {
      to "| notmuch insert +spam -inbox"
      }

      if (/^From:.*@eq.edu.au$/:H)
      {
      to "| notmuch insert +inbox +school"
      }

      if (/^To:.*romanofski.de$/:H)
      {
      to "| notmuch insert +spam -inbox"
      }

      if (/^To:.*goodness@bromeco.de$/:H || /^To: roman+jw@bromeco.de$/:H )
      {
      to "| notmuch insert +spam"
      }
      if (/^To:.*undisclosed-recipients.*/:H)
      {
      to "| notmuch insert +spam"
      }

      # anything not addressed to me
      if ( ! (${whitelist}) )
      {
      to "| notmuch insert +spam"
      }

      # anything clearly labelled as spam
      if (/^X-Spam:?Yes$/:H)
      {
      to "| notmuch insert +spam"
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
