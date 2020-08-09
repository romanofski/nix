#!/bin/sh
# set filetype=bash
# save stdout and stderr to file descriptors 3 and 4, then redirect them
# to "startup.log"
# http://stackoverflow.com/questions/314675/how-do-i-redirect-the-output-of-an-entire-shell-script-within-the-script-itself
exec 3>&1 4>&2 >/tmp/startup.log 2>&1

set -xe
#
# Checks if the process has already started. If not start it, otherwise
# bail silently out.
#
function save_start {
    if [ -z "$(pgrep ${1})" ]; then
        $@ &
    fi
}

save_start "feh" "-z" "--bg-fill" "Pictures/*.jpg"

# restore stdout and stderr
exec 1>&3 2>&4
