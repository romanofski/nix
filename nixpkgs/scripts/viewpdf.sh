#!/bin/bash

if [ -n "$SSH_CONNECTION" ]; then
    # When connected remotely
    exec your-remote-pdf-viewer "$@"
else
    # When locally on Wayland
    xdg-open "$@"
fi
