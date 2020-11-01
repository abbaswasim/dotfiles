#!/bin/sh

# Check if emacs is running
if ! (pgrep -U $(id -u) -i emacs > /dev/null) then
   emacs "$@" & # Start emacs if not running already
   exit
fi

emacsclient --no-wait --alternate-editor "" "$@"
