#!/usr/bin/env bash

# this file does not seem to be executed during cinnamon session initialization

# Load modified keys for the user
# TODO: Only do this if the current xmodmap map does not match the one in file
if [ -f "${HOME}/.Xmodmap" ]; then
  xmodmap "${HOME}/.Xmodmap"
fi

# Load xbindkeys
# TODO: Only run xbindkeys if it is not already running or destroy any
# existing instance before running
if [ -x "/usr/bin/xbindkeys_autostart" ]; then
  /usr/bin/xbindkeys_autostart
fi
