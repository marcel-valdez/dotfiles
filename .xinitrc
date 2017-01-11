#!/usr/bin/env bash

# this file does not seem to be executed during cinnamon session initialization

# Load modified keys for the user
if [ -f "${HOME}/.Xmodmap" ]; then
  xmodmap "${HOME}/.Xmodmap"
fi
# Load xbindkeys
if [ -x "/usr/bin/xbindkeys_autostart" ]; then
  /usr/bin/xbindkeys_autostart
fi
