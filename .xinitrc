#!/usr/bin/env bash

# Load modified keys for the user
if [ -f "$HOME/.Xmodmap" ]; then
  xmodmap "$HOME/.Xmodmap"
fi

# Load albert
if [ -f "/usr/bin/albert" ]; then
  /usr/bin/albert &
fi
