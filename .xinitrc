#!/usr/bin/env bash

# Load modified keys for the user
if [ -f "$HOME/.Xmodmap" ]; then
  xmodmap "$HOME/.Xmodmap"
fi
