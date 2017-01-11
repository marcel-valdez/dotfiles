#!/usr/bin/env bash

# this file does not seem to be executed during cinnamon session initialization

if [ -f "${HOME}/.xinitrc" ]; then
  source "${HOME}/.xinitrc"
fi
