#!/usr/bin/env bash

source ~/.bash_functions
function today() {
  ~/bin/today
}

# this file does not seem to be executed during cinnamon session initialization
# so we are calling it from .xprofile
echo "$(today) $(now) Executed .xinitrc during initialization" >> ~/.xstartup.log
# here we should set environment variables needed by the X environment BEFORE
# there is a windowing environment available
