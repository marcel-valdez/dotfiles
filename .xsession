#!/usr/bin/env bash

source ~/.bash_functions
function today() {
  ~/bin/today
}

echo "$(today) $(now) Executed .xsession during initialization" >> ~/.xstartup.log

# this file does not seem to be executed during cinnamon session initialization,
# so we are calling it from .xsessionrc (which does seem to be called)

type xmodmap &>/dev/null && [[ -e ~/.Xmodmap ]] &&\
  xmodmap ~/.Xmodmap >> ~/.xstartup.log
