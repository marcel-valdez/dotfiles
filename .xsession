#!/usr/bin/env bash

# this file does not seem to be executed during cinnamon session initialization,
# so we are calling it from .xsessionrc (which does seem to be called)
echo "Executed .xsession during initialization" >> ~/.xstartup.log

type xmodmap &>/dev/null && [ -e ~/.Xmodmap ] && xmodmap ~/.Xmodmap
