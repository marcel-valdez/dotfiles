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

# Initialize logitech trackball mouse (the /usr/share/X11/xorg.conf.d/10-evdev.conf did not work)
trackball_name="Logitech USB Trackball"
if xinput list --name-only | grep "${trackball_name}" &>/dev/null; then
  xinput set-prop "${trackball_name}" "Evdev Wheel Emulation Button" 9
  xinput set-prop "${trackball_name}" "Evdev Wheel Emulation" 1
  xinput set-prop "${trackball_name}" "EmulateWheel" "true"
  xinput set-button-map "${trackball_name}" 3 8 1 4 5 6 7 2 2
fi
