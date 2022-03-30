#!/usr/bin/env bash

session_name="${GDMSESSION}"
[[ -z "${session_name}" ]] && session_name="${XDG_SESSION_DESKTOP}"

function now() {
  date "+%H:%M:%S"
}

function today() {
  "${HOME}/bin/today"
}

function cmd_exists() {
  type "$1" &>/dev/null
}

function cmd_path() {
  type -p "$1"
}

function is_cmd_running() {
  pgrep -af "$1" &>/dev/null
}

echo "$(today) $(now) Executed .xsession during initialization" >> ~/.xstartup.log

# this file does not seem to be executed during cinnamon session initialization,
# so we are calling it from .xsessionrc (which does seem to be called)

cmd_exists xmodmap && [[ -e ~/.Xmodmap ]] &&\
  xmodmap ~/.Xmodmap >> ~/.xstartup.log

# Initialize logitech trackball mouse (the /usr/share/X11/xorg.conf.d/10-evdev.conf did not work)
trackball_name="Logitech USB Trackball"
if xinput list --name-only | grep "${trackball_name}" &>/dev/null; then
  xinput set-prop "${trackball_name}" "libinput Left Handed Enabled" 1
  xinput set-prop "${trackball_name}" "libinput Scroll Method Enabled" 0, 0, 1
  xinput set-prop "${trackball_name}" "libinput Button Scrolling Button" 9
#  xinput set-button-map "${trackball_name}" 3 8 1 4 5 6 7 2 2
fi

if cmd_exists conky && ! is_cmd_running conky; then
  "${HOME}/bin/start-conky"
fi

if [[ "${session_name}" == "xfce" ]]; then
  if [[ -x "${HOME}/.xss-lock" ]]; then
    if ! is_cmd_running xss-lock; then
      "${HOME}/.xss-lock" &>/dev/null &
    fi
    xfconf-query --channel "xfce4-session" --property "/general/LockCommand" \
          --set "xset s activate"
  else
    # configure X secure lock
    if cmd_exists xsecurelock; then
      if cmd_exists xscreensaver; then
        export XSECURELOCK_SAVER=saver_xscreensaver
      fi

      export XSECURELOCK_PASSWORD_PROMPT=disco
      export XSECURELOCK_FONT='Noto Serif CJK JP'
      export XSECURELOCK_SHOW_DATETIME=1
      export XSECURELOCK_SINGLE_AUTH_WINDOW=1
      export XSECURELOCK_AUTH_TIMEOUT=30

      locker_cmd=$(xfconf-query --channel "xfce4-session" --property "/general/LockCommand")
      if [[ "${locker_cmd}" != "xsecurelock" ]]; then
        xfconf-query --channel "xfce4-session" --property "/general/LockCommand" \
          --set "xsecurelock"
      fi
    fi
  fi

  # configure the X screen saver
  if cmd_exists xscreensaver && ! is_cmd_running xscreensaver; then
    xscreensaver &
  fi
fi
