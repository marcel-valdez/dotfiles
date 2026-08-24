#!/usr/bin/env bash

# File used to track whether Zen Mode is active
if ! [[ -d "${HOME}/.cache" ]]; then
  mkdir "${HOME}/.cache"
fi

STATE_FILE="${HOME}/.cache/zen_mode_enabled"
MIN_LIST_FILE="${HOME}/.cache/zen_mode_minimized_wins"
ICON_STYLE_FILE="${HOME}/.cache/zen_mode_icon_style"

# 1. Ensure we are running XFCE
if [ "${XDG_CURRENT_DESKTOP}" != "XFCE" ]; then
    echo "Zen Mode Error: XFCE desktop not detected."
    # We don't use notify-send here in case the notification daemon isn't standard
    exit 1
fi

# 2. Ensure we are running in X11 (not Wayland)
if [ "${XDG_SESSION_TYPE}" = "wayland" ] || [ -n "${WAYLAND_DISPLAY}" ]; then
    echo "Zen Mode Error: Wayland detected. This script requires X11."
    notify-send "Zen Mode Error" "Wayland detected. This script requires X11." -u critical
    exit 1
fi

# 3. Ensure xfce4-panel is being used
# We only enforce this when turning Zen Mode ON, since Zen Mode itself kills the panel.
if [ ! -f "${STATE_FILE}" ] && ! pgrep -x "xfce4-panel" >/dev/null; then
    echo "Zen Mode Error: xfce4-panel is not running."
    notify-send "Zen Mode Error" "xfce4-panel is not running." -u critical
    exit 1
fi

if [ ! -f "${STATE_FILE}" ]; then
    # ==========================================
    # ENABLE ZEN MODE
    # ==========================================
    touch "${STATE_FILE}"

    # 2. Show the "Enabled" Notification
    # -t 1000 tells it to expire in 1000ms, though DND will silence it anyway
    notify-send "Zen Mode" "Zen mode is enabled" -t 1000 -i preferences-desktop-screensaver
    
    # 3. Set Desktop Background to Solid Black
    #for prop in $(xfconf-query -c xfce4-desktop -l | grep 'image-show'); do
    #    xfconf-query -c xfce4-desktop -p "${prop}" -s false 2>/dev/null
    #done
    #for prop in $(xfconf-query -c xfce4-desktop -l | grep 'color-style'); do
    #    xfconf-query -c xfce4-desktop -p "${prop}" -s 0 2>/dev/null
    #done
    #for prop in $(xfconf-query -c xfce4-desktop -l | grep 'rgba1'); do
    #    xfconf-query -c xfce4-desktop -p "${prop}" -s "(0.000000, 0.000000, 0.000000, 1.000000)" 2>/dev/null
    #done

    # 4. Hide Top Bar / Panel
    xfce4-panel --quit 2>/dev/null

    # 5. Minimize All Other Windows on Current Workspace
    if command -v wmctrl &>/dev/null && command -v xdotool &>/dev/null; then
      ACTIVE_WIN_DEC=$(xdotool getactivewindow 2>/dev/null)
      ACTIVE_HEX=""
      if [ -n "${ACTIVE_WIN_DEC}" ]; then
        ACTIVE_HEX=$(printf "0x%08x" "${ACTIVE_WIN_DEC}")
      fi

      CURRENT_DESK=$(wmctrl -d | awk '/\*/ {print $1}')
      > "${MIN_LIST_FILE}"

      wmctrl -l | while read -r win_id desk host title; do
        if [ "${desk}" -eq "${CURRENT_DESK}" ]; then
          if [ "${win_id,,}" != "${ACTIVE_HEX,,}" ]; then
            if ! xprop -id "${win_id}" _NET_WM_STATE 2>/dev/null | grep -q "_NET_WM_STATE_HIDDEN"; then
              echo "${win_id}" >> "${MIN_LIST_FILE}"
              wmctrl -i -r "${win_id}" -b add,hidden
            fi
          fi
        fi
      done

      # Fullscreen the active window
      #wmctrl -r :ACTIVE: -b add,fullscreen
    fi


    # 5. Hide Desktop Icons Natively
    # Save current style (default to 2 if empty)
    ICON_STYLE=$(xfconf-query -c xfce4-desktop -p /desktop-icons/style 2>/dev/null)
    [ -z "${ICON_STYLE}" ] && ICON_STYLE=2
    echo "${ICON_STYLE}" > "${ICON_STYLE_FILE}"
    # Set to 0 (None). If it fails because it doesn't exist, force-create it (-n -t int).
    if ! xfconf-query -c xfce4-desktop -p /desktop-icons/style -s 0 2>/dev/null; then
      xfconf-query -c xfce4-desktop -p /desktop-icons/style -n -t int -s 0 2>/dev/null
    else
      xfconf-query -c xfce4-desktop -p /desktop-icons/style -s 0 2>/dev/null
    fi

    # 1. Enable Do Not Disturb (silence notifications)
    sleep 1
    xfconf-query -c xfce4-notifyd -p /do-not-disturb -s true 2>/dev/null

else
    # ==========================================
    # DISABLE ZEN MODE (RESTORE DEFAULT)
    # ==========================================
    rm -f "${STATE_FILE}"

    # 1. Disable Do Not Disturb
    xfconf-query -c xfce4-notifyd -p /do-not-disturb -s false 2>/dev/null

    # 2. Restore Desktop Icons
    if [ -f "$ICON_STYLE_FILE" ]; then
        STYLE=$(cat "$ICON_STYLE_FILE")
        xfconf-query -c xfce4-desktop -p /desktop-icons/style -s "$STYLE" 2>/dev/null
        rm -f "$ICON_STYLE_FILE"
    fi

    # 3. Restore Wallpaper
    #for prop in $(xfconf-query -c xfce4-desktop -l | grep 'image-show'); do
    #    xfconf-query -c xfce4-desktop -p "${prop}" -s true 2>/dev/null
    #done

    # 4. Relaunch XFCE Panel
    xfce4-panel &>/dev/null &

    # Give XFCE a split second to draw the UI before un-minimizing 
    sleep 0.25

    # 5. Restore Windows
    if command -v wmctrl &>/dev/null; then
        # Remove fullscreen state from current active window
        # wmctrl -r :ACTIVE: -b remove,fullscreen

        # Restore only the windows that were minimized by this script
        if [ -f "${MIN_LIST_FILE}" ]; then
            while read -r win_id; do
                [ -n "${win_id}" ] && wmctrl -i -r "${win_id}" -b remove,hidden
            done < "${MIN_LIST_FILE}"
            rm -f "${MIN_LIST_FILE}"
        fi
    fi

    # 4. Show the "Disabled" Notification
    notify-send "Zen Mode" "Zen mode is disabled" -t 2000 -i preferences-desktop-screensaver
fi
