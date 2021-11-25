#!/usr/bin/env bash

SCRIPT="$(basename $0)"

window_name=
poll_frequency_seq=
declare -a refresh_script

function usage() {
  cat<<EOF

${SCRIPT} [--help|-h] [--window_name|-w <name>] [--freq|-f <seconds>] [--custom_message|-m <message>] -- refresh_script_args ...

This is a script to monitor the contents of a tmux window. If the contents change a popup will be shown on the tmux client with a message.

help: Displays this help message
window_name: The name of the window to monitor.
custom_message: A custom message to show in the popup dialog. Default: Window <window_name> changed!
freq: Frequency in seconds to poll for changes.
refresh_script_args...: The command to execute before polling, this command may be used to refresh the contents of the pane (e.g. push data to a server and that push may cause a change in the contents of the monitored pane).

Example:

${SCRIPT} --window_name LOG_WINDOW --freq 1 --custom_message "SSH client received change!" -- log "new log entry!"

That would monitor a window called LOG_WINDOW for changes and suppose this window's contents changed whenever someone added a log entry, the tmux monitor would add an entry and then notify if the log actually showed up in the LOG_WINDOW.
EOF
}

USE_DESKTOP_NOTIFY=
function parse_args {
  while [ $# -gt 0 ]; do
    arg="$1"
    case $arg in
      --help|-h)
        usage
        exit 0
        ;;
      --window_name|-w)
        window_name="$2"
        shift
        ;;
      --freq|-f)
        poll_frequency_sec="$2"
        shift
        ;;
      --custom_message|-m)
        custom_message="$2"
        shift
        ;;
      --)
        shift
        refresh_script+=("$@")
        break
        ;;
      *)
        echo "Unknown parameter: $1" >&2
        usage
        exit 1
        ;;
    esac
    shift
  done
  echo "window_name=${window_name}" >&2
  echo "poll_frequency_sec=${poll_frequency_sec}" >&2
  echo "custom_message=${custom_message}" >&2
  echo "refresh_script=${refresh_script[@]}" >&2
}

function get_pane_content {
  tmux capture-pane -p -t "${window_name}"
}

function refresh_pane {
  "${refresh_script[@]}"
}

function notify_new {
  local message="$1"
  message_len="${#message}"
  popup_width=$((message_len + 2))
  popup_height=3

  if [[ "${USE_DESKTOP_NOTIFY}" ]]; then
    notify-send "tmux-monitor" "${message}"
  fi
  tmux display-popup -y 1 -w "${popup_width}" -h "${popup_height}" "echo" "-e" "\e[1m\e[5m\033[1;93m${message}"
}

function monitor_content {
  local old_content=
  local new_content=
  old_content="$(get_pane_content)"
  while true; do
    focused_window="$(tmux display-message -p '#W')"
    if [[ "${focused_window}" == "${window_name}" ]]; then
      sleep "${poll_frequency_sec}s"
      continue
    fi
    refresh_pane
    sleep "2s"
    new_content="$(get_pane_content)"
    if [[ "${old_content}" != "${new_content}" ]]; then
      local message="Window ${window_name} changed!"
      if [[ "${custom_message}" ]]; then
        message="${custom_message}"
      fi
      notify_new "${message}"
      old_content="${new_content}"
    fi
    sleep "${poll_frequency_sec}s"
  done
}


function main {
  monitor_content
}


parse_args "$@"
main
