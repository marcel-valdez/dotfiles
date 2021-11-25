#!/usr/bin/env bash

window_name=
poll_frequency_seq=
declare -a refresh_script

USE_DESKTOP_NOTIFY=

function parse_args {
  window_name="$1"
  poll_frequency_sec="$2"
  shift
  shift
  refresh_script+=("$@")

  echo "window_name=${window_name}" >&2
  echo "poll_frequency_sec=${poll_frequency_sec}" >&2
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
      notify_new "Window ${window_name} changed!"
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
