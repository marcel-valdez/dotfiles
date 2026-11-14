#!/usr/bin/env bash
# monitor-tmux-pane.sh: Polls a tmux window for output changes and dispatches
# non-blocking notifications via ~/bin/tmux-notify and os::notify.
#
# Usage Example:
#   monitor-tmux-pane.sh --window_name LOG_WINDOW --freq 2 -- true

# shellcheck source=/dev/null
[[ -f "${HOME}/lib/os_compat.sh" ]] && source "${HOME}/lib/os_compat.sh"

SCRIPT="$(basename "${BASH_SOURCE[0]}")"

function debug {
  if [[ -n "${DEBUG:-}" ]]; then
    echo "$@"
  fi
}

window_name=""
poll_frequency_sec=5
custom_message=""
declare -a refresh_script=()

function usage {
  cat <<EOF

${SCRIPT} [--help|-h] [--window_name|-w <name>] [--freq|-f <seconds>] [--custom_message|-m <message>] [--no_notify] -- refresh_script_args ...

This is a script to monitor the contents of a tmux window. If the contents change a popup will be shown on the tmux client with a message.

help: Displays this help message
window_name: The name of the window to monitor.
freq: Frequency in seconds to poll for changes.
custom_message: A custom message to show in the popup dialog. Default: Window <window_name> changed!
no_notify: Do not notify about new messages, only highlight the window.
refresh_script_args...: The command to execute before polling, this command may be used to refresh the contents of the pane (e.g. push data to a server and that push may cause a change in the contents of the monitored pane).

Example:

${SCRIPT} --window_name LOG_WINDOW --freq 1 --custom_message "SSH client received change!" -- log "new log entry!"

That would monitor a window called LOG_WINDOW for changes and suppose this window's contents changed whenever someone added a log entry, the tmux monitor would add an entry and then notify if the log actually showed up in the LOG_WINDOW.
EOF
}

USE_DESKTOP_NOTIFY="${USE_DESKTOP_NOTIFY:-}"
notify_enabled=1

function parse_args {
  while (( $# > 0 )); do
    local arg="$1"
    case "${arg}" in
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
      --no_notify)
        notify_enabled=""
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
  echo "notify_enabled=${notify_enabled}" >&2
  echo "refresh_script=${refresh_script[*]}" >&2
}

function get_pane_content {
  tmux capture-pane -p -t "${window_name}" \
    | tr -d '\n' \
    | sed 's|oh=..*&||g' \
    | sed 's|ohc=..*&||g'
}

function refresh_pane {
  if (( ${#refresh_script[@]} > 0 )); then
    "${refresh_script[@]}"
  fi
}

function notify_new {
  local message="$1"
  if [[ -n "${USE_DESKTOP_NOTIFY:-}" ]]; then
    if declare -F os::notify >/dev/null 2>&1; then
      os::notify -- "tmux-monitor" "${message}" || true
    elif command -v notify-send >/dev/null 2>&1; then
      notify-send "tmux-monitor" "${message}" || true
    fi
  fi
  "${HOME}/bin/tmux-notify" -- "tmux-monitor: ${window_name:-pane}" \
    "${message}" >/dev/null 2>&1 &
}

function remove_window_hook {
  tmux set-hook -t "${window_name}" -u "pane-focus-in[11]"
}

function set_window_hook {
  # Remove the 'changed' mark after the user focuses into the window.
  tmux set-hook -t "${window_name}" "pane-focus-in[11]" \
    "setw -t '${window_name}' window-status-style default"
}

function mark_window_changed {
  # Highlight the window status in green when its pane content changes.
  tmux setw -t "${window_name}" "window-status-style" "fg=#88c388"
}

function remove_tmux_monitor {
  tmux setw -t "${window_name}" "monitor-activity" "off"
}

function cleanup {
  local exit_code="${1:-0}"
  remove_window_hook
  exit "${exit_code}"
}

function monitor_content {
  local old_content new_content focused_window diff_str message
  old_content="$(get_pane_content)"
  set_window_hook
  trap 'cleanup 0' EXIT
  trap 'cleanup 130' INT

  while true; do
    focused_window="$(tmux display-message -p '#W')"
    if [[ "${focused_window}" == "${window_name}" ]]; then
      sleep "${poll_frequency_sec}s"
      continue
    fi
    refresh_pane
    sleep "2s"
    # Normalize ephemeral media query tokens prior to comparing snapshots.
    new_content="$(get_pane_content)"
    if ! diff <(printf '%s\n' "${old_content}") \
        <(printf '%s\n' "${new_content}") &>/dev/null; then
      if [[ -n "${DEBUG:-}" ]]; then
        diff_str="$(diff <(printf '%s\n' "${old_content}") \
          <(printf '%s\n' "${new_content}") || true)"
        debug "--- diff start ---"
        debug "${diff_str}"
        debug "--- diff end ---"
      fi
      message="Window ${window_name} changed!"
      if [[ -n "${custom_message}" ]]; then
        message="${custom_message}"
      fi
      mark_window_changed
      if [[ -n "${notify_enabled}" ]]; then
        notify_new "${message}"
      fi
      old_content="${new_content}"
    fi
    sleep "${poll_frequency_sec}s"
  done
}

function main {
  monitor_content
}

if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then
  parse_args "$@"
  main
fi
