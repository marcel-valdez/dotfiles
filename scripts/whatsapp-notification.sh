a#!/usr/bin/env bash

# Log settings
export LOG_SCRIPT_NAME=
LOG_SCRIPT_NAME="$(basename "$0")"
[[ -z "${LOG_LEVEL}" ]] && export LOG_LEVEL=3
# Log to the stdout and let the journal system log capture the output
[[ -z "${LOG_FILE}" ]] && export LOG_FILE="/tmp/whatsapp-notification.log"
source "${HOME}/lib/log_lib.sh"

function dispatch {
  log::debug "dispatch $*"
  (nohup systemd-run --user "$@" &>/dev/null) & disown
}

function run {
  log::debug "run $*"
  "$@"
}

# Read the incoming JSON context from Gemini CLI
NOTIFICATION_DURATION_MINUTES=$((60*12))
TITLE="$1"
MESSAGE="$2"
log::info "Processing WhatsApp title: <${TITLE}> message: <${MESSAGE}>"

# Otherwise use local notifications on terminal & desktop.
notified=
if run type tmux-notify &>/dev/null; then
  dispatch tmux-notify "${TITLE}" "${MESSAGE}"
  notified=1
fi

if [[ -n "${DISPLAY}" ]] && run type notify-send &>/dev/null; then
  EXPIRE_TIME_MILLI_SECS=$((NOTIFICATION_DURATION_MINUTES * 60 * 1000))
  dispatch notify-send "--app-name=WhatsApp" \
    "--urgency=normal" \
    "--icon=user-available" \
    "--expire-time=${EXPIRE_TIME_MILLI_SECS}" \
    "${TITLE}" "${MESSAGE}"
  notified=1
fi

if [[ -z "${DISPLAY}" ]]; then
  log::warn "Unable to send notification to desktop due to ${DISPLAY} not being available."
fi

if [[ -z "${notified}" ]]; then
  log::error "neither tmux-notify nor notify-send where available to notify the user."
fi
