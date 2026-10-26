#!/usr/bin/env bash

# Log settings
export LOG_SCRIPT_NAME=
LOG_SCRIPT_NAME="$(basename "$0")"
[[ -z "${LOG_LEVEL}" ]] && export LOG_LEVEL=3
# Log to the stdout and let the journal system log capture the output
[[ -z "${LOG_FILE}" ]] && export LOG_FILE="/tmp/whatsapp-notification.log"
source "${HOME}/lib/log_lib.sh"
if [[ -f "${HOME}/lib/os_compat.sh" ]]; then
  source "${HOME}/lib/os_compat.sh"
fi

# Daemon launcher mode when invoked with 0 arguments (e.g. from systemd or launchd)
if [[ $# -eq 0 ]]; then
  WHATSAPP_CONFIG_ENV="${HOME}/.config/whatsapp-notification/config.env"
  if [[ -f "${WHATSAPP_CONFIG_ENV}" ]]; then
    # shellcheck disable=SC1090
    source "${WHATSAPP_CONFIG_ENV}"
  fi
  TARGET_JIDS="${TARGET_JIDS:-${WHATSAPP_TARGET_JIDS:-}}"
  WHATSAPP_BIN_DIR="${WHATSAPP_BIN_DIR:-${HOME}/projects/whatsapp-notification/bin}"
  WHATSAPP_BIN="${WHATSAPP_BIN_DIR}/whatsapp-notification"
  if [[ ! -x "${WHATSAPP_BIN}" ]]; then
    log::error "whatsapp-notification binary not found or not executable at ${WHATSAPP_BIN}"
    exit 1
  fi
  cd "${WHATSAPP_BIN_DIR}" || exit 1
  help_text="$("${WHATSAPP_BIN}" -h 2>&1 || true)"
  jid_flag="-target"
  if [[ "${help_text}" == *"-jids"* ]]; then
    jid_flag="-jids"
  fi
  if [[ -n "${TARGET_JIDS}" ]]; then
    exec "${WHATSAPP_BIN}" "${jid_flag}" "${TARGET_JIDS}" -exec "${HOME}/scripts/whatsapp-notification.sh"
  else
    exec "${WHATSAPP_BIN}" -exec "${HOME}/scripts/whatsapp-notification.sh"
  fi
fi

function dispatch {
  log::debug "dispatch $*"
  if declare -F os::is_macos >/dev/null 2>&1 && os::is_macos; then
    ("$@" &>/dev/null) & disown
  elif command -v systemd-run &>/dev/null; then
    (nohup systemd-run --user "$@" &>/dev/null) & disown
  else
    ("$@" &>/dev/null) & disown
  fi
}

function run {
  log::debug "run $*"
  "$@"
}

# Read the incoming notification arguments
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

if declare -F os::is_macos >/dev/null 2>&1 && os::is_macos; then
  dispatch os::notify -a "WhatsApp" -g "whatsapp-notification" -- "${TITLE}" "${MESSAGE}"
  notified=1
elif [[ -n "${DISPLAY}" ]] && run type notify-send &>/dev/null; then
  EXPIRE_TIME_MILLI_SECS=$((NOTIFICATION_DURATION_MINUTES * 60 * 1000))
  dispatch notify-send "--app-name=WhatsApp" \
    "--urgency=normal" \
    "--icon=user-available" \
    "--expire-time=${EXPIRE_TIME_MILLI_SECS}" \
    "${TITLE}" "${MESSAGE}"
  notified=1
else
  log::warn "Unable to send notification to desktop due to DISPLAY not being available."
fi

if [[ -z "${notified}" ]]; then
  log::error "neither tmux-notify nor desktop notification was available to notify the user."
fi
