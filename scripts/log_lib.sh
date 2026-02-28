#!/usr/bin/env bash

[[ -z "${LOG_LEVEL}" ]] && LOG_LEVEL=2
[[ -z "${LOG_FILE}" ]] && LOG_FILE="$(mktemp)"
[[ -z "${LOG_SCRIPT_NAME}" ]] && LOG_SCRIPT_NAME=

function log {
  local log_level=$1
  local log_type=$2
  shift
  shift
  if [[ "${LOG_LEVEL}" -ge "${log_level}" ]] || {
       [[ "${log_type}" == "DEBUG" ]] && [[ -n "${DEBUG}" ]]
     }; then

    if [[ -n "${LOG_FILE}" ]]; then
      if ! [[ -e "${LOG_FILE}" ]]; then
        touch "${LOG_FILE}"
      fi
      echo "[${log_type}] ${LOG_SCRIPT_NAME}: $*" >> "${LOG_FILE}"
    fi
    if [[ -n "${LOG_TO_STDERR}" ]]; then
      echo "[${log_type}] ${LOG_SCRIPT_NAME}: $*" >&2
    fi
  fi
}

function debug {
  log 3 DEBUG "$@" & disown
}

function info {
  log 2 INFO "$@" & disown
}

function warn {
  log 1 WARN "$@" & disown
}

function error {
  log 0 ERROR "$@" & disown
}

function fatal {
  log -1 FATAL "$@" & disown
  exit 1
}
