#!/usr/bin/env bash

# Configurable variables:
# LOG_LEVEL: Log level to use when logging 0 (error), 1 (warn), 2 (info), 3 (debug). Default: 2 (info).
# LOG_SCRIPT_NAME: The name of script to use in log statements. Default: generated.
# LOG_FILE: The file in which to log entries. Default: generated.
# LOG_TO_STDERR: Whether to log to STDERR as well. Default: empty.
# LOG_PID: PID for the program logging the entries. Default: $$ (recommended).
# DEBUG: (deprecated) Forced logging DEBUG statements (NOT equivalent to LOG_LEVEL=3). Default: empty.

[[ -z "${LOG_PID}" ]] && export LOG_PID="$$"
[[ -z "${LOG_LEVEL}" ]] && export LOG_LEVEL=2
if [[ -z "${LOG_SCRIPT_NAME}" ]]; then
  if [[ -n "${BASH_SOURCE[1]}" ]]; then
    export LOG_SCRIPT_NAME="$(basename ${BASH_SOURCE[1]})"
  else
    export LOG_SCRIPT_NAME="$(basename $(ps -p "${LOG_PID}" -o comm=))"
  fi
fi
if [[ -z "${LOG_FILE}" ]]; then
  if [[ -n "${BASH_SOURCE[1]}" ]]; then
    export LOG_FILE="/tmp/${LOG_SCRIPT_NAME}.log"
  else
    LOG_START_TIME="$(date "+%H%M%S")"
    export LOG_FILE="/tmp/${LOG_SCRIPT_NAME}_${LOG_PID}_${LOG_START_TIME}.log"
  fi
fi

function log::now {
  date "+%H:%M:%S"
}

function log::log {
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
      echo "[${log_type}] @$(now) ${LOG_SCRIPT_NAME}/${LOG_PID}: $*" >> "${LOG_FILE}"
    fi
    if [[ -n "${LOG_TO_STDERR}" ]]; then
      echo "[${log_type}] @$(now) ${LOG_SCRIPT_NAME}/${LOG_PID}: $*" >&2
    fi
  fi
}

function log::debug {
  log 3 DEBUG "$@" & disown
}

function log::info {
  log 2 INFO "$@" & disown
}

function log::warn {
  log 1 WARN "$@" & disown
}

function log::error {
  log 0 ERROR "$@" & disown
}

function log::fatal {
  log -1 FATAL "$@" & disown
  exit 1
}
