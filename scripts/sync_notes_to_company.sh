#!/usr/bin/env bash

set -o errexit  # Exit if a command fails
set -o nounset  # Exit if we use an undeclared variable
set -o pipefail  # Use the exit status of the last command that threw a non-zero exit code
# set -o xtrace  # Trace what gets executed, useful for debugging.

LOG_FILE="/tmp/sync_notes_to_company.log"

function now {
  date "+%H:%M:%S"
}

function log {
  local msg=
  msg="$(now) $*"
  echo "${msg}" >> "${LOG_FILE}"
  echo "${msg}"
}

INPUT_DIRECTORY="${INPUT_DIRECTORY:-${HOME}/notes}"
OUTPUT_DIRECTORY="${OUTPUT_DIRECTORY:-/google/src/cloud/marcelvaldez/personal_notes/company/users/marcelvaldez}"
STATE_DIRECTORY="${SYNC_STATE_DIR:-${HOME}/.config/sync_notes_to_company}"
SCRIPT_DIR="$(cd "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")" && pwd)"
ENGINE="${SCRIPT_DIR}/sync_notes_engine.py"
if [[ ! -f "${ENGINE}" ]]; then
  ENGINE="${HOME}/scripts/sync_notes_engine.py"
fi

function is_dry_run {
  for arg in "$@"; do
    if [[ "${arg}" == "--dry-run" || "${arg}" == "-n" ]]; then
      return 0
    fi
  done
  return 1
}

function is_help {
  for arg in "$@"; do
    if [[ "${arg}" == "--help" || "${arg}" == "-h" ]]; then
      return 0
    fi
  done
  return 1
}

function main {
  if is_help "$@"; then
    python3 "${ENGINE}" --help
    return 0
  fi

  if is_dry_run "$@"; then
    log "Dry-run mode requested, executing sync_notes preview without modifying files or VCS."
    python3 "${ENGINE}" \
      --input "${INPUT_DIRECTORY}" \
      --output "${OUTPUT_DIRECTORY}" \
      --state-dir "${STATE_DIRECTORY}" \
      "$@"
    return $?
  fi

  if /usr/bin/gcertstatus -check_ssh=false -check_remaining=60s --quiet; then
    log "Found a valid gcert, proceeding to sync notes with company docs"
    python3 "${ENGINE}" \
      --input "${INPUT_DIRECTORY}" \
      --output "${OUTPUT_DIRECTORY}" \
      --state-dir "${STATE_DIRECTORY}" \
      "$@"
  else
    log "WARNING: gcert is no longer valid, therefore we can't sync notes to company doc." >&2
    exit 1
  fi
}

main "$@"
