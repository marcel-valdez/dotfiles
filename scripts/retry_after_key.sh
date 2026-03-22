#!/usr/bin/env bash
UNRETRIABLE_ERROR=128
UNRETRIABLE_SUCCESS=129

source "${HOME}/lib/log_lib.sh" || exit ${UNRETRIABLE_ERROR}

command="$@"

ctrl_c_pressed=

function ctrl_c_handler {
  ctrl_c_pressed=1
  log::info "[ctrl+c] pressed, exiting."
  exit 0
}

trap ctrl_c_handler SIGINT

while true; do
  "$@"
  exit_code=$?
  log::info "Command '$*' finished with exit code: $?"
  if [[ ${exit_code} -eq ${UNRETRIABLE_SUCCESS} ]]; then
    log::info "Code ${exit_code} is UNRETRIABLE_SUCCESS, not retrying."
    break
  fi
  if [[ ${exit_code} -eq ${UNRETRIABLE_ERROR} ]]; then
    log::fatal "Code ${exit_code} is UNRETRIABLE_ERROR, not retrying."
    break
  fi

  echo "Press ctrl+c to exit, any other key to retry: '$*'" >&2
  read -n 1 -s -r >&2

  # 2. Flush any remaining characters (like [I) in the buffer
  # -t 0.05 waits 50ms for extra chars
  # -k is not standard, so we use a read loop
  while read -r -t 0.05; do :; done
done
