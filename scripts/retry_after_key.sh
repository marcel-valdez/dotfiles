#!/usr/bin/env bash
UNRETRIABLE_ERROR=128
UNRETRIABLE_SUCCESS=129

for _dir in "${HOME}/bin" "${HOME}/.local/bin" "${HOME}/.fzf/bin" "/usr/local/bin" "/usr/local/go/bin" "/opt/homebrew/bin" "/Applications/kitty.app/Contents/MacOS"; do
  if [[ -d "${_dir}" && ":${PATH}:" != *":${_dir}:"* ]]; then
    export PATH="${_dir}:${PATH}"
  fi
done
unset _dir

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

  # 2. Flush any remaining characters (like [I) in the buffer.
  # macOS /bin/bash 3.2 only accepts integer -t timeouts, so use perl IO::Select
  # on Bash < 4 for a 50ms drain window.
  if (( BASH_VERSINFO[0] >= 4 )); then
    while read -r -t 0.05; do :; done
  elif command -v perl >/dev/null 2>&1; then
    while perl -MIO::Select -e 'exit(IO::Select->new(\*STDIN)->can_read(0.05) ? 0 : 1)' 2>/dev/null; do
      IFS= read -r -n 1 _ || break
    done
  fi
done
