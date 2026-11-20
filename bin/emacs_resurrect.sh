#!/usr/bin/env bash

DEBUG=
LOG_FILE="/tmp/emacs_resurrect_debug.log"

function log {
  if [[ "${DEBUG}" -eq 1 ]]; then
    date >> "${LOG_FILE}"
    echo "$@" >> "${LOG_FILE}"
  fi
}

function run {
  log "$@"
  "$@"
}

TMUX_SESSION_NAME=$(tmux display-message -p '#S')
GOOGLE3=
# NOTE: the corp-only google3 client layout (/google/src/cloud/...) doesn't
# exist outside the office machine, so those branches are commented out here;
# every session just resurrects emacs from $HOME.
case "${TMUX_SESSION_NAME}" in
  "default")
    TARGET_DIR="${HOME}"
    ;;
  # "helper")
  #   TMUX_WINDOW_NAME=$(tmux display-message -p '#W')
  #   TARGET_DIR="/google/src/cloud/${USER}/${TMUX_WINDOW_NAME}/google3"
  #   GOOGLE3=1
  #   ;;
  *)
    # TARGET_DIR="/google/src/cloud/${USER}/${TMUX_SESSION_NAME}/google3"
    # GOOGLE3=1
    TARGET_DIR="${HOME}"
    ;;
esac

if ! [[ -d "${TARGET_DIR}" ]]; then
  GOOGLE3=
  TARGET_DIR="${HOME}"
fi

log "TMUX_SESSION_NAME: ${TMUX_SESSION_NAME}"
log "TARGET_DIR: ${TARGET_DIR}"

if ! cd "${TARGET_DIR}"; then
  echo "Unable to CD into ${TARGET_DIR}" >&2
  exit 1
fi

declare -a ORIGINAL_ARGS=("$@")

log "ORIGINAL_ARGS: ${ORIGINAL_ARGS[*]}"

has_no_window_system=
for arg in "${ORIGINAL_ARGS[@]}"; do
  if [[ "${arg}" == "--no-window-system" ]]; then
    has_no_window_system=1
  fi
done

if [[ -z "${has_no_window_system}" ]]; then
  ORIGINAL_ARGS+=("--no-window-system")
fi

# Corp-only: sourcing ~/.googlerc.d/.google_functions before launching emacs
# inside a google3 client. Not applicable outside the office machine.
# if [[ "${GOOGLE3}" -eq 1 ]] && [[ -f "${HOME}/.googlerc.d/.google_functions" ]]; then
#   run source "${HOME}/.googlerc.d/.google_functions"
#   run exec emacs "${ORIGINAL_ARGS[@]}"
if [[ -f "${HOME}/.bash_functions" ]]; then
  run source "${HOME}/.bash_functions"
fi

# Resolve the real emacs binary (bypassing function emacs() from .bash_functions)
EMACS_BIN=""
for _cand in \
  "${HOME}/.local/bin/emacs" \
  "/snap/bin/emacs" \
  "$(type -P emacs 2>/dev/null || true)" \
  "/usr/bin/emacs"; do
  if [[ -n "${_cand}" && -x "${_cand}" ]]; then
    EMACS_BIN="${_cand}"
    break
  fi
done
unset _cand
: "${EMACS_BIN:=emacs}"

run exec "${EMACS_BIN}" "${ORIGINAL_ARGS[@]}"
