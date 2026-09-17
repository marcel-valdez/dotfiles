#!/usr/bin/env bash
UNRETRIABLE_SUCCESS=129
UNRETRIABLE_ERROR=128

source "${HOME}/lib/log_lib.sh" || exit ${UNRETRIABLE_ERROR}

# Source .googlerc to inherit configurations like USE_GMOSH if not already in env
if [[ -z "${USE_GMOSH:-}" ]]; then
  if [[ -f "${HOME}/.googlerc" ]]; then
    source "${HOME}/.googlerc" 2>/dev/null || true
  elif [[ -f "${HOME}/.googlerc.d/.googlerc" ]]; then
    source "${HOME}/.googlerc.d/.googlerc" 2>/dev/null || true
  fi
fi

use_gmosh=0
if [[ "$1" == "--gmosh" ]]; then
  use_gmosh=1
  shift
elif [[ "${USE_GMOSH:-0}" == "1" ]]; then
  use_gmosh=1
fi

if [[ ${use_gmosh} -eq 1 ]] && ! command -v gmosh &>/dev/null; then
  log::warn "gmosh not found in PATH, falling back to ssh."
  use_gmosh=0
fi

#######################################
# Ensures valid credentials exist before attempting connection.
#
# If gcertstatus indicates expired credentials, enters a non-blocking polling
# loop checking credential validity every GCERT_POLL_INTERVAL seconds (default:
# 2s). This allows credentials renewed in any other terminal or machine to
# automatically unblock and reconnect background Kitty tabs without prompting
# for passwords.
#
# If stdin is an interactive terminal, the user can press 'g' or 'G' to trigger
# gcert authentication directly in this tab. Kitty focus-reporting escape
# sequences (\e[I / \e[O) are safely discarded to prevent false trigger events.
#
# Arguments:
#   None
# Returns:
#   0 if credentials are valid or renewed.
#
# Usage Example:
#   wait_for_valid_credentials
#######################################
wait_for_valid_credentials() {
  if ! command -v gcertstatus &>/dev/null; then
    return 0
  fi

  local gcert_cmd=(gcertstatus --nocheck_loas2 --ssh_cert_comment='corp/normal' --quiet)

  if "${gcert_cmd[@]}"; then
    return 0
  fi

  log::warn "[$(date +%H:%M:%S)] Credentials expired. Waiting for gcert renewal in another terminal... (or press 'g' to authenticate here)"

  local poll_interval="${GCERT_POLL_INTERVAL:-2}"

  while ! "${gcert_cmd[@]}"; do
    local user_key=""
    if read -r -s -t "${poll_interval}" -n 1 user_key; then
      if [[ "${user_key}" == $'\e' ]]; then
        # Drain escape sequences (e.g. Kitty focus events ^[[I / ^[[O or arrow keys)
        while read -r -s -t 0.05 -n 1; do :; done
      elif [[ "${user_key}" == "g" || "${user_key}" == "G" ]]; then
        log::info "Starting gcert authentication in this terminal..."
        if command -v gcert &>/dev/null; then
          gcert || true
        fi
      fi
    else
      local read_status=$?
      if [[ ${read_status} -ne 0 && ${read_status} -le 128 ]]; then
        # EOF on non-terminal stdin (such as </dev/null); sleep to avoid spinning
        sleep "${poll_interval}"
      fi
    fi
  done

  log::info "[$(date +%H:%M:%S)] Credentials valid. Resuming connection..."
  return 0
}

# Use a specific session name so you always land in the same place

while true; do
    if [[ ${use_gmosh} -eq 1 ]]; then
        # Parse arguments, filtering out ssh-specific flags (-t, -Y, -tY)
        target_args=()
        for arg in "$@"; do
            if [[ "$arg" =~ ^-[tY]+$ ]]; then
                continue
            fi
            target_args+=("$arg")
        done

        target_host="${target_args[0]}"
        cmd_args=("${target_args[@]:1}")

        wait_for_valid_credentials

        log::info "[$(date +%H:%M:%S)] Attempting to connect via gmosh to ${target_host}..."

        # mosh requires remote command tokens passed separately so mosh-server can execvp correctly
        if [[ ${#cmd_args[@]} -eq 1 ]]; then
            read -r -a split_cmd <<< "${cmd_args[0]}"
            gmosh --ssh="ssh -o ConnectTimeout=5 -ttY" "${target_host}" -- "${split_cmd[@]}"
        elif [[ ${#cmd_args[@]} -gt 1 ]]; then
            gmosh --ssh="ssh -o ConnectTimeout=5 -ttY" "${target_host}" -- "${cmd_args[@]}"
        else
            gmosh --ssh="ssh -o ConnectTimeout=5 -ttY" "${target_host}"
        fi
        exit_code=$?
    else
        if [[ "${USE_GCERT_GATE:-0}" == "1" ]] || [[ "$*" =~ (gcloud|\.corp\.google\.com|\.c\.googlers\.com) ]]; then
            wait_for_valid_credentials
        fi
        log::info "[$(date +%H:%M:%S)] Attempting to connect via ssh..."
        # -o ConnectTimeout=5 prevents the script from hanging if the network is still waking up
        ssh -o ConnectTimeout=5 "$@"
        exit_code=$?
    fi

    # If you manually typed 'exit' or 'ctrl+d' inside tmux, exit_code is usually 0.
    # In that case, we should actually stop the loop.
    if [ "${exit_code}" -eq 0 ]; then
        log::info "Session exited normally."
        exit ${UNRETRIABLE_SUCCESS}
    fi

    log::warn "Connection lost (Code ${exit_code}). Retrying in 3s... (Ctrl+C to stop)"
    # Flush focus-reporting garbage before sleeping
    while read -r -t 0.05; do :; done
    sleep 3
    # Flush focus-reporting garbage after sleeping
    while read -r -t 0.05; do :; done
done


