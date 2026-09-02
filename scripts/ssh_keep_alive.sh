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


