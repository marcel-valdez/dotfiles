#!/usr/bin/env bash
UNRETRIABLE_SUCCESS=129
UNRETRIABLE_ERROR=128

source "${HOME}/lib/log_lib.sh" || exit ${UNRETRIABLE_ERROR}

# Use a specific session name so you always land in the same place

while true; do
    log::info "[$(date +%H:%M:%S)] Attempting to connect..."
    # -o ConnectTimeout=5 prevents the script from hanging if the network is still waking up
    ssh -o ConnectTimeout=5 "$@"
    exit_code=$?

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


