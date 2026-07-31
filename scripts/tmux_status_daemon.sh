#!/usr/bin/env bash

INTERVAL_SECS=1
# Ensure only one instance of the daemon runs at a time
PIDFILE="/tmp/tmux_status_daemon.pid"
if [ -f "${PIDFILE}" ]; then
    kill -9 $(cat "${PIDFILE}") 2>/dev/null
fi
echo $$ > "${PIDFILE}"

TPM_PATH="${TMUX_PLUGIN_MANAGER_PATH:-$HOME/.tmux/plugins}"

while true; do
    # 1. Start the interval timer in the background instantly
    sleep "${INTERVAL_SECS}" &
    TIMER_PID=$!

    # 2. Fetch Ping
    ping_val=$(ping -c 1 -W 1 8.8.8.8 2>/dev/null | sed -n "s/.*time=\([^ ]*\).*/\1/p")
    ping_val=${ping_val:-"ERR"}

    # 3. Fetch CPU/Mem
    if [ -x "${TPM_PATH}/tmux-mem-cpu-load/tmux-mem-cpu-load" ]; then
        mem_cpu=$("${TPM_PATH}/tmux-mem-cpu-load/tmux-mem-cpu-load" --colors --interval 1 --powerline-right --segments-right 233)
    else
        mem_cpu=" CPU Load Error "
    fi

    # 4. Fetch Time natively
    pac_date=$(TZ="US/Pacific" date "+%d/%m")
    
    if [[ "$(hostname)" == *"marcelvaldez.c.googlers.com"* ]]; then
        time_val=$(TZ="US/Pacific" date "+%H:%M:%S")
    else
        time_val=$(date "+%H:%M:%S")
    fi

    # 5. Fetch the @server_alias directly from tmux memory
    server_alias=$(tmux show-option -gqv @server_alias)

    # 6. PUSH the constructed string directly into tmux
    NEW_STATUS="#[reverse]📡 ${ping_val} ms#[default]${mem_cpu}#[default]#[reverse,bold] ${server_alias} #[default,bold] ${pac_date} #[reverse,bold] ${time_val}"
    
    tmux set-option -g status-right "${NEW_STATUS}"

    # 7. Wait for the background timer to finish.
    # This automatically calculates and waits for the exact remaining time delta!
    wait ${TIMER_PID} 2>/dev/null
done
