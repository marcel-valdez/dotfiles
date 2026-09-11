#!/usr/bin/env bash
# ==============================================================================
# Tmux Status Daemon
# ==============================================================================
# Background worker updating tmux status line with network latency, CPU/memory
# metrics, and timestamps.
#
# Hardened against cross-server signal interference:
# - Namespaces PID files by UID and tmux socket label
# - Validates /proc/<pid>/cmdline before sending signals
# - Uses graceful SIGTERM with 1.5s polling loop before SIGKILL escalation
# - Traps EXIT, SIGTERM, SIGINT, SIGHUP to reap child timers and PID files
# ==============================================================================

set -uo pipefail

INTERVAL_SECS=1
TPM_PATH="${TMUX_PLUGIN_MANAGER_PATH:-${HOME}/.tmux/plugins}"
USER_ID="${UID:-$(id -u)}"
PID_DIR="${XDG_RUNTIME_DIR:-/tmp}"

# ------------------------------------------------------------------------------
# 1. Socket Discovery & PID File Namespacing
# ------------------------------------------------------------------------------
if [[ -z "${TMUX_SOCKET_LABEL:-}" ]]; then
  SOCKET_PATH="$(tmux display-message -p '#{socket_path}' 2>/dev/null || true)"
  if [[ -n "${SOCKET_PATH}" ]]; then
    SOCKET_LABEL="$(basename "${SOCKET_PATH}")"
  else
    SOCKET_LABEL="default"
  fi
else
  SOCKET_LABEL="${TMUX_SOCKET_LABEL}"
fi

# Sanitize socket label to prevent directory traversal or invalid characters
SOCKET_LABEL="${SOCKET_LABEL//[^a-zA-Z0-9_-]/_}"
PIDFILE="${PID_DIR}/tmux_status_daemon_${USER_ID}_${SOCKET_LABEL}.pid"

# Parameterize tmux command to explicitly target discovered socket
if [[ -n "${SOCKET_PATH:-}" && -S "${SOCKET_PATH}" ]]; then
  TMUX_CMD=(tmux -S "${SOCKET_PATH}")
else
  TMUX_CMD=(tmux -L "${SOCKET_LABEL}")
fi

# ------------------------------------------------------------------------------
# 2. Process Validation & Graceful Termination
# ------------------------------------------------------------------------------
is_status_daemon_process() {
  local target_pid="$1"
  [[ -n "${target_pid}" && "${target_pid}" =~ ^[0-9]+$ ]] || return 1
  kill -0 "${target_pid}" 2>/dev/null || return 1
  if [[ -r "/proc/${target_pid}/cmdline" ]]; then
    if { tr '\0' ' ' < "/proc/${target_pid}/cmdline"; } 2>/dev/null | grep -Fq "tmux_status_daemon.sh"; then
      return 0
    fi
  fi
  return 1
}

terminate_existing_daemon() {
  local target_pid="$1"
  if ! is_status_daemon_process "${target_pid}"; then
    return 0
  fi

  # Attempt graceful termination first
  kill -TERM "${target_pid}" 2>/dev/null || true

  # Poll exit every 100ms up to 1.5s (15 iterations of 0.1s)
  local elapsed=0
  while kill -0 "${target_pid}" 2>/dev/null; do
    if (( elapsed >= 15 )); then
      # Escalate to SIGKILL on timeout
      kill -KILL "${target_pid}" 2>/dev/null || true
      break
    fi
    sleep 0.1
    elapsed=$((elapsed + 1))
  done
}

# ------------------------------------------------------------------------------
# 3. Clean Prior Instances & Migrate Legacy PID Files
# ------------------------------------------------------------------------------
# Clean up existing instance for this socket
if [[ -f "${PIDFILE}" ]]; then
  existing_pid="$(head -n 1 "${PIDFILE}" 2>/dev/null | tr -d '[:space:]')"
  if [[ -n "${existing_pid}" && "${existing_pid}" != "$$" ]]; then
    if is_status_daemon_process "${existing_pid}"; then
      terminate_existing_daemon "${existing_pid}"
    fi
  fi
  rm -f "${PIDFILE}" 2>/dev/null || true
fi

# If on default socket, clean up legacy un-namespaced PID file if present
LEGACY_PIDFILE="${TEST_LEGACY_PIDFILE:-/tmp/tmux_status_daemon.pid}"
if [[ "${SOCKET_LABEL}" == "default" && "${PIDFILE}" != "${LEGACY_PIDFILE}" && -f "${LEGACY_PIDFILE}" ]]; then
  legacy_pid="$(head -n 1 "${LEGACY_PIDFILE}" 2>/dev/null | tr -d '[:space:]')"
  if [[ -n "${legacy_pid}" && "${legacy_pid}" != "$$" ]]; then
    if is_status_daemon_process "${legacy_pid}"; then
      terminate_existing_daemon "${legacy_pid}"
    fi
  fi
  rm -f "${LEGACY_PIDFILE}" 2>/dev/null || true
fi

# ------------------------------------------------------------------------------
# 4. Signal Traps & PID Registration
# ------------------------------------------------------------------------------
TIMER_PID=""

cleanup() {
  trap - SIGTERM SIGINT SIGHUP EXIT

  # Terminate child timer process if alive
  if [[ -n "${TIMER_PID:-}" ]] && kill -0 "${TIMER_PID}" 2>/dev/null; then
    kill -TERM "${TIMER_PID}" 2>/dev/null || true
    wait "${TIMER_PID}" 2>/dev/null || true
  fi

  # Remove PIDFILE if it records our PID
  if [[ -f "${PIDFILE}" ]]; then
    local current_pid
    current_pid="$(head -n 1 "${PIDFILE}" 2>/dev/null | tr -d '[:space:]')"
    if [[ "${current_pid}" == "$$" ]]; then
      rm -f "${PIDFILE}" 2>/dev/null || true
    fi
  fi

  exit 0
}

trap cleanup SIGTERM SIGINT SIGHUP EXIT

echo $$ > "${PIDFILE}"

# ------------------------------------------------------------------------------
# 5. Status Polling Loop
# ------------------------------------------------------------------------------
CONSECUTIVE_FAILURES=0

while true; do
  # 1. Start the interval timer in the background instantly
  sleep "${INTERVAL_SECS}" &
  TIMER_PID="$!"

  # 2. Fetch Ping
  ping_val="$(ping -c 1 -W 1 8.8.8.8 2>/dev/null | sed -n "s/.*time=\([^ ]*\).*/\1/p")"
  ping_val="${ping_val:-ERR}"

  # 3. Fetch CPU/Mem
  if [ -x "${TPM_PATH}/tmux-mem-cpu-load/tmux-mem-cpu-load" ]; then
    mem_cpu="$("${TPM_PATH}/tmux-mem-cpu-load/tmux-mem-cpu-load" --colors --interval 1 --powerline-right --segments-right 233)"
  else
    mem_cpu=" CPU Load Error "
  fi

  # 4. Fetch Time natively
  pac_date="$(TZ="US/Pacific" date "+%d/%m")"

  if [[ "$(hostname)" == *"marcelvaldez.c.googlers.com"* ]]; then
    time_val="$(TZ="US/Pacific" date "+%H:%M:%S")"
  else
    time_val="$(date "+%H:%M:%S")"
  fi

  # 5. Fetch the @server_alias directly from tmux memory
  server_alias="$("${TMUX_CMD[@]}" show-option -gqv @server_alias 2>/dev/null || true)"

  # 6. PUSH the constructed string directly into tmux status
  NEW_STATUS="#[reverse]📡 ${ping_val} ms#[default]${mem_cpu}#[default]#[reverse,bold] ${server_alias} #[default,bold] ${pac_date} #[reverse,bold] ${time_val}"
  if "${TMUX_CMD[@]}" set -g status-right "${NEW_STATUS}" 2>/dev/null; then
    CONSECUTIVE_FAILURES=0
  else
    CONSECUTIVE_FAILURES=$((CONSECUTIVE_FAILURES + 1))
    if (( CONSECUTIVE_FAILURES >= 5 )); then
      exit 0
    fi
  fi

  # 7. Wait for the background timer to finish.
  wait "${TIMER_PID}" 2>/dev/null || true
  TIMER_PID=""
done
