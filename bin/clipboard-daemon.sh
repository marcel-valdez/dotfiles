#!/usr/bin/env bash
# clipboard-daemon.sh: Listens locally for remote clipboard payloads and
# maintains a reverse SSH tunnel to gcloud so remote tmux copies sync to
# the local X11 clipboard.
# Designed to run as a user systemd service (clipboard-daemon.service).

HOST="127.0.0.1"
REMOTE_CLIPBOARD_HOST="${REMOTE_CLIPBOARD_HOST:-${USER}@marcelvaldez.c.googlers.com}"
LOG_FILE="/tmp/clipboard-daemon.log"

RESTART=0
PORT=""

# Parse CLI arguments
while [[ $# -gt 0 ]]; do
  case "$1" in
    -r|--restart|restart)
      RESTART=1
      shift
      ;;
    -p|--port)
      PORT="$2"
      shift 2
      ;;
    [0-9]*)
      PORT="$1"
      shift
      ;;
    *)
      shift
      ;;
  esac
done

# Source .googlerc to inherit configurations like CLIPBOARD_PORT
if [[ -z "${CLIPBOARD_PORT:-}" ]]; then
  if [[ -f "${HOME}/.googlerc" ]]; then
    source "${HOME}/.googlerc" 2>/dev/null || true
  elif [[ -f "${HOME}/.googlerc.d/.googlerc" ]]; then
    source "${HOME}/.googlerc.d/.googlerc" 2>/dev/null || true
  fi
fi

# Safeguard: Do not run on Cloudtop VM itself
if [[ "${PS1_HOST:-}" == "gcloud" || "$(hostname)" =~ \.c\.googlers\.com ]]; then
  echo "clipboard-daemon is designed to run on local workstation/laptop machines, not on Cloudtop."
  exit 0
fi

# Auto-detect default port by host type
DEFAULT_PORT=""
host_name="$(hostname -s 2>/dev/null || hostname)"
if [[ "${host_name}" =~ laptop|glaptop ]]; then
  DEFAULT_PORT=3334
elif [[ "${host_name}" =~ mtv|workstation ]]; then
  DEFAULT_PORT=3333
else
  DEFAULT_PORT=3333
fi

# Determine PORT
if [[ -z "${PORT}" ]]; then
  if [[ -n "${CLIPBOARD_PORT:-}" ]]; then
    PORT="${CLIPBOARD_PORT}"
  else
    PORT="${DEFAULT_PORT}"
  fi
fi

# Ensure default DISPLAY if unset (common under systemd user units)
if [[ -z "${DISPLAY:-}" ]]; then
  export DISPLAY=":0"
fi
if [[ -z "${XAUTHORITY:-}" && -f "${HOME}/.Xauthority" ]]; then
  export XAUTHORITY="${HOME}/.Xauthority"
fi

get_timestamp() {
  local d
  d="$(today 2>/dev/null || date '+%Y-%m-%d')"
  echo "${d} $(date '+%H:%M:%S')"
}

send_desktop_notification() {
  local title="$1"
  local body="$2"
  if command -v notify-send &>/dev/null; then
    # -u normal -t 600000 makes the notification stay on screen for exactly
    # 10 minutes (600,000 ms) before automatically disappearing
    local displays
    displays=$(ls /tmp/.X11-unix/ 2>/dev/null | sed 's/X//g')
    if [[ -z "${displays}" ]]; then
      notify-send -u normal -t 600000 -a "Clipboard Daemon" -i dialog-warning "${title}" "${body}" 2>/dev/null || true
    else
      for d in ${displays}; do
        [[ -n "${d}" ]] && DISPLAY=":${d}" notify-send -u normal -t 600000 -a "Clipboard Daemon" -i dialog-warning "${title}" "${body}" 2>/dev/null || true
      done
    fi
  fi
}

RUNTIME_DIR="${XDG_RUNTIME_DIR:-/tmp}"
LOCK_FILE="${RUNTIME_DIR}/clipboard-daemon-${PORT}.lock"
DAEMON_PID_FILE="${RUNTIME_DIR}/clipboard-daemon-${PORT}.pid"
PID_FILE="${RUNTIME_DIR}/clipboard-tunnel-${PORT}.pid"
ERR_LOG="${RUNTIME_DIR}/clipboard-ssh-${PORT}.log"

# Open lock file unconditionally on FD 200 before evaluating restart or startup
exec 200>"${LOCK_FILE}"

find_local_tunnel_pids() {
  local target_port="$1"
  local matching_pids=()
  local ssh_pids
  ssh_pids=$(pgrep -u "$(id -u)" -x ssh 2>/dev/null || true)
  local regex_r="(^|[[:space:]])-R[[:space:]]*(${target_port}:|[0-9a-zA-Z.*_-]+:${target_port}:)"
  local regex_rf="(^|[[:space:]])-o[[:space:]]+RemoteForward([=[:space:]]+)(${target_port}[:[:space:]]|[0-9a-zA-Z.*_-]+:${target_port}[:[:space:]])"

  for pid in ${ssh_pids}; do
    [[ -z "${pid}" ]] && continue
    if [[ -r "/proc/${pid}/status" ]]; then
      local state
      state=$(awk '/^State:/ {print $2}' "/proc/${pid}/status" 2>/dev/null || true)
      if [[ "${state}" == "Z" || "${state}" == "T" ]]; then
        continue
      fi
    fi

    if [[ -r "/proc/${pid}/cmdline" ]]; then
      local cmdline
      cmdline=$(tr '\0' ' ' < "/proc/${pid}/cmdline" 2>/dev/null || true)
      if [[ "${cmdline}" =~ ${regex_r} ]] || [[ "${cmdline}" =~ ${regex_rf} ]]; then
        matching_pids+=("${pid}")
      fi
    fi
  done
  echo "${matching_pids[@]}"
}

kill_existing_tunnel() {
  if [[ -f "${PID_FILE}" ]]; then
    local pid
    pid=$(cat "${PID_FILE}" 2>/dev/null || true)
    if [[ -n "${pid}" ]] && kill -0 "${pid}" 2>/dev/null; then
      kill -TERM "${pid}" 2>/dev/null || true
      for _ in {1..10}; do
        kill -0 "${pid}" 2>/dev/null || break
        sleep 0.1
      done
      if kill -0 "${pid}" 2>/dev/null; then
        kill -KILL "${pid}" 2>/dev/null || true
      fi
    fi
    rm -f "${PID_FILE}" 2>/dev/null || true
  fi
}

kill_existing_daemon() {
  local target_pids=()
  if [[ -f "${DAEMON_PID_FILE}" ]]; then
    local dpid
    dpid=$(cat "${DAEMON_PID_FILE}" 2>/dev/null || true)
    if [[ -n "${dpid}" && -r "/proc/${dpid}/cmdline" ]]; then
      local cmdline
      cmdline=$(tr '\0' ' ' < "/proc/${dpid}/cmdline" 2>/dev/null || true)
      if [[ ! "${cmdline}" =~ [[:space:]]-c[[:space:]] ]] && \
         [[ "${cmdline}" =~ (^|[[:space:]])([^[:space:]]*/)?clipboard-daemon ]]; then
        target_pids+=("${dpid}")
      fi
    fi
  fi

  local pgrep_pids
  pgrep_pids=$(pgrep -f "clipboard-daemon.*${PORT}" 2>/dev/null | grep -v -E "^($$|${PPID})$" || true)
  for p in ${pgrep_pids}; do
    if [[ -r "/proc/${p}/cmdline" ]]; then
      local cmdline
      cmdline=$(tr '\0' ' ' < "/proc/${p}/cmdline" 2>/dev/null || true)
      if [[ ! "${cmdline}" =~ [[:space:]]-c[[:space:]] ]] && \
         [[ "${cmdline}" =~ (^|[[:space:]])([^[:space:]]*/)?clipboard-daemon ]]; then
        target_pids+=("${p}")
      fi
    fi
  done

  for pid in "${target_pids[@]}"; do
    [[ -z "${pid}" ]] && continue
    kill -TERM "${pid}" 2>/dev/null || true
    for _ in {1..20}; do
      kill -0 "${pid}" 2>/dev/null || break
      sleep 0.1
    done
    if kill -0 "${pid}" 2>/dev/null; then
      kill -9 "${pid}" 2>/dev/null || true
    fi
  done

  rm -f "${DAEMON_PID_FILE}" 2>/dev/null || true
  kill_existing_tunnel
  fuser -k "${PORT}/tcp" 2>/dev/null || true
}

exit_code=0
cleanup() {
  local code="${1:-${exit_code}}"
  trap - EXIT SIGINT SIGTERM
  if [[ -n "${TUNNEL_PID:-}" ]]; then
    kill "${TUNNEL_PID}" 2>/dev/null || true
  fi
  kill_existing_tunnel
  if [[ -f "${DAEMON_PID_FILE}" ]]; then
    local current_pid
    current_pid=$(cat "${DAEMON_PID_FILE}" 2>/dev/null || true)
    if [[ "${current_pid}" == "$$" ]]; then
      rm -f "${DAEMON_PID_FILE}" 2>/dev/null || true
    fi
  fi
  exec 200>&- 2>/dev/null || true
  exit "${code}"
}

if [[ "${RESTART}" -eq 1 ]]; then
  echo "[$(get_timestamp)] Restarting clipboard daemon and tunnel on port ${PORT}..." >> "${LOG_FILE}"
  echo "Restarting clipboard daemon and tunnel on port ${PORT}..."

  if systemctl --user is-active --quiet "clipboard-daemon@${PORT}.service" 2>/dev/null; then
    systemctl --user restart "clipboard-daemon@${PORT}.service"
    exit 0
  elif [[ "${PORT}" == "${DEFAULT_PORT}" ]] && \
       systemctl --user is-active --quiet "clipboard-daemon.service" 2>/dev/null; then
    systemctl --user restart "clipboard-daemon.service"
    exit 0
  fi

  kill_existing_daemon
  flock -w 5 200 || { echo "Failed to acquire lock on ${LOCK_FILE}" >&2; cleanup 1; }
else
  flock -n 200 || { echo "clipboard-daemon already running on port ${PORT}" >&2; exit 0; }
fi

echo "$$" > "${DAEMON_PID_FILE}"
trap 'cleanup 130' SIGINT
trap 'cleanup 143' SIGTERM
trap 'cleanup 0' EXIT

# Start reverse SSH tunnel in background keeper loop with gcert-aware polling
start_ssh_tunnel() {
  kill_existing_tunnel
  local parent_pid=$$
  (
    exec 200>&-
    local gcert_warned=0
    local consecutive_drain_retries=0
    local persistent_conflict=0
    local reconnect_backoff=5
    local ssh_child_pid=""

    subshell_cleanup() {
      if [[ -n "${ssh_child_pid}" ]] && kill -0 "${ssh_child_pid}" 2>/dev/null; then
        kill -TERM "${ssh_child_pid}" 2>/dev/null || true
        sleep 0.1
        if kill -0 "${ssh_child_pid}" 2>/dev/null; then
          kill -KILL "${ssh_child_pid}" 2>/dev/null || true
        fi
      fi
      rm -f "${PID_FILE}" 2>/dev/null || true
      exit 0
    }
    trap subshell_cleanup SIGINT SIGTERM EXIT

    while true; do
      # Subshell parent liveness check
      if ! kill -0 "${parent_pid}" 2>/dev/null; then
        exit 0
      fi

      # 1. Check if an active local reverse SSH tunnel is already running
      # on this port
      local local_pids
      local_pids=($(find_local_tunnel_pids "${PORT}"))

      if [[ ${#local_pids[@]} -gt 0 ]]; then
        local ext_pid="${local_pids[0]}"
        echo "[$(get_timestamp)] [Tunnel] Active local reverse SSH tunnel detected (PID ${ext_pid}). Monitoring existing tunnel." >> "${LOG_FILE}"

        local stable_polls=0
        while kill -0 "${ext_pid}" 2>/dev/null; do
          if ! kill -0 "${parent_pid}" 2>/dev/null; then
            exit 0
          fi
          sleep 15
          ((stable_polls++)) || true
          if [[ ${stable_polls} -ge 4 ]]; then # > 60s
            consecutive_drain_retries=0
            persistent_conflict=0
            reconnect_backoff=5
          fi
        done
        echo "[$(get_timestamp)] [Tunnel] Monitored local SSH tunnel (PID ${ext_pid}) exited. Resuming keeper loop..." >> "${LOG_FILE}"
        continue
      fi

      # 2. Check gcert credentials before attempting connection
      if command -v gcertstatus &>/dev/null; then
        if ! gcertstatus --nocheck_loas2 --quiet 2>/dev/null; then
          if [[ ${gcert_warned} -eq 0 ]]; then
            echo "[$(get_timestamp)] [Tunnel] SSH credentials (gcert) expired or missing. Waiting for gcert..." >> "${LOG_FILE}"
            gcert_warned=1
          fi
          for _ in {1..15}; do
            kill -0 "${parent_pid}" 2>/dev/null || exit 0
            sleep 1
          done
          continue
        fi
      fi

      if [[ ${gcert_warned} -eq 1 ]]; then
        echo "[$(get_timestamp)] [Tunnel] Valid gcert detected. Connecting reverse SSH tunnel..." >> "${LOG_FILE}"
        gcert_warned=0
      fi

      # 3. Spawn daemon-managed SSH tunnel
      > "${ERR_LOG}"
      local start_time
      start_time=$(date +%s)

      ssh -N -T \
        -o ExitOnForwardFailure=yes \
        -o ServerAliveInterval=15 \
        -o ServerAliveCountMax=6 \
        -o TCPKeepAlive=no \
        -o ConnectTimeout=10 \
        -o ControlMaster=no \
        -o ControlPath=none \
        -R "${PORT}:127.0.0.1:${PORT}" \
        "${REMOTE_CLIPBOARD_HOST}" >> "${LOG_FILE}" 2> "${ERR_LOG}" &

      ssh_child_pid=$!
      echo "${ssh_child_pid}" > "${PID_FILE}"

      local exit_code=0
      wait "${ssh_child_pid}" 2>/dev/null || exit_code=$?
      local end_time
      end_time=$(date +%s)
      local duration=$(( end_time - start_time ))
      ssh_child_pid=""
      rm -f "${PID_FILE}" 2>/dev/null || true

      # 4. Evaluate exit error output and connection duration
      # Check for AFK / authentication timeout
      if grep -q -E "Permission denied|timed out waiting for user presence" "${ERR_LOG}" 2>/dev/null; then
        local auth_msg="SSH authentication timed out or was denied on ${REMOTE_CLIPBOARD_HOST}.
Gnubby touch prompt timed out. Pausing 5m before retry."
        echo "[$(get_timestamp)] [Tunnel] SSH authentication timeout or permission denied. Sending desktop notification and pausing 300s..." >> "${LOG_FILE}"
        send_desktop_notification "Clipboard Tunnel: Authentication Timeout" "${auth_msg}"
        for _ in {1..300}; do
          kill -0 "${parent_pid}" 2>/dev/null || exit 0
          sleep 1
        done
        consecutive_drain_retries=0
        reconnect_backoff=5
        continue
      fi

      # Check for remote port forwarding collisions
      if grep -q "remote port forwarding failed" "${ERR_LOG}" 2>/dev/null; then
        local current_local_pids
        current_local_pids=($(find_local_tunnel_pids "${PORT}"))
        if [[ ${#current_local_pids[@]} -gt 0 ]]; then
          echo "[$(get_timestamp)] [Tunnel] Port taken by local process (PID ${current_local_pids[0]}). Attaching to monitor." >> "${LOG_FILE}"
          consecutive_drain_retries=0
          persistent_conflict=0
          reconnect_backoff=5
          continue
        fi

        ((consecutive_drain_retries++)) || true
        if [[ ${consecutive_drain_retries} -eq 1 ]]; then
          echo "[$(get_timestamp)] [Tunnel] Remote port forwarding failed (attempt 1/3). Waiting 15s for remote socket to clear..." >> "${LOG_FILE}"
          for _ in {1..15}; do
            kill -0 "${parent_pid}" 2>/dev/null || exit 0
            sleep 1
          done
        elif [[ ${consecutive_drain_retries} -eq 2 ]]; then
          echo "[$(get_timestamp)] [Tunnel] Remote port forwarding failed (attempt 2/3). Waiting 30s for remote socket to clear..." >> "${LOG_FILE}"
          for _ in {1..30}; do
            kill -0 "${parent_pid}" 2>/dev/null || exit 0
            sleep 1
          done
        else
          # Consecutive retries >= 3
          if [[ ${persistent_conflict} -eq 0 ]]; then
            local conflict_msg="Port ${PORT} on ${REMOTE_CLIPBOARD_HOST} is in use by another session.
Pausing 60s for remote socket teardown before retrying.

Logs: ${LOG_FILE}
To fix manually: Run ~/bin/clear-clipboard-port ${PORT} on ${REMOTE_CLIPBOARD_HOST}"
            echo "[$(get_timestamp)] [Tunnel] Port ${PORT} conflict on ${REMOTE_CLIPBOARD_HOST} after 3 attempts. First incident; alerting and pausing 60s..." >> "${LOG_FILE}"
            send_desktop_notification "Clipboard Tunnel: Port ${PORT} Conflict" "${conflict_msg}"
            for _ in {1..60}; do
              kill -0 "${parent_pid}" 2>/dev/null || exit 0
              sleep 1
            done
            consecutive_drain_retries=0
            persistent_conflict=1
          else
            local persistent_msg="Persistent port ${PORT} conflict on ${REMOTE_CLIPBOARD_HOST}.
Pausing 10 minutes to prevent gnubby prompt storm.

Logs: ${LOG_FILE}
To fix: Close conflicting sessions on ${REMOTE_CLIPBOARD_HOST} or run ~/bin/clear-clipboard-port ${PORT}, then:
systemctl --user restart clipboard-daemon@${PORT}"
            echo "[$(get_timestamp)] [Tunnel] Persistent port ${PORT} conflict on ${REMOTE_CLIPBOARD_HOST}. Pausing 600s..." >> "${LOG_FILE}"
            send_desktop_notification "Clipboard Tunnel: Persistent Port ${PORT} Conflict" "${persistent_msg}"
            for _ in {1..600}; do
              kill -0 "${parent_pid}" 2>/dev/null || exit 0
              sleep 1
            done
            consecutive_drain_retries=0
          fi
        fi
        continue
      fi

      # Non-conflict drop
      if [[ ${duration} -ge 60 ]]; then
        consecutive_drain_retries=0
        persistent_conflict=0
        reconnect_backoff=5
        echo "[$(get_timestamp)] [Tunnel] SSH tunnel exited after stable connection (${duration}s, code ${exit_code}). Waiting 15s socket drain buffer..." >> "${LOG_FILE}"
        for _ in {1..15}; do
          kill -0 "${parent_pid}" 2>/dev/null || exit 0
          sleep 1
        done
      else
        echo "[$(get_timestamp)] [Tunnel] SSH tunnel exited (code ${exit_code}, ran for ${duration}s). Reconnecting in ${reconnect_backoff}s..." >> "${LOG_FILE}"
        for _ in $(seq 1 "${reconnect_backoff}"); do
          kill -0 "${parent_pid}" 2>/dev/null || exit 0
          sleep 1
        done
        if [[ ${reconnect_backoff} -lt 15 ]]; then
          reconnect_backoff=$(( reconnect_backoff + 5 ))
        elif [[ ${reconnect_backoff} -lt 30 ]]; then
          reconnect_backoff=30
        fi
      fi
    done
  ) &
  TUNNEL_PID=$!
}

# Start the reverse SSH tunnel
start_ssh_tunnel

echo "[$(get_timestamp)] Started clipboard-daemon on ${HOST}:${PORT} (Tunnel to ${REMOTE_CLIPBOARD_HOST}:${PORT})" >> "${LOG_FILE}"
echo "Started clipboard-daemon on ${HOST}:${PORT} (Tunnel to ${REMOTE_CLIPBOARD_HOST}:${PORT})"

while true; do
  displays=$(ls /tmp/.X11-unix/ 2>/dev/null | sed 's/X//g')
  readarray -t displays_array <<< "${displays}"

  # Wait for next payload from remote tmux via netcat
  paste_buffer=$(nc -l "${HOST}" "${PORT}" 200>&-)
  if [[ -z "${paste_buffer}" ]]; then
    sleep 0.5
    continue
  fi

  copied=0
  for display in "${displays_array[@]}"; do
    [[ -z "${display}" ]] && continue
    display_num="${display/:/}"
    display_num="${display_num/.0/}"
    if [[ "${display_num}" =~ ^[0-9]+$ ]] && [[ "${display_num}" -le 100 ]]; then
      printf '%s' "${paste_buffer}" | xclip -selection clipboard -display ":${display}" 200>&- 2>/dev/null
      if [[ $? -eq 0 ]]; then
        copied=1
      else
        echo "[$(get_timestamp)] Failed to copy contents to display :${display}" >> "${LOG_FILE}"
      fi
    fi
  done

  # Fallback to current $DISPLAY if set and not already copied
  if [[ -n "${DISPLAY:-}" ]] && [[ "${copied}" -eq 0 ]]; then
    printf '%s' "${paste_buffer}" | xclip -selection clipboard 200>&- 2>/dev/null && copied=1
  fi

  if [[ "${copied}" -eq 1 ]]; then
    echo "[$(get_timestamp)] Copied $(printf '%s' "${paste_buffer}" | wc -c) bytes to local clipboard." >> "${LOG_FILE}"
  fi
done
