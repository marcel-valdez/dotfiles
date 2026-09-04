#!/usr/bin/env bash
# clipboard-daemon.sh: Listens locally for remote clipboard payloads and maintains
# a reverse SSH tunnel to gcloud so remote tmux copies sync to the local X11 clipboard.
# Designed to run as a user systemd service (clipboard-daemon.service).

HOST="127.0.0.1"
REMOTE_HOST="${REMOTE_HOST:-gcloud}"
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

# Determine PORT
if [[ -z "${PORT}" ]]; then
  if [[ -n "${CLIPBOARD_PORT:-}" ]]; then
    PORT="${CLIPBOARD_PORT}"
  else
    # Auto-detect default port by host type
    host_name="$(hostname -s 2>/dev/null || hostname)"
    if [[ "${host_name}" =~ laptop|glaptop ]]; then
      PORT=3334
    elif [[ "${host_name}" =~ mtv|workstation ]]; then
      PORT=3333
    else
      PORT=3333
    fi
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
    notify-send -u normal -t 600000 -a "Clipboard Daemon" -i dialog-warning "${title}" "${body}" 2>/dev/null || true
    local displays
    displays=$(ls /tmp/.X11-unix/ 2>/dev/null | sed 's/X//g')
    for d in ${displays}; do
      [[ -n "${d}" ]] && DISPLAY=":${d}" notify-send -u normal -t 600000 -a "Clipboard Daemon" -i dialog-warning "${title}" "${body}" 2>/dev/null || true
    done
  fi
}

kill_existing_daemon() {
  local pids
  pids=$(pgrep -f "clipboard-daemon.*${PORT}" 2>/dev/null | grep -v "^$$$" || true)
  if [[ -n "${pids}" ]]; then
    kill ${pids} 2>/dev/null || true
  fi
  fuser -k "${PORT}/tcp" 2>/dev/null || true
}

RUNTIME_DIR="${XDG_RUNTIME_DIR:-/tmp}"
PID_FILE="${RUNTIME_DIR}/clipboard-tunnel-${PORT}.pid"
ERR_LOG="${RUNTIME_DIR}/clipboard-ssh-${PORT}.log"

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
      kill -KILL "${pid}" 2>/dev/null || true
    fi
    rm -f "${PID_FILE}" 2>/dev/null || true
  fi
}

cleanup() {
  if [[ -n "${TUNNEL_PID:-}" ]]; then
    kill "${TUNNEL_PID}" 2>/dev/null || true
  fi
  kill_existing_tunnel
  exit 0
}
trap cleanup SIGINT SIGTERM

if [[ "${RESTART}" -eq 1 ]]; then
  echo "[$(get_timestamp)] Restarting clipboard daemon and tunnel on port ${PORT}..." >> "${LOG_FILE}"
  echo "Restarting clipboard daemon and tunnel on port ${PORT}..."
  kill_existing_daemon
  kill_existing_tunnel
  sleep 1
fi

# Check if another daemon is already actively listening on this port
if ss -tln | grep -q "${HOST}:${PORT} "; then
  echo "clipboard-daemon is already listening on ${HOST}:${PORT}."
  local_tunnels=($(find_local_tunnel_pids "${PORT}"))
  if [[ ${#local_tunnels[@]} -eq 0 ]]; then
    echo "Reverse SSH tunnel was down; starting tunnel to ${REMOTE_HOST} on port ${PORT}..."
    start_ssh_tunnel
  else
    echo "Reverse SSH tunnel is also running (PID ${local_tunnels[0]})."
  fi
  exit 0
fi

# Start reverse SSH tunnel in background keeper loop with gcert-aware polling
start_ssh_tunnel() {
  kill_existing_tunnel
  (
    local gcert_warned=0
    local consecutive_drain_retries=0
    local reconnect_backoff=5
    local ssh_child_pid=""

    subshell_cleanup() {
      if [[ -n "${ssh_child_pid}" ]] && kill -0 "${ssh_child_pid}" 2>/dev/null; then
        kill -TERM "${ssh_child_pid}" 2>/dev/null || true
        sleep 0.1
        kill -KILL "${ssh_child_pid}" 2>/dev/null || true
      fi
      rm -f "${PID_FILE}" 2>/dev/null || true
      exit 0
    }
    trap subshell_cleanup SIGINT SIGTERM EXIT

    while true; do
      # 1. Check if an active local reverse SSH tunnel is already running on this port
      local local_pids
      local_pids=($(find_local_tunnel_pids "${PORT}"))

      if [[ ${#local_pids[@]} -gt 0 ]]; then
        local ext_pid="${local_pids[0]}"
        echo "[$(get_timestamp)] [Tunnel] Active local reverse SSH tunnel detected (PID ${ext_pid}). Monitoring existing tunnel." >> "${LOG_FILE}"

        local stable_polls=0
        while kill -0 "${ext_pid}" 2>/dev/null; do
          sleep 15
          ((stable_polls++)) || true
          if [[ ${stable_polls} -ge 4 ]]; then # > 60s
            consecutive_drain_retries=0
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
          sleep 15
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

      ssh -N -T -R "${PORT}:127.0.0.1:${PORT}" \
          -o ControlMaster=no \
          -o ControlPath=none \
          -o ExitOnForwardFailure=yes \
          -o ServerAliveInterval=10 \
          -o ServerAliveCountMax=6 \
          -o ConnectTimeout=10 \
          "${REMOTE_HOST}" >> "${LOG_FILE}" 2> "${ERR_LOG}" &
      ssh_child_pid=$!
      echo "${ssh_child_pid}" > "${PID_FILE}"

      wait "${ssh_child_pid}" 2>/dev/null || true
      local exit_code=$?
      local end_time
      end_time=$(date +%s)
      local duration=$(( end_time - start_time ))
      ssh_child_pid=""
      rm -f "${PID_FILE}" 2>/dev/null || true

      if [[ ${duration} -ge 60 ]]; then
        consecutive_drain_retries=0
        reconnect_backoff=5
      fi

      # 4. Evaluate exit error output
      if grep -q "remote port forwarding failed" "${ERR_LOG}" 2>/dev/null; then
        local current_local_pids
        current_local_pids=($(find_local_tunnel_pids "${PORT}"))
        if [[ ${#current_local_pids[@]} -gt 0 ]]; then
          echo "[$(get_timestamp)] [Tunnel] Port taken by local process (PID ${current_local_pids[0]}). Attaching to monitor." >> "${LOG_FILE}"
          consecutive_drain_retries=0
          reconnect_backoff=5
          continue
        fi

        ((consecutive_drain_retries++)) || true
        if [[ ${consecutive_drain_retries} -le 6 ]]; then
          local drain_sleep=$(( consecutive_drain_retries * 5 ))
          echo "[$(get_timestamp)] [Tunnel] Remote port forwarding failed (attempt ${consecutive_drain_retries}/6). Waiting ${drain_sleep}s for remote socket to clear..." >> "${LOG_FILE}"
          sleep "${drain_sleep}"
        else
          local conflict_msg="Port ${PORT} on ${REMOTE_HOST} is already in use by another session.

Logs: ${LOG_FILE}

To fix: Close conflicting sessions on ${REMOTE_HOST} or run ~/bin/clear-clipboard-port, then:
systemctl --user restart clipboard-daemon@${PORT}"
          echo "[$(get_timestamp)] [Tunnel] Persistent port ${PORT} conflict on ${REMOTE_HOST} after 6 retries." >> "${LOG_FILE}"
          echo "[$(get_timestamp)] [Tunnel] Sending desktop notification with log instructions." >> "${LOG_FILE}"
          send_desktop_notification "Clipboard Tunnel: Port ${PORT} Conflict" "${conflict_msg}"
          echo "[$(get_timestamp)] [Tunnel] Pausing tunnel creation for 10 minutes before re-checking..." >> "${LOG_FILE}"
          sleep 600
          consecutive_drain_retries=0
        fi
      else
        echo "[$(get_timestamp)] [Tunnel] SSH tunnel exited (code ${exit_code}, ran for ${duration}s). Reconnecting in ${reconnect_backoff}s..." >> "${LOG_FILE}"
        sleep "${reconnect_backoff}"
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

echo "[$(get_timestamp)] Started clipboard-daemon on ${HOST}:${PORT} (Tunnel to ${REMOTE_HOST}:${PORT})" >> "${LOG_FILE}"
echo "Started clipboard-daemon on ${HOST}:${PORT} (Tunnel to ${REMOTE_HOST}:${PORT})"

while true; do
  displays=$(ls /tmp/.X11-unix/ 2>/dev/null | sed 's/X//g')
  readarray -t displays_array <<< "${displays}"

  # Wait for next payload from remote tmux via netcat
  paste_buffer=$(nc -l "${HOST}" "${PORT}")
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
      echo -n "${paste_buffer}" | xclip -selection clipboard -display ":${display}" 2>/dev/null
      if [[ $? -eq 0 ]]; then
        copied=1
      else
        echo "[$(get_timestamp)] Failed to copy contents to display :${display}" >> "${LOG_FILE}"
      fi
    fi
  done

  # Fallback to current $DISPLAY if set and not already copied
  if [[ -n "${DISPLAY:-}" ]] && [[ "${copied}" -eq 0 ]]; then
    echo -n "${paste_buffer}" | xclip -selection clipboard 2>/dev/null && copied=1
  fi

  if [[ "${copied}" -eq 1 ]]; then
    echo "[$(get_timestamp)] Copied $(echo -n "${paste_buffer}" | wc -c) bytes to local clipboard." >> "${LOG_FILE}"
  fi
done
