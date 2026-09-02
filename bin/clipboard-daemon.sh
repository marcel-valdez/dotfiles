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
    notify-send -u critical -a "Clipboard Daemon" -i dialog-warning "${title}" "${body}" 2>/dev/null || true
    local displays
    displays=$(ls /tmp/.X11-unix/ 2>/dev/null | sed 's/X//g')
    for d in ${displays}; do
      [[ -n "${d}" ]] && DISPLAY=":${d}" notify-send -u critical -a "Clipboard Daemon" -i dialog-warning "${title}" "${body}" 2>/dev/null || true
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

kill_existing_tunnel() {
  pkill -f "ssh.*-R.*${PORT}:.*${REMOTE_HOST}" 2>/dev/null || true
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
  if ! pgrep -f "ssh.*-R.*${PORT}:.*${REMOTE_HOST}" >/dev/null; then
    echo "Reverse SSH tunnel was down; starting tunnel to ${REMOTE_HOST} on port ${PORT}..."
    start_ssh_tunnel
  else
    echo "Reverse SSH tunnel is also running."
  fi
  exit 0
fi

# Start reverse SSH tunnel in background keeper loop with gcert-aware polling
start_ssh_tunnel() {
  kill_existing_tunnel
  (
    gcert_warned=0
    while true; do
      # Gracefully handle when gcert is down or expired:
      # If gcertstatus indicates expired credentials, wait quietly without failing
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

      # -o ControlMaster=no -o ControlPath=none isolates daemon from interactive multiplex sockets
      ssh -N -T -R "${PORT}:127.0.0.1:${PORT}" \
          -o ControlMaster=no \
          -o ControlPath=none \
          -o ExitOnForwardFailure=yes \
          -o ServerAliveInterval=15 \
          -o ServerAliveCountMax=3 \
          -o ConnectTimeout=10 \
          "${REMOTE_HOST}" >> "${LOG_FILE}" 2>&1
      exit_code=$?

      if tail -n 10 "${LOG_FILE}" 2>/dev/null | grep -q "remote port forwarding failed"; then
        local conflict_msg="Port ${PORT} on ${REMOTE_HOST} is already in use by another session.

Logs: ${LOG_FILE}

To fix: Close the conflicting SSH session on ${REMOTE_HOST} and run:
systemctl --user restart clipboard-daemon@${PORT}"

        echo "[$(get_timestamp)] [Tunnel] Port ${PORT} is currently in use on ${REMOTE_HOST}." >> "${LOG_FILE}"
        echo "[$(get_timestamp)] [Tunnel] Sending desktop notification with log instructions." >> "${LOG_FILE}"
        send_desktop_notification "Clipboard Tunnel: Port ${PORT} Conflict" "${conflict_msg}"
        echo "[$(get_timestamp)] [Tunnel] Pausing tunnel creation until service restart." >> "${LOG_FILE}"
        sleep infinity
      else
        echo "[$(get_timestamp)] [Tunnel] SSH tunnel exited (code ${exit_code}). Reconnecting in 10s..." >> "${LOG_FILE}"
        sleep 10
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
