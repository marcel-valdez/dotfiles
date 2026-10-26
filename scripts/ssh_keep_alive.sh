#!/usr/bin/env bash
UNRETRIABLE_SUCCESS=129
UNRETRIABLE_ERROR=128

source "${HOME}/lib/log_lib.sh" || exit ${UNRETRIABLE_ERROR}
if [[ -f "${HOME}/lib/os_compat.sh" ]]; then
  source "${HOME}/lib/os_compat.sh"
fi

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
# Checks whether valid SSH/corp credentials currently exist without blocking.
#
# Arguments:
#   None
# Returns:
#   0 if gcertstatus is unavailable or credentials are valid; non-zero otherwise.
#
# Usage Example:
#   if is_credential_valid_quiet; then echo "valid"; fi
#######################################
is_credential_valid_quiet() {
  if ! command -v gcertstatus &>/dev/null; then
    return 0
  fi
  gcertstatus --nocheck_loas2 --ssh_cert_comment='corp/normal' --quiet
}

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
  if is_credential_valid_quiet; then
    return 0
  fi

  log::warn "[$(date +%H:%M:%S)] Credentials expired. Waiting for gcert renewal in another terminal... (or press 'g' to authenticate here)"

  local poll_interval="${GCERT_POLL_INTERVAL:-2}"

  while ! is_credential_valid_quiet; do
    local user_key=""
    if read -r -s -t "${poll_interval}" -n 1 user_key; then
      if [[ "${user_key}" == $'\e' ]]; then
        # Drain escape sequences (e.g. Kitty focus events ^[[I / ^[[O or arrow keys)
        while read -r -s -t 0.05 -n 1; do :; done
      elif [[ "${user_key}" == "g" || "${user_key}" == "G" ]]; then
        log::info "[$(date +%H:%M:%S)] Starting gcert authentication in this terminal..."
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

#######################################
# Monitors a running gmosh-client process in the background to record
# connection/disconnection timeline events and detect HTTP 502 responses when a
# remote gmosh-server has self-terminated after >24h of disconnection.
#
# When gorilla-RPC code 502 is detected in gmosh-client's warning log (which
# only occurs after SSODance succeeds with valid gcert and Wi-Fi), marks
# expired_flag_file and terminates gmosh-client so the foreground gmosh call
# exits raw curses mode and ssh_keep_alive.sh automatically reconnects.
#
# Arguments:
#   $1 - parent_pid: PID of the ssh_keep_alive.sh shell
#   $2 - timeline_file: Path to file recording timestamped timeline events
#   $3 - expired_flag_file: Path to flag file touched when HTTP 502 is detected
# Returns:
#   0 on clean termination.
#
# Usage Example:
#   monitor_gmosh_session "$$" "${timeline_file}" "${expired_flag_file}" &
#######################################
monitor_gmosh_session() {
  local parent_pid="$1"
  local timeline_file="$2"
  local expired_flag_file="$3"
  local watch_interval="${GMOSH_WATCH_INTERVAL:-2}"
  local self_pid="${BASHPID:-$$}"
  local last_state=""

  while kill -0 "${parent_pid}" 2>/dev/null; do
    sleep "${watch_interval}" || break

    local info_log=""
    local warn_log=""
    local gmosh_wrapper_pid=""
    local candidate_pid=""

    for candidate_pid in $(pgrep -P "${parent_pid}" 2>/dev/null || true); do
      if [[ "${candidate_pid}" != "${self_pid}" ]]; then
        gmosh_wrapper_pid="${candidate_pid}"
        break
      fi
    done

    local tree_pids=()
    if [[ -n "${gmosh_wrapper_pid}" ]]; then
      tree_pids+=("${gmosh_wrapper_pid}")
      local child_pid grandchild_pid
      for child_pid in $(pgrep -P "${gmosh_wrapper_pid}" 2>/dev/null || true); do
        tree_pids+=("${child_pid}")
        for grandchild_pid in $(pgrep -P "${child_pid}" 2>/dev/null || true); do
          tree_pids+=("${grandchild_pid}")
        done
      done
    fi

    if [[ -n "${GMOSH_TEST_LOG_DIR:-}" && -d "${GMOSH_TEST_LOG_DIR}" ]]; then
      info_log="$(ls -1t "${GMOSH_TEST_LOG_DIR}"/*.log.INFO.* 2>/dev/null | head -n 1 || true)"
      warn_log="$(ls -1t "${GMOSH_TEST_LOG_DIR}"/*.log.WARNING.* 2>/dev/null | head -n 1 || true)"
    else
      local inspect_pid target_path
      for inspect_pid in "${tree_pids[@]}"; do
        while IFS= read -r target_path; do
          [[ -z "${target_path}" ]] && continue
          if [[ "${target_path}" == *".log.INFO."* ]]; then
            info_log="${target_path}"
          elif [[ "${target_path}" == *".log.WARNING."* ]]; then
            warn_log="${target_path}"
          fi
        done < <(
          if declare -F os::pid_open_files >/dev/null 2>&1; then
            os::pid_open_files "${inspect_pid}"
          elif [[ -d "/proc/${inspect_pid}/fd" ]]; then
            local fd_link
            for fd_link in "/proc/${inspect_pid}/fd"/*; do
              readlink "${fd_link}" 2>/dev/null || true
            done
          fi
        )
      done
    fi

    if [[ -n "${info_log}" && -f "${info_log}" ]]; then
      if grep -q "Selected candidate pair changed" "${info_log}" 2>/dev/null; then
        if [[ "${last_state}" != "CONNECTED" ]] && ! grep -q "ICE transport connected" "${timeline_file}" 2>/dev/null; then
          echo "[$(date +%H:%M:%S)] Connected: ICE transport connected to gmosh-server." >> "${timeline_file}"
          last_state="CONNECTED"
        fi
      fi
    fi

    if [[ -n "${warn_log}" && -f "${warn_log}" ]]; then
      if grep -q "ICE connection disconnected" "${warn_log}" 2>/dev/null; then
        if [[ "${last_state}" != "ICE_DISCONNECTED" ]]; then
          echo "[$(date +%H:%M:%S)] Disconnected: ICE connection lost (Wi-Fi drop or network roam)." >> "${timeline_file}"
          last_state="ICE_DISCONNECTED"
        fi
      fi

      if grep -q "SSO credentials insufficient" "${warn_log}" 2>/dev/null; then
        if [[ "${last_state}" != "SSO_EXPIRED" ]]; then
          echo "[$(date +%H:%M:%S)] Auth Expired: SSO/gcert credentials expired during session." >> "${timeline_file}"
          last_state="SSO_EXPIRED"
        fi
      fi

      if grep -q "gorilla-RPC failed with code 502" "${warn_log}" 2>/dev/null; then
        echo "[$(date +%H:%M:%S)] Server Expired: Remote gmosh-server exited after >24h inactivity (HTTP 502)." >> "${timeline_file}"
        touch "${expired_flag_file}"
        local kill_pid
        for kill_pid in "${tree_pids[@]}"; do
          pkill -TERM -P "${kill_pid}" 2>/dev/null || true
          kill -TERM "${kill_pid}" 2>/dev/null || true
        done
        return 0
      fi
    fi
  done
}

#######################################
# Flushes any recorded gmosh session timeline events to the terminal log.
#
# Arguments:
#   $1 - timeline_file: Path to the timeline log file
# Returns:
#   0 always.
#
# Usage Example:
#   flush_session_timeline "${timeline_file}"
#######################################
flush_session_timeline() {
  local timeline_file="$1"
  if [[ -f "${timeline_file}" && -s "${timeline_file}" ]]; then
    log::info "[$(date +%H:%M:%S)] --- Session Connection Timeline ---"
    while IFS= read -r timeline_line; do
      log::info "  ${timeline_line}"
    done < "${timeline_file}"
    log::info "[$(date +%H:%M:%S)] -----------------------------------"
  fi
}

# Use a specific session name so you always land in the same place
MOSH_NET_TMOUT="${MOSH_SERVER_NETWORK_TMOUT:-86400}"
MOSH_SIG_TMOUT="${MOSH_SERVER_SIGNAL_TMOUT:-30}"
GMOSH_SERVER_CMD="env MOSH_SERVER_NETWORK_TMOUT=${MOSH_NET_TMOUT} MOSH_SERVER_SIGNAL_TMOUT=${MOSH_SIG_TMOUT} gmosh-server"

while true; do
    timeline_file=""
    expired_flag_file=""
    monitor_pid=""

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

        log::info "[$(date +%H:%M:%S)] Attempting to connect via gmosh to ${target_host} (idle server timeout: ${MOSH_NET_TMOUT}s)..."

        timeline_file="$(mktemp "/tmp/gmosh_timeline_XXXXXX")"
        expired_flag_file="${timeline_file}.expired"
        monitor_gmosh_session "$$" "${timeline_file}" "${expired_flag_file}" &
        monitor_pid="$!"

        # mosh requires remote command tokens passed separately so mosh-server can execvp correctly
        if [[ ${#cmd_args[@]} -eq 1 ]]; then
            read -r -a split_cmd <<< "${cmd_args[0]}"
            gmosh --server="${GMOSH_SERVER_CMD}" --ssh="ssh -o ConnectTimeout=5 -ttY" "${target_host}" -- "${split_cmd[@]}"
        elif [[ ${#cmd_args[@]} -gt 1 ]]; then
            gmosh --server="${GMOSH_SERVER_CMD}" --ssh="ssh -o ConnectTimeout=5 -ttY" "${target_host}" -- "${cmd_args[@]}"
        else
            gmosh --server="${GMOSH_SERVER_CMD}" --ssh="ssh -o ConnectTimeout=5 -ttY" "${target_host}"
        fi
        exit_code=$?

        if [[ -n "${monitor_pid}" ]]; then
            kill -TERM "${monitor_pid}" 2>/dev/null || true
            wait "${monitor_pid}" 2>/dev/null || true
        fi

        flush_session_timeline "${timeline_file}"

        if [[ -f "${expired_flag_file}" ]]; then
            rm -f "${timeline_file}" "${expired_flag_file}" 2>/dev/null || true
            log::warn "[$(date +%H:%M:%S)] Remote gmosh-server expired after >24h disconnect. Re-establishing new gmosh session to ${target_host}..."
            continue
        fi
        rm -f "${timeline_file}" "${expired_flag_file}" 2>/dev/null || true
    else
        if [[ "${USE_GCERT_GATE:-0}" == "1" ]] || [[ "$*" =~ (gcloud|\.corp\.google\.com|\.c\.googlers\.com) ]]; then
            wait_for_valid_credentials
        fi
        log::info "[$(date +%H:%M:%S)] Attempting to connect via ssh..."
        # -o ConnectTimeout=5 prevents the script from hanging if the network is still waking up
        ssh -o ConnectTimeout=5 "$@"
        exit_code=$?
    fi

    # If gmosh/ssh exited with 0, check whether credentials expired while idle
    # (meaning the server timed out or detached an unattended session).
    if [ "${exit_code}" -eq 0 ]; then
        if ! is_credential_valid_quiet; then
            log::warn "[$(date +%H:%M:%S)] Session detached or timed out while credentials were expired. Waiting to reconnect..."
            continue
        fi
        log::info "[$(date +%H:%M:%S)] Session exited normally."
        exit ${UNRETRIABLE_SUCCESS}
    fi

    log::warn "[$(date +%H:%M:%S)] Connection lost (Code ${exit_code}). Retrying in 3s... (Ctrl+C to stop)"
    # Flush focus-reporting garbage before sleeping
    while read -r -t 0.05; do :; done
    sleep 3
    # Flush focus-reporting garbage after sleeping
    while read -r -t 0.05; do :; done
done



