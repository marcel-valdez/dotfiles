#!/usr/bin/env bash
# jetski_hook_utils.sh: Centralized shared utilities, atomic state management,
# and threshold constants for Jetski agent hooks.

# Log settings
export LOG_SCRIPT_NAME="${LOG_SCRIPT_NAME:-$(basename "$0")}"
[[ -z "${LOG_LEVEL}" ]] && export LOG_LEVEL=2
[[ -z "${LOG_FILE}" ]] && export LOG_FILE="/tmp/jetski_hook.log"

if [[ -f "${HOME}/lib/log_lib.sh" ]]; then
  source "${HOME}/lib/log_lib.sh"
else
  function log::debug { :; }
  function log::info { :; }
  function log::warn { :; }
  function log::error { echo "[ERROR] $*" >&2; }
  function log::fatal { echo "[FATAL] $*" >&2; exit 1; }
fi

# Threshold constants
JETSKI_STOP_NOTIFY_THRESHOLD_SECS="${JETSKI_STOP_NOTIFY_THRESHOLD_SECS:-60}"
JETSKI_APPROVAL_NOTIFY_THRESHOLD_SECS="${JETSKI_APPROVAL_NOTIFY_THRESHOLD_SECS:-0}"
JETSKI_TOOL_NOTIFY_THRESHOLD_SECS="${JETSKI_TOOL_NOTIFY_THRESHOLD_SECS:-300}"
JETSKI_ENABLE_OSC99="${JETSKI_ENABLE_OSC99:-true}"
JETSKI_BRAIN_BASE="${JETSKI_BRAIN_BASE:-${HOME}/.gemini/jetski}"

# Environment & tmux state
TMUX_SESSION="Unknown"
TMUX_WINDOW="Unknown"
TMUX_PANE=""
TARGET_PANE=""
TARGET_TTY=""

function dispatch {
  log::debug "dispatch $*"
  "$@" &>/dev/null & disown
}

function run {
  log::debug "run $*"
  "$@"
}

function get_now_seconds {
  echo "${JETSKI_MOCK_NOW:-${EPOCHSECONDS:-$(date +%s)}}"
}

function get_session_state_dir {
  local ppid="${1:-${JETSKI_PPID:-${PPID}}}"
  local dir="/tmp/jetski_${UID}_${ppid}"
  if [[ ! -d "${dir}" ]]; then
    mkdir -p -m 0700 "${dir}" 2>/dev/null || mkdir -p "${dir}" 2>/dev/null
  fi
  echo "${dir}"
}

function write_atomic_state {
  local target_path="$1"
  local content="$2"
  local dir
  dir="$(dirname "${target_path}")"
  if [[ ! -d "${dir}" ]]; then
    mkdir -p -m 0700 "${dir}" 2>/dev/null || mkdir -p "${dir}" 2>/dev/null
  fi
  local tmp_path="${target_path}.tmp.$$.${RANDOM}"
  printf '%s\n' "${content}" > "${tmp_path}"
  mv -f "${tmp_path}" "${target_path}"
}

function read_numeric_state {
  local target_path="$1"
  local fallback="${2:-}"
  if [[ -f "${target_path}" ]]; then
    local val
    val="$(head -n 1 "${target_path}" 2>/dev/null | tr -d '[:space:]')"
    if [[ -n "${val}" && "${val}" =~ ^[0-9]+$ ]]; then
      echo "${val}"
      return 0
    fi
  fi
  if [[ -n "${fallback}" ]]; then
    echo "${fallback}"
  fi
  return 1
}

function read_request_origin_state {
  local target_path="$1"
  if [[ -f "${target_path}" ]]; then
    local line
    line="$(head -n 1 "${target_path}" 2>/dev/null)"
    local st_time prompt_hash
    read -r st_time prompt_hash <<< "${line}"
    if [[ -n "${st_time}" && "${st_time}" =~ ^[0-9]+$ ]]; then
      echo "${st_time} ${prompt_hash}"
      return 0
    fi
  fi
  return 1
}

function is_main_conversation {
  local cid="$1"
  local transcript_path="${2:-}"
  local ppid="${3:-${JETSKI_PPID:-${PPID}}}"

  # 0. Subagent transcript isolation
  if [[ -n "${transcript_path}" && "${transcript_path}" =~ /subagents?/ ]]; then
    return 1
  fi

  local brain_base="${JETSKI_BRAIN_BASE}"

  # 1. Check if the conversation has an annotation file
  if [[ -f "${brain_base}/annotations/${cid}.pbtxt" ]]; then
    return 0
  fi

  # 2. Check if parent CLI process command line explicitly specifies this conversation ID
  local cli_args
  cli_args=$(run ps -p "${ppid}" -o args= 2>/dev/null)
  if [[ "${cli_args}" == *"--conversation=${cid}"* ]]; then
    return 0
  fi

  # 3. Check if session-scoped cli_main.txt matches this conversation ID
  local state_dir
  state_dir=$(get_session_state_dir "${ppid}")
  local cli_tracker="${state_dir}/cli_main.txt"
  if [[ -f "${cli_tracker}" ]]; then
    local tracked_cid
    tracked_cid="$(head -n 1 "${cli_tracker}" 2>/dev/null | tr -d '[:space:]')"
    if [[ -n "${tracked_cid}" && "${tracked_cid}" == "${cid}" ]]; then
      return 0
    fi
  fi

  return 1
}

function is_subagent_conversation {
  local cid="$1"
  local transcript_path="${2:-}"
  local ppid="${3:-${JETSKI_PPID:-${PPID}}}"

  if [[ -n "${transcript_path}" && "${transcript_path}" =~ /subagents?/ ]]; then
    return 0
  fi

  if is_main_conversation "${cid}" "${transcript_path}" "${ppid}"; then
    return 1
  fi

  return 0
}

function init_cli_main_session {
  local cid="$1"
  local transcript_path="${2:-}"
  local ppid="${3:-${JETSKI_PPID:-${PPID}}}"

  if [[ -n "${transcript_path}" && "${transcript_path}" =~ /subagents?/ ]]; then
    return 1
  fi

  local state_dir
  state_dir=$(get_session_state_dir "${ppid}")
  write_atomic_state "${state_dir}/cli_main.txt" "${cid}"
  return 0
}

function populate_tmux_info {
  local ppid="${1:-${JETSKI_PPID:-${PPID}}}"
  local cli_tty
  cli_tty=$(run ps -p "${ppid}" -o tty= 2>/dev/null | run awk '{print $1}')

  if [[ -z "${cli_tty}" || "${cli_tty}" == "?" ]]; then
    if [[ -n "${TMUX:-}" ]]; then
      cli_tty=$(run tmux display-message -p '#{pane_tty}' 2>/dev/null)
      cli_tty="${cli_tty#/dev/}"
    fi
  fi

  if [[ -n "${cli_tty}" && "${cli_tty}" != "?" ]]; then
    local full_tty="/dev/${cli_tty#/dev/}"
    export TARGET_TTY="${full_tty}"

    local tmux_info
    tmux_info=$(run tmux list-panes -a -F $'#{pane_tty}\t#{session_name}\t#{window_name}\t#{pane_id}' 2>/dev/null | grep "^${full_tty}"$'\t' | head -n 1)
    if [[ -n "${tmux_info}" ]]; then
      local p_tty s_name w_name p_id
      IFS=$'\t' read -r p_tty s_name w_name p_id <<< "${tmux_info}"
      export TMUX_SESSION="${s_name:-Unknown}"
      export TMUX_WINDOW="${w_name:-Unknown}"
      export TMUX_PANE="${p_id:-}"
      export TARGET_PANE="${p_id:-}"
    fi
  fi

  if [[ -n "${TMUX:-}" && "${TMUX_SESSION}" == "Unknown" ]]; then
    local curr_info
    curr_info=$(run tmux display-message -p $'#{pane_tty}\t#{session_name}\t#{window_name}\t#{pane_id}' 2>/dev/null)
    if [[ -n "${curr_info}" ]]; then
      local p_tty s_name w_name p_id
      IFS=$'\t' read -r p_tty s_name w_name p_id <<< "${curr_info}"
      export TMUX_SESSION="${s_name:-Unknown}"
      export TMUX_WINDOW="${w_name:-Unknown}"
      export TMUX_PANE="${p_id:-}"
      export TARGET_PANE="${p_id:-}"
      if [[ -z "${TARGET_TTY}" && -n "${p_tty}" ]]; then
        export TARGET_TTY="${p_tty}"
      fi
    fi
  fi
}

function extract_approval_prompt {
  local payload="$1"
  run jq -r '
    .toolCall as $tc |
    ($tc.name // "") as $name |
    if ($name | test("(^|:)(ask_question)$")) then
      (($tc.args.questions | map(.question) | join(" | ")) // $tc.args.question // "[Question Required]")
    elif ($name | test("(^|:)(ask_permission)$")) then
      "[Permission: \($tc.args.Action // "action") \($tc.args.Target // "target")] \($tc.args.Reason // "")"
    elif ($name | test("(^|:)(ask_custom_permission)$")) then
      "[Custom Permission: \($tc.args.Grant // "grant")]"
    else
      ""
    end
  ' <<< "${payload}" 2>/dev/null
}

function dispatch_notification {
  local title="$1"
  local msg="$2"
  local urgency="${3:-normal}"

  if [[ -n "${JETSKI_TEST_NOTIFY_LOG:-}" ]]; then
    printf 'NOTIFY\t%s\t%s\t%s\n' "${title}" "${urgency}" "${msg}" >> "${JETSKI_TEST_NOTIFY_LOG}"
  fi

  # Attempt to use knock to notify
  if [[ -e /google/bin/releases/knock/knock.sh ]]; then
    source /google/bin/releases/knock/knock.sh &>/dev/null
    dispatch knock "${msg}"
  else
    local notified=""
    if run type tmux-notify &>/dev/null; then
      dispatch tmux-notify "${title}" "${msg}"
      notified=1
    fi
    if [[ -n "${DISPLAY:-}" ]] && run type notify-send &>/dev/null; then
      if [[ -n "${urgency}" ]]; then
        dispatch notify-send -u "${urgency}" "${title}" "${msg}"
      else
        dispatch notify-send "${title}" "${msg}"
      fi
      notified=1
    fi
    if [[ -z "${notified}" ]]; then
      log::debug "neither tmux-notify nor notify-send were available to notify the user."
    fi
  fi

  if [[ "${JETSKI_ENABLE_OSC99:-true}" == "true" && -f "${HOME}/scripts/osc99_notify.sh" ]]; then
    TARGET_TTY="${TARGET_TTY}" dispatch "${HOME}/scripts/osc99_notify.sh" "${title}" "${msg}"
  fi
}

function prune_dead_sessions {
  local max_age_secs="${1:-86400}"
  local lock_dir="/tmp/jetski_${UID}_prune.lock"
  if ! mkdir "${lock_dir}" 2>/dev/null; then
    local lock_mtime
    lock_mtime=$(stat -c %Y "${lock_dir}" 2>/dev/null || echo 0)
    local now
    now=$(get_now_seconds)
    if (( now - lock_mtime > 3600 )); then
      rmdir "${lock_dir}" 2>/dev/null
      mkdir "${lock_dir}" 2>/dev/null || return 0
    else
      return 0
    fi
  fi

  local session_dir
  for session_dir in /tmp/jetski_${UID}_*; do
    if [[ ! -d "${session_dir}" || "${session_dir}" == "${lock_dir}" ]]; then
      continue
    fi
    local pid="${session_dir##*_}"
    if [[ "${pid}" =~ ^[0-9]+$ ]]; then
      if ! kill -0 "${pid}" 2>/dev/null; then
        local dir_mtime
        dir_mtime=$(stat -c %Y "${session_dir}" 2>/dev/null || echo 0)
        local now
        now=$(get_now_seconds)
        if (( now - dir_mtime >= max_age_secs )); then
          log::info "Pruning dead session dir: ${session_dir}"
          rm -rf "${session_dir}" 2>/dev/null
        fi
      fi
    fi
  done

  rmdir "${lock_dir}" 2>/dev/null
}
