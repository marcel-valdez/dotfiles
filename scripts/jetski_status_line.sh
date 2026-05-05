#!/bin/bash
# set -euo pipefail
# set -u

export LOG_SCRIPT_NAME=
LOG_SCRIPT_NAME="$(basename "$0")"
[[ -z "${LOG_LEVEL:-}" ]] && export LOG_LEVEL=3
[[ -z "${LOG_FILE:-}" ]] && export LOG_FILE="/tmp/jetski_statusline.log"

source "${HOME}/lib/log_lib.sh"

function dispatch {
  log::debug "dispatch $*"
  "$@" &>/dev/null & disown
}

function run {
  log::debug "run $*"
  "$@"
}

# Read JSON payload from stdin
DATA=$(cat)
log::debug "PAYLOAD: $(echo ${DATA} | run jq --monochrome-output)"

# Extract fields using jq
eval $(echo "$DATA" | jq -r '
  "STATE=\"\(.agent_state // "idle")\"
   CWD=\"\(.workspace.current_dir // "")\"
   USED_PCT=\"\(.context_window.used_percentage // 0)\"
   COST=\"\(.cost.total_cost_usd // 0)\"
   VCS_BRANCH=\"\(.vcs.branch // "")\"
   VCS_DIRTY=\"\(.vcs.dirty // false)\"
   SANDBOX=\"\(.sandbox.enabled // false)\"
   TASKS_COUNT=\"\(.background_tasks | length)\"
   ARTIFACTS_COUNT=\"\(.artifacts | length)\"
   MODEL_NAME=\"\(.model.display_name // "")\"
   COLS=\"\(.terminal_width // 80)\"
   CONFIRMATION_DIALOG_PENDING=\"\(.tool_confirmation_pending // false)\"
   CONVERSATION_ID=\"\(.conversation_id // "")\"
  "
' 2>/dev/null || echo 'STATE="idle" CWD="" USED_PCT="0" COST="0" VCS_BRANCH="" VCS_DIRTY="false" SANDBOX="false" TASKS_COUNT="0" ARTIFACTS_COUNT="0" MODEL_NAME="" CONFIRMATION_DIALOG_PENDING="false" CONVERSATION_ID=""')

log::debug "STATE=${STATE}"
log::debug "CWD=${CWD}"
log::debug "USED_PCT=${USED_PCT}"
log::debug "COST=${COST}"
log::debug "VCS_BRANCH=${VCS_BRANCH}"
log::debug "VCS_DIRTY=${VCS_DIRTY}"
log::debug "SANDBOX=${SANDBOX}"
log::debug "TASKS_COUNT=${TASKS_COUNT}"
log::debug "ARTIFACTS_COUNT=${ARTIFACTS_COUNT}"
log::debug "MODEL_NAME=${MODEL_NAME}"
log::debug "CONFIRMATION_DIALOG_PENDING=${CONFIRMATION_DIALOG_PENDING:-}"
log::debug "CONVERSATION_ID=${CONVERSATION_ID:-}"
if [[ -z ${CONVERSATION_ID:-} ]]; then
  CONVERSATION_ID="${PPID}"
fi

DIALOG_TRACKER_FILE="/tmp/jetski_dialog_${CONVERSATION_ID}.txt"
run touch "${DIALOG_TRACKER_FILE}"

function populate_tmux_info {
  local cli_tty=
  cli_tty=$(run ps -p "${PPID}" -o tty= 2>/dev/null | run awk '{print $1}' 2>/dev/null)

  if [[ -n "${cli_tty}" ]] && [[ "${cli_tty}" != "?" ]]; then
    local full_tty="/dev/${cli_tty}"
    local tmux_info=
    tmux_info=$(run tmux list-panes -a -F '#{pane_tty} #{session_name} #{window_name}' 2>/dev/null | run grep "^${full_tty} " 2>/dev/null)
    if [[ -n "${tmux_info}" ]]; then
      export TMUX_SESSION=$(echo "${tmux_info}" | run awk '{print $2}' 2>/dev/null)
      log::debug "TMUX_SESSION=${TMUX_SESSION}"
      export TMUX_WINDOW=$(echo "${tmux_info}" | run awk '{print $3}' 2>/dev/null)
      log::debug "TMUX_WINDOW=${TMUX_WINDOW}"
    fi
  fi
}

DIALOG_NOTIFICATION_THRESHOLD_SECS=60
function handle_dialog_notification {
  local start_time=
  start_time="$(run cat "${DIALOG_TRACKER_FILE}")"
  local end_time=
  end_time=$(run date '+%s')
  local elapsed=
  elapsed=$((end_time-start_time))
  log::debug "elapsed: ${elapsed}"
  local should_notify=0
  if [ "${elapsed}" -ge "${DIALOG_NOTIFICATION_THRESHOLD_SECS}" ]; then
    should_notify=1
    # No more notifications after the threshold
    run echo -n 9999999999 > "${DIALOG_TRACKER_FILE}"
  elif [ ${elapsed} -eq 0 ] || [ ${elapsed} -eq 1 ]; then
    should_notify=1
    run echo -n $((start_time-2)) > "${DIALOG_TRACKER_FILE}"
  fi
     
  if [ ${should_notify} -eq 1 ]; then
    log::info "Notifying user about dialog"
    # TODO: Do all of this in a dispatched script, instead of doing it here inline.
    local title="Jetski CLI: User approval required"
    local msg=""
    populate_tmux_info
    dispatch "${HOME}/scripts/agent_notify.sh" "${title}" ""
  fi
}

# Handle async (fire & forget) processing
dialog_tracker_size=$(run stat "${DIALOG_TRACKER_FILE}" --format '%s' 2>/dev/null)
log::debug "dialog_tracker_size=${dialog_tracker_size}"
if [[ "${CONFIRMATION_DIALOG_PENDING}" == "true" ]]; then
  if [ ${dialog_tracker_size} -eq 0 ]; then
    date '+%s' > "${DIALOG_TRACKER_FILE}"
  fi
  handle_dialog_notification
elif [ ${dialog_tracker_size} -ne 0 ]; then
  log::debug "clearing ${DIALOG_TRACKER_FILE}"
  echo -n > "${DIALOG_TRACKER_FILE}"
fi

USED_PCT_FMT=$(printf "%.2f" "$USED_PCT")
USED_PCT_INT=${USED_PCT%.*}
USED_PCT_INT=${USED_PCT_INT:-0}

# Format state
case "$STATE" in
  idle)     STATE_FMT="\033[1;32m● READY\033[0m" ;;
  thinking) STATE_FMT="\033[1;33m◆ THINKING\033[0m" ;;
  working)  STATE_FMT="\033[1;36m⚙ WORKING\033[0m" ;;
  tool_use) STATE_FMT="\033[1;35m🛠 TOOL USE\033[0m" ;;
  *)        STATE_FMT="\033[2m⏳ ${STATE}\033[0m" ;;
esac

# Format VCS
VCS_FMT=""
if [ -n "$VCS_BRANCH" ]; then
  if [ "$VCS_DIRTY" = "true" ]; then
    VCS_FMT=" \033[31m $VCS_BRANCH*\033[0m"
  else
    VCS_FMT=" \033[34m $VCS_BRANCH\033[0m"
  fi
fi

# Format Sandbox
SANDBOX_FMT="\033[90mSandbox: OFF\033[0m"
if [ "$SANDBOX" = "true" ]; then
  SANDBOX_FMT="\033[32mSandbox: ON\033[0m"
fi

# Format Context usage bar
CONTEXT_PCT_TRACKER_FILE="/tmp/jetski_context_pct_${CONVERSATION_ID}.txt"
if ! [[ -f "${CONTEXT_PCT_TRACKER_FILE}" ]]; then
  echo "${USED_PCT_INT}" > "${CONTEXT_PCT_TRACKER_FILE}"
fi

PREV_USED_PCT_INT="$(cat ${CONTEXT_PCT_TRACKER_FILE})"
BAR_COLOR="\033[32m"
if [ "${USED_PCT_INT}" -ge 85 ]; then
  if [ "${PREV_USED_PCT_INT}" -lt 85 ]; then
    # Track that a higher milestone was reached.
    echo "${USED_PCT_INT}" > "${CONTEXT_PCT_TRACKER_FILE}"
    # TODO: NOTIFY USER CONTEXT REACHED 85%
  fi
  BAR_COLOR="\033[31m"
elif [ "${USED_PCT_INT}" -ge 60 ]; then
  if [ "${PREV_USED_PCT_INT}" -ge 85 ]; then
    # Track that a lower milestone was reached.
    echo "${USED_PCT_INT}" > "${CONTEXT_PCT_TRACKER_FILE}"
    # No notification
  elif [ "${PREV_USED_PCT_INT}" -lt 60 ]; then
    # Tracke that a higher milestone was reached.
    echo "${USED_PCT_INT}" > "${CONTEXT_PCT_TRACKER_FILE}"
    # TODO: NOTIFY USER CONTEXT REACHED 60%
  fi
  BAR_COLOR="\033[33m"
elif [ "$USED_PCT_INT" -ge 35 ]; then
  if [ "${PREV_USED_PCT_INT}" -ge 60 ]; then
    # Track that a lower milestone was reached
    echo "${USED_PCT_INT}" > "${CONTEXT_PCT_TRACKER_FILE}"
    # No notification
  elif [ "${PREV_USED_PCT_INT}" -lt 35 ]; then
    # Track that a higher milestone was reached.
    echo "${USED_PCT_INT}" > "${CONTEXT_PCT_TRACKER_FILE}"
  fi
elif [ "${USED_PCT_INT}" -le 25 ]; then
  if [ "${PREV_USED_PCT_INT}" -ge 35 ]; then
    # Track that a lower milestone was reached
    echo "${USED_PCT_INT}" > "${CONTEXT_PCT_TRACKER_FILE}"
    # No notification
  fi
fi


BAR=""
TOTAL_BARS=10
FILLED_BARS=$((USED_PCT_INT / 10))
for ((i=0; i<FILLED_BARS; i++)); do BAR="${BAR}▰"; done
for ((i=FILLED_BARS; i<TOTAL_BARS; i++)); do BAR="${BAR}▱"; done

# Format width
if [ "$COLS" -ge 100 ]; then
  echo -e " $STATE_FMT \033[90m│\033[0m \033[35m$MODEL_NAME\033[0m$VCS_FMT \033[90m│\033[0m $SANDBOX_FMT \033[90m│\033[0m \033[90mContext:\033[0m ${BAR_COLOR}${BAR}\033[0m \033[33m${USED_PCT_FMT}%\033[0m \033[90m│\033[0m Tasks: \033[36m$TASKS_COUNT\033[0m \033[90m│\033[0m Artifacts: \033[35m$ARTIFACTS_COUNT\033[0m"
else
  echo -e "\033[90m╭─\033[0m $STATE_FMT \033[90m│\033[0m \033[35m$MODEL_NAME\033[0m$VCS_FMT \033[90m│\033[0m $SANDBOX_FMT"
  echo -e "\033[90m╰─\033[0m \033[90mContext:\033[0m ${BAR_COLOR}${BAR}\033[0m \033[33m${USED_PCT_FMT}%\033[0m \033[90m│\033[0m Tasks: \033[36m$TASKS_COUNT\033[0m \033[90m│\033[0m Artifacts: \033[35m$ARTIFACTS_COUNT\033[0m"
fi
