#!/bin/bash
# set -euo pipefail
# set -u

# Jetski CLI status line, set with `/statusline ~/scripts/jetski_status_line.sh`.
# Renders the JSON payload the CLI sends on stdin and sends a desktop
# notification when a tool approval is pending and when the context window
# crosses 60% and 85%. Runs on Linux and on macOS (/bin/bash 3.2, BSD tools).

export LOG_SCRIPT_NAME=
LOG_SCRIPT_NAME="$(basename "$0")"
[[ -z "${LOG_LEVEL:-}" ]] && export LOG_LEVEL=2
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

# Every log::debug call forks and the payload dump runs jq, on each refresh, so
# only do them when debug logging is on (LOG_LEVEL=3, or the deprecated DEBUG).
function debug_enabled {
  [[ "${LOG_LEVEL}" -ge 3 ]] || [[ -n "${DEBUG:-}" ]]
}

# Read JSON payload from stdin
DATA=$(cat)
if debug_enabled; then
  log::debug "PAYLOAD: $(echo "${DATA}" | run jq --monochrome-output)"
fi

# Extract fields using jq. These defaults stay when the payload is empty or not
# valid JSON.
STATE='idle' USED_PCT='0' VCS_TYPE='' VCS_BRANCH='' VCS_DIRTY='false' SANDBOX='false' TASKS_COUNT='0'
ARTIFACTS_COUNT='0' MODEL_NAME='' COLS='80' CONFIRMATION_DIALOG_PENDING='false' CONVERSATION_ID='' CWD=''
eval "$(echo "$DATA" | jq -r '
  "STATE=\(.agent_state // "idle" | @sh)
   USED_PCT=\(.context_window.used_percentage // 0 | @sh)
   VCS_TYPE=\(.vcs.type // "" | @sh)
   VCS_BRANCH=\(.vcs.branch // "" | @sh)
   VCS_DIRTY=\(.vcs.dirty // false | @sh)
   SANDBOX=\(.sandbox.enabled // false | @sh)
   TASKS_COUNT=\(.task_count // 0 | @sh)
   ARTIFACTS_COUNT=\(.artifact_count // 0 | @sh)
   MODEL_NAME=\(.model.display_name // "" | @sh)
   COLS=\(.terminal_width // 80 | @sh)
   CONFIRMATION_DIALOG_PENDING=\(.tool_confirmation_pending // false | @sh)
   CONVERSATION_ID=\(.conversation_id // "" | @sh)
   CWD=\(.workspace.current_dir // .cwd // "" | @sh)
  "
' 2>/dev/null)"

# CitC and Fig clients live under /google/src/cloud/<user>/<workspace>: show the
# workspace (with its owner when it isn't yours) instead of a branch, tagged fig
# when the client has an hg repo, else citc. Path-only, so no FUSE-heavy calls.
VCS_WORKSPACE=''
CITC_PATH_RE='^/google/src/cloud/([^/]+)/([^/]+)'
if [[ "${CWD}" =~ ${CITC_PATH_RE} ]]; then
  citc_owner="${BASH_REMATCH[1]}"
  citc_root="/google/src/cloud/${citc_owner}/${BASH_REMATCH[2]}"
  VCS_WORKSPACE="${BASH_REMATCH[2]}"
  if [[ "${citc_owner}" != "${USER:-$(id -un 2>/dev/null)}" ]]; then
    VCS_WORKSPACE="${citc_owner}/${VCS_WORKSPACE}"
  fi
  if [[ "${VCS_TYPE}" == "hg" || "${VCS_TYPE}" == "fig" || -d "${citc_root}/.hg" || -d "${citc_root}/google3/.hg" ]]; then
    VCS_WORKSPACE="fig:${VCS_WORKSPACE}"
  else
    VCS_WORKSPACE="citc:${VCS_WORKSPACE}"
  fi
fi

# Newer CLIs send only vcs.type, so ask git for the branch (the short commit
# when detached) and whether tracked files have uncommitted changes. Skipping
# untracked files and optional locks keeps it fast (~20ms) and out of the way.
if [[ -z "${VCS_WORKSPACE}" ]] && [[ -z "${VCS_BRANCH}" ]] && [[ -z "${VCS_TYPE}" || "${VCS_TYPE}" == "git" ]] &&
   [[ -d "${CWD}" ]]; then
  VCS_BRANCH=$(git -C "${CWD}" symbolic-ref --quiet --short HEAD 2>/dev/null ||
    git -C "${CWD}" rev-parse --short HEAD 2>/dev/null)
  if [[ -n "${VCS_BRANCH}" ]] &&
     [[ -n "$(git --no-optional-locks -C "${CWD}" status --porcelain --untracked-files=no 2>/dev/null)" ]]; then
    VCS_DIRTY='true'
  fi
fi

if debug_enabled; then
  log::debug "STATE=${STATE} USED_PCT=${USED_PCT} VCS_TYPE=${VCS_TYPE} VCS_WORKSPACE=${VCS_WORKSPACE} VCS_BRANCH=${VCS_BRANCH} VCS_DIRTY=${VCS_DIRTY}" \
    "SANDBOX=${SANDBOX} TASKS_COUNT=${TASKS_COUNT} ARTIFACTS_COUNT=${ARTIFACTS_COUNT}" \
    "MODEL_NAME=${MODEL_NAME} COLS=${COLS} CONFIRMATION_DIALOG_PENDING=${CONFIRMATION_DIALOG_PENDING}" \
    "CONVERSATION_ID=${CONVERSATION_ID}"
fi
if [[ -z ${CONVERSATION_ID:-} ]]; then
  CONVERSATION_ID="${PPID}"
fi

# Per-conversation tracker files. When a conversation starts being tracked,
# delete the ones untouched for a week (the trailing slash makes find descend
# into macOS's /tmp, which is a symlink to /private/tmp).
STATE_DIR="${JETSKI_STATUSLINE_STATE_DIR:-/tmp}"
DIALOG_TRACKER_FILE="${STATE_DIR}/jetski_dialog_${CONVERSATION_ID}.txt"
CONTEXT_PCT_TRACKER_FILE="${STATE_DIR}/jetski_context_pct_${CONVERSATION_ID}.txt"
if ! [[ -e "${DIALOG_TRACKER_FILE}" ]]; then
  dispatch find "${STATE_DIR}/" -maxdepth 1 \
    \( -name 'jetski_dialog_*.txt' -o -name 'jetski_context_pct_*.txt' \) -mtime +7 -delete
fi
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
  start_time="$(run cat "${DIALOG_TRACKER_FILE}" 2>/dev/null)"
  start_time=${start_time:-0}
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
  elif [ "${elapsed}" -eq 0 ] || [ "${elapsed}" -eq 1 ]; then
    should_notify=1
    run echo -n $((start_time-2)) > "${DIALOG_TRACKER_FILE}"
  fi

  if [ "${should_notify}" -eq 1 ]; then
    log::info "Notifying user about dialog"
    # TODO: Do all of this in a dispatched script, instead of doing it here inline.
    local title="Jetski CLI: User approval required"
    local msg=""
    populate_tmux_info
    dispatch "${HOME}/scripts/agent_notify.sh" "${title}" ""
  fi
}

function notify_context_milestone {
  local milestone="$1"
  log::info "Notifying user that the context window reached ${milestone}%"
  populate_tmux_info
  dispatch "${HOME}/scripts/agent_notify.sh" "Jetski CLI: Context ${milestone}% used" ""
}

# Handle async (fire & forget) processing. A non-empty tracker (-s) means the
# pending dialog was already seen (portable, unlike GNU `stat --format`).
if [[ "${CONFIRMATION_DIALOG_PENDING}" == "true" ]]; then
  if ! [[ -s "${DIALOG_TRACKER_FILE}" ]]; then
    date '+%s' > "${DIALOG_TRACKER_FILE}"
  fi
  handle_dialog_notification
elif [[ -s "${DIALOG_TRACKER_FILE}" ]]; then
  log::debug "clearing ${DIALOG_TRACKER_FILE}"
  : > "${DIALOG_TRACKER_FILE}"
fi

printf -v USED_PCT_FMT "%.2f" "${USED_PCT}" 2>/dev/null || USED_PCT_FMT="0.00"
USED_PCT_INT=${USED_PCT_FMT%.*}

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
if [ -n "$VCS_WORKSPACE" ]; then
  VCS_FMT=" \033[36m☁ $VCS_WORKSPACE\033[0m"
elif [ -n "$VCS_BRANCH" ]; then
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

# Format Context usage bar, notifying once each time usage climbs past 60% and
# 85% (dropping back below a lower milestone re-arms it).
if ! [[ -f "${CONTEXT_PCT_TRACKER_FILE}" ]]; then
  echo "${USED_PCT_INT}" > "${CONTEXT_PCT_TRACKER_FILE}"
fi

PREV_USED_PCT_INT="$(run cat "${CONTEXT_PCT_TRACKER_FILE}" 2>/dev/null)"
[[ "${PREV_USED_PCT_INT}" =~ ^[0-9]+$ ]] || PREV_USED_PCT_INT=0
BAR_COLOR="\033[32m"
if [ "${USED_PCT_INT}" -ge 85 ]; then
  if [ "${PREV_USED_PCT_INT}" -lt 85 ]; then
    # Track that a higher milestone was reached.
    echo "${USED_PCT_INT}" > "${CONTEXT_PCT_TRACKER_FILE}"
    notify_context_milestone 85
  fi
  BAR_COLOR="\033[31m"
elif [ "${USED_PCT_INT}" -ge 60 ]; then
  if [ "${PREV_USED_PCT_INT}" -ge 85 ]; then
    # Track that a lower milestone was reached.
    echo "${USED_PCT_INT}" > "${CONTEXT_PCT_TRACKER_FILE}"
    # No notification
  elif [ "${PREV_USED_PCT_INT}" -lt 60 ]; then
    # Track that a higher milestone was reached.
    echo "${USED_PCT_INT}" > "${CONTEXT_PCT_TRACKER_FILE}"
    notify_context_milestone 60
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
