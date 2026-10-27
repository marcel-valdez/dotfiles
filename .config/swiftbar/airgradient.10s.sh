#!/usr/bin/env bash
# ==============================================================================
# airgradient.10s.sh: SwiftBar Menu Bar Plugin for AirGradient Monitor
# ==============================================================================
# Disabled by default on gMac because custom Go binaries require Santa peer
# upvotes. Set AIRGRADIENT_ENABLED=1 or touch
# ~/.config/airgradient-monitor/enabled to enable.
# ==============================================================================

if [[ "${AIRGRADIENT_ENABLED:-0}" != "1" && ! -f "${HOME}/.config/airgradient-monitor/enabled" ]]; then
  exit 0
fi

export PATH="/opt/homebrew/bin:/usr/local/bin:/usr/local/go/bin:${HOME}/.local/bin:${HOME}/bin:${PATH}"

AG_REPO_DIR="${HOME}/projects/air_gradient_alarm_daemon"
AG_CONFIG="${HOME}/.config/airgradient-monitor/config.json"

find_ag_bin() {
  if [[ -x "${AG_REPO_DIR}/airgradient-monitor" ]]; then
    printf '%s\n' "${AG_REPO_DIR}/airgradient-monitor"
  elif [[ -x "${AG_REPO_DIR}/bin/airgradient-monitor" ]]; then
    printf '%s\n' "${AG_REPO_DIR}/bin/airgradient-monitor"
  elif [[ -x "${HOME}/.local/bin/airgradient-monitor" ]]; then
    printf '%s\n' "${HOME}/.local/bin/airgradient-monitor"
  elif command -v airgradient-monitor &>/dev/null; then
    command -v airgradient-monitor
  fi
}

AG_BIN="$(find_ag_bin)"

if [[ -z "${AG_BIN}" ]]; then
  echo "AG: N/A"
  echo "---"
  echo "airgradient-monitor binary not found"
  exit 0
fi

AG_ARGS=(-genmon)
if [[ -f "${AG_CONFIG}" ]]; then
  AG_ARGS+=(-config "${AG_CONFIG}")
fi

RAW_OUT="$("${AG_BIN}" "${AG_ARGS[@]}" 2>/dev/null || true)"
if [[ -z "${RAW_OUT}" ]]; then
  echo "AG: offline"
  echo "---"
  echo "Daemon or sensor unreachable"
  exit 0
fi

# Extract <txt>...</txt> for the menu bar title and strip Pango markup tags
TXT_LINE="$(printf '%s\n' "${RAW_OUT}" | tr '\n' ' ' | sed -nE 's/.*<txt>(.*)<\/txt>.*/\1/p' | sed -E 's/<[^>]+>//g' | sed -E 's/^[[:space:]]+|[[:space:]]+$//g')"
if [[ -z "${TXT_LINE}" ]]; then
  TXT_LINE="AG: active"
fi

echo "${TXT_LINE} | font='JetBrains Mono' size=12"
echo "---"

# Extract <tool>...</tool> for dropdown lines
TOOL_TEXT="$(printf '%s\n' "${RAW_OUT}" | sed -n '/<tool>/,/<\/tool>/p' | sed -E 's/<\/?tool>//g; s/<[^>]+>//g')"
if [[ -n "${TOOL_TEXT}" ]]; then
  while IFS= read -r line; do
    [[ -n "${line}" ]] && echo "${line}"
  done <<< "${TOOL_TEXT}"
fi
