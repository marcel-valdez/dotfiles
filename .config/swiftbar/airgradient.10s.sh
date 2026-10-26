#!/usr/bin/env bash
# ==============================================================================
# airgradient.10s.sh: SwiftBar Menu Bar Plugin for AirGradient Monitor
# ==============================================================================
# Polls `airgradient-monitor -genmon` every 10 seconds, converts XFCE4 genmon
# <txt>/<tool> XML tags and Pango <span> markup into SwiftBar menu bar format.
# ==============================================================================

export PATH="/opt/homebrew/bin:/usr/local/bin:${HOME}/bin:${PATH}"

AG_BIN=""
if [[ -x "${HOME}/projects/air_gradient_alarm_daemon/airgradient-monitor" ]]; then
  AG_BIN="${HOME}/projects/air_gradient_alarm_daemon/airgradient-monitor"
elif command -v airgradient-monitor &>/dev/null; then
  AG_BIN="$(command -v airgradient-monitor)"
fi

if [[ -z "${AG_BIN}" ]]; then
  echo "AG: N/A"
  echo "---"
  echo "airgradient-monitor binary not found"
  exit 0
fi

RAW_OUT="$("${AG_BIN}" -genmon 2>/dev/null || true)"
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
