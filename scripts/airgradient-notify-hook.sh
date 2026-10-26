#!/usr/bin/env bash
# ==============================================================================
# airgradient-notify-hook.sh: Dual-OS AirGradient Notification Hook
# ==============================================================================
# Invoked by airgradient-monitor when air quality metrics cross thresholds.
# Uses os::notify with -group "airgradient-${AG_METRIC}" so repeated alerts
# replace in place on macOS (terminal-notifier) and Linux (notify-send).
#
# Usage Example:
#   AG_METRIC="rco2" AG_STATUS="CRITICAL" AG_SUMMARY="CO2 High" \
#     AG_BODY="CO2 is 1450 ppm" ~/scripts/airgradient-notify-hook.sh
# ==============================================================================

set -euo pipefail

if [[ -f "${HOME}/lib/os_compat.sh" ]]; then
  source "${HOME}/lib/os_compat.sh"
fi

METRIC="${AG_METRIC:-all}"
STATUS="${AG_STATUS:-WARNING}"
TITLE="${AG_SUMMARY:-${1:-AirGradient Alert (${STATUS})}}"
BODY="${AG_BODY:-${2:-Metric ${METRIC}: ${AG_VALUE:-unknown} (threshold ${AG_THRESHOLD:-unknown})}}"

URGENCY="normal"
if [[ "${STATUS}" =~ CRITICAL|ALARM|DANGER|HIGH ]]; then
  URGENCY="critical"
elif [[ "${STATUS}" =~ OK|NORMAL|RECOVERY ]]; then
  URGENCY="low"
fi

if declare -F os::notify >/dev/null 2>&1; then
  os::notify -u "${URGENCY}" -a "AirGradient" -g "airgradient-${METRIC}" -- "${TITLE}" "${BODY}"
fi
