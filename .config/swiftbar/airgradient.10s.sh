#!/usr/bin/env bash
# ==============================================================================
# airgradient.10s.sh: SwiftBar Menu Bar Plugin for AirGradient Monitor
# ==============================================================================
# Polls `airgradient-monitor -genmon` every 10 seconds, converts XFCE4 genmon
# <txt>/<tool> XML tags and Pango <span> markup into SwiftBar menu bar format.
# ==============================================================================

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

# Self-heal on fresh machines: if airgradient-monitor has not been built yet
# and Go is installed, clone marcel-valdez/air_gradient_alarm_daemon and build.
if [[ -z "${AG_BIN}" ]] && command -v go &>/dev/null; then
  mkdir -p "${HOME}/projects" "${HOME}/.local/bin"
  if [[ ! -d "${AG_REPO_DIR}/.git" ]] && command -v git &>/dev/null; then
    git clone --depth 1 https://github.com/marcel-valdez/air_gradient_alarm_daemon.git "${AG_REPO_DIR}" &>/dev/null || true
  fi
  if [[ -d "${AG_REPO_DIR}/cmd/airgradient-monitor" ]]; then
    (
      cd "${AG_REPO_DIR}" && \
      CGO_ENABLED=0 go build -trimpath -o "${AG_REPO_DIR}/airgradient-monitor" ./cmd/airgradient-monitor &>/dev/null && \
      cp -f "${AG_REPO_DIR}/airgradient-monitor" "${HOME}/.local/bin/airgradient-monitor" &>/dev/null
    ) || true
  fi
  if [[ ! -f "${AG_CONFIG}" && -f "${HOME}/.config/airgradient-monitor/config.macos.json" ]]; then
    mkdir -p -m 700 "${HOME}/.config/airgradient-monitor"
    cp "${HOME}/.config/airgradient-monitor/config.macos.json" "${AG_CONFIG}"
    chmod 600 "${AG_CONFIG}"
  fi
  AG_BIN="$(find_ag_bin)"
fi

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
