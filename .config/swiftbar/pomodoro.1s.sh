#!/usr/bin/env bash
# ==============================================================================
# pomodoro.1s.sh: SwiftBar Menu Bar Plugin for ~/.pomodororc & ~/modules/pomodoro
# ==============================================================================
# Renders Pomodoro timer status in the macOS menu bar every second and provides
# dropdown actions to Start/Resume, Pause, and Reset the Pomodoro state in
# ~/.pomodoro.d/ while invoking ~/.pomodororc event hooks.
# ==============================================================================

export PATH="/opt/homebrew/bin:/usr/local/bin:${HOME}/bin:${PATH}"

STORAGE="${HOME}/.pomodoro.d"
mkdir -p "${STORAGE}"

# Handle SwiftBar click actions passed via CLI argument
case "${1:-}" in
  start)
    date +%s > "${STORAGE}/start_time"
    echo "pomodoro" > "${STORAGE}/state"
    rm -f "${STORAGE}/paused_remaining"
    "${HOME}/.pomodororc" --command pomodoro_start &>/dev/null || true
    exit 0
    ;;
  pause)
    if [[ -f "${STORAGE}/start_time" ]]; then
      now=$(date +%s)
      start=$(cat "${STORAGE}/start_time" 2>/dev/null || echo "${now}")
      elapsed=$(( now - start ))
      rem=$(( 45 * 60 - elapsed ))
      (( rem < 0 )) && rem=0
      echo "${rem}" > "${STORAGE}/paused_remaining"
      echo "paused" > "${STORAGE}/state"
      "${HOME}/.pomodororc" --command pause &>/dev/null || true
    fi
    exit 0
    ;;
  resume)
    rem=$(cat "${STORAGE}/paused_remaining" 2>/dev/null || echo $((45 * 60)))
    now=$(date +%s)
    new_start=$(( now - (45 * 60 - rem) ))
    echo "${new_start}" > "${STORAGE}/start_time"
    echo "pomodoro" > "${STORAGE}/state"
    rm -f "${STORAGE}/paused_remaining"
    "${HOME}/.pomodororc" --command resume pomodoro &>/dev/null || true
    exit 0
    ;;
  stop)
    rm -f "${STORAGE}/start_time" "${STORAGE}/paused_remaining"
    echo "stopped" > "${STORAGE}/state"
    "${HOME}/.pomodororc" --command pause &>/dev/null || true
    exit 0
    ;;
esac

# If ~/modules/pomodoro/pomodoro.sh exists and produces genmon XML, parse it
if [[ -x "${HOME}/modules/pomodoro/pomodoro.sh" ]]; then
  RAW_XML="$("${HOME}/.pomodororc" 2>/dev/null || true)"
  TXT="$(printf '%s\n' "${RAW_XML}" | tr '\n' ' ' | sed -nE 's/.*<txt>(.*)<\/txt>.*/\1/p' | sed -E 's/<[^>]+>//g; s/^[[:space:]]+|[[:space:]]+$//g')"
  if [[ -n "${TXT}" ]]; then
    echo "🍅 ${TXT} | font='JetBrains Mono' size=12"
    echo "---"
    echo "Start New Pomodoro (45m) | bash='$0' param1=start terminal=false refresh=true"
    echo "Pause Pomodoro | bash='$0' param1=pause terminal=false refresh=true"
    echo "Resume Pomodoro | bash='$0' param1=resume terminal=false refresh=true"
    echo "Stop / Reset | bash='$0' param1=stop terminal=false refresh=true"
    exit 0
  fi
fi

# Standalone state rendering using ~/.pomodoro.d/
STATE="$(cat "${STORAGE}/state" 2>/dev/null || echo "stopped")"
if [[ "${STATE}" == "pomodoro" && -f "${STORAGE}/start_time" ]]; then
  now=$(date +%s)
  start=$(cat "${STORAGE}/start_time" 2>/dev/null || echo "${now}")
  rem=$(( 45 * 60 - (now - start) ))
  if (( rem <= 0 )); then
    echo "stopped" > "${STORAGE}/state"
    rm -f "${STORAGE}/start_time"
    "${HOME}/.pomodororc" --command pomodoro_end &>/dev/null || true
    echo "🍅 Done! | color=green font='JetBrains Mono' size=12"
  else
    mins=$(( rem / 60 ))
    secs=$(( rem % 60 ))
    printf "🍅 %02d:%02d | font='JetBrains Mono' size=12\n" "${mins}" "${secs}"
  fi
elif [[ "${STATE}" == "paused" ]]; then
  rem=$(cat "${STORAGE}/paused_remaining" 2>/dev/null || echo 0)
  mins=$(( rem / 60 ))
  secs=$(( rem % 60 ))
  printf "⏸ %02d:%02d | color=yellow font='JetBrains Mono' size=12\n" "${mins}" "${secs}"
else
  echo "🍅 Idle | font='JetBrains Mono' size=12"
fi

echo "---"
echo "Start Pomodoro (45m) | bash='$0' param1=start terminal=false refresh=true"
echo "Pause Pomodoro | bash='$0' param1=pause terminal=false refresh=true"
echo "Resume Pomodoro | bash='$0' param1=resume terminal=false refresh=true"
echo "Stop / Reset | bash='$0' param1=stop terminal=false refresh=true"
