#!/usr/bin/env bash
# ==============================================================================
# Cross-Platform OS Compatibility Library (gLinux + macOS Darwin)
# ==============================================================================
# Provides unified shell primitives across Linux (gLinux X11/Wayland) and macOS
# (Darwin arm64/x86_64) for clipboard access, desktop notifications, process
# inspection, runtime directories, file metadata, advisory locking, FUSE/SSHFS
# mount management, and audio playback.
#
# Environment Variables:
#   OS_COMPAT_UNAME_S - Optional override for $(uname -s) used by unit tests.
#
# Usage Example:
#   source "${HOME}/lib/os_compat.sh"
#   if os::is_macos; then
#     printf "hello" | os::clipboard_copy
#   fi
# ==============================================================================

if [[ -n "${_OS_COMPAT_SH_LOADED:-}" ]]; then
  return 0 2>/dev/null || exit 0
fi
_OS_COMPAT_SH_LOADED=1

#######################################
# Returns the active kernel name ('Linux' or 'Darwin'), honoring
# OS_COMPAT_UNAME_S when set for deterministic unit testing.
#
# Arguments:
#   None
# Outputs:
#   Writes kernel name string to stdout.
# Returns:
#   0 always.
#
# Usage Example:
#   kernel="$(os::uname_s)"
#######################################
os::uname_s() {
  if [[ -n "${OS_COMPAT_UNAME_S:-}" ]]; then
    printf '%s\n' "${OS_COMPAT_UNAME_S}"
  else
    uname -s
  fi
}

#######################################
# Checks whether the current operating system is Linux.
#
# Arguments:
#   None
# Returns:
#   0 if running on Linux, 1 otherwise.
#
# Usage Example:
#   if os::is_linux; then echo "On Linux"; fi
#######################################
os::is_linux() {
  [[ "$(os::uname_s)" == "Linux" ]]
}

#######################################
# Checks whether the current operating system is macOS (Darwin).
#
# Arguments:
#   None
# Returns:
#   0 if running on Darwin, 1 otherwise.
#
# Usage Example:
#   if os::is_macos; then echo "On macOS"; fi
#######################################
os::is_macos() {
  [[ "$(os::uname_s)" == "Darwin" ]]
}

#######################################
# Alias for os::is_macos.
#
# Arguments:
#   None
# Returns:
#   0 if running on Darwin, 1 otherwise.
#
# Usage Example:
#   if os::is_darwin; then echo "On Darwin"; fi
#######################################
os::is_darwin() {
  os::is_macos
}

#######################################
# Resolves and ensures a user-private runtime directory (mode 0700) for state
# files, sockets, and locks. Avoids /run/user/<uid> on macOS where /run does not
# exist.
#
# Arguments:
#   None
# Outputs:
#   Writes the runtime directory path to stdout.
# Returns:
#   0 if directory exists or was created, non-zero on failure.
#
# Usage Example:
#   run_dir="$(os::runtime_dir)"
#######################################
os::runtime_dir() {
  local uid="${UID:-$(id -u)}"
  local dir
  if os::is_macos; then
    local base_tmp="${TMPDIR:-/tmp}"
    base_tmp="${base_tmp%/}"
    dir="${base_tmp}/runtime-${uid}"
  else
    dir="${XDG_RUNTIME_DIR:-/run/user/${uid}}"
    if [[ ! -d "${dir}" || ! -w "${dir}" ]]; then
      dir="/tmp/runtime-${uid}"
    fi
  fi
  if [[ ! -d "${dir}" ]]; then
    mkdir -p -m 0700 "${dir}" 2>/dev/null || mkdir -p "${dir}" 2>/dev/null
  fi
  printf '%s\n' "${dir}"
}

#######################################
# Copies standard input to the system clipboard (and X11 primary selection on
# Linux).
#
# Arguments:
#   None (reads payload from stdin).
# Returns:
#   0 if copied to at least one clipboard target, 1 otherwise.
#
# Usage Example:
#   printf "copied text" | os::clipboard_copy
#######################################
os::clipboard_copy() {
  local payload
  payload="$(cat)"

  if os::is_macos; then
    if command -v pbcopy &>/dev/null; then
      printf '%s' "${payload}" | pbcopy
      return $?
    fi
    return 1
  fi

  local copied=0
  if [[ -n "${WAYLAND_DISPLAY:-}" ]] && command -v wl-copy &>/dev/null; then
    if printf '%s' "${payload}" | wl-copy 2>/dev/null; then
      copied=1
    fi
  fi

  if command -v xclip &>/dev/null; then
    if [[ -n "${DISPLAY:-}" ]]; then
      if printf '%s' "${payload}" | xclip -i -selection clipboard 2>/dev/null; then
        printf '%s' "${payload}" | xclip -i -selection primary 2>/dev/null || true
        copied=1
      fi
    fi
    if [[ -d /tmp/.X11-unix ]]; then
      local sock display_num
      for sock in /tmp/.X11-unix/X*; do
        [[ -e "${sock}" ]] || continue
        display_num="${sock#/tmp/.X11-unix/X}"
        if [[ "${display_num}" =~ ^[0-9]+$ ]]; then
          if DISPLAY=":${display_num}" printf '%s' "${payload}" | DISPLAY=":${display_num}" xclip -i -selection clipboard 2>/dev/null; then
            DISPLAY=":${display_num}" printf '%s' "${payload}" | DISPLAY=":${display_num}" xclip -i -selection primary 2>/dev/null || true
            copied=1
          fi
        fi
      done
    fi
  fi

  [[ ${copied} -eq 1 ]]
}

#######################################
# Outputs the current system clipboard contents to stdout.
#
# Arguments:
#   None
# Outputs:
#   Clipboard text to stdout.
# Returns:
#   0 on success, non-zero otherwise.
#
# Usage Example:
#   text="$(os::clipboard_paste)"
#######################################
os::clipboard_paste() {
  if os::is_macos; then
    pbpaste
    return $?
  fi
  if [[ -n "${WAYLAND_DISPLAY:-}" ]] && command -v wl-paste &>/dev/null; then
    wl-paste --no-newline 2>/dev/null && return 0
  fi
  if command -v xclip &>/dev/null; then
    xclip -o -selection clipboard 2>/dev/null && return 0
  fi
  return 1
}

#######################################
# Dispatches a native desktop notification on macOS (terminal-notifier or
# osascript) or Linux (notify-send).
#
# Arguments:
#   [--urgency|-u LEVEL]     Optional urgency (low, normal, critical).
#   [--group|-g GROUP_ID]    Optional replacement group ID.
#   [--expire-time|-t MS]    Optional expiration timeout in milliseconds.
#   [--app-name|-a APP_NAME] Optional application name.
#   $1 - title: Notification title.
#   $2 - body: Optional notification message body.
# Returns:
#   0 on success, non-zero otherwise.
#
# Usage Example:
#   os::notify -u critical -g "build-alert" "Build Failed" "Target //foo:bar"
#######################################
os::notify() {
  local urgency="normal"
  local group_id=""
  local expire_ms=""
  local app_name=""

  while [[ $# -gt 0 ]]; do
    case "$1" in
      -u|--urgency)
        urgency="${2:-normal}"
        shift 2
        ;;
      --urgency=*)
        urgency="${1#*=}"
        shift
        ;;
      -g|--group)
        group_id="${2:-}"
        shift 2
        ;;
      --group=*)
        group_id="${1#*=}"
        shift
        ;;
      -t|--expire-time)
        expire_ms="${2:-}"
        shift 2
        ;;
      --expire-time=*)
        expire_ms="${1#*=}"
        shift
        ;;
      -a|--app-name)
        app_name="${2:-}"
        shift 2
        ;;
      --app-name=*)
        app_name="${1#*=}"
        shift
        ;;
      --)
        shift
        break
        ;;
      -*)
        shift
        ;;
      *)
        break
        ;;
    esac
  done

  local title="${1:-Notification}"
  local body="${2:-}"
  if [[ $# -ge 3 && "${urgency}" == "normal" ]]; then
    urgency="$3"
  fi
  if [[ $# -ge 4 && -z "${group_id}" ]]; then
    group_id="$4"
  fi

  if os::is_macos; then
    if command -v terminal-notifier &>/dev/null; then
      local tn_args=(-title "${title}" -message "${body:- }")
      [[ -n "${app_name}" ]] && tn_args+=(-subtitle "${app_name}")
      [[ -n "${group_id}" ]] && tn_args+=(-group "${group_id}")
      terminal-notifier "${tn_args[@]}" &>/dev/null
      return $?
    elif command -v osascript &>/dev/null; then
      osascript -e 'on run argv' \
        -e 'display notification (item 2 of argv) with title (item 1 of argv)' \
        -e 'end run' -- "${title}" "${body}" &>/dev/null
      return $?
    fi
    return 1
  fi

  if [[ -n "${DISPLAY:-}" || -n "${WAYLAND_DISPLAY:-}" ]] && command -v notify-send &>/dev/null; then
    local ns_args=(-u "${urgency}")
    [[ -n "${app_name}" ]] && ns_args+=(-a "${app_name}")
    [[ -n "${expire_ms}" ]] && ns_args+=(-t "${expire_ms}")
    if [[ -n "${group_id}" ]]; then
      ns_args+=(-h "string:x-canonical-private-synchronous:${group_id}")
    fi
    notify-send "${ns_args[@]}" -- "${title}" "${body}"
    return $?
  fi
  return 1
}

#######################################
# Returns the modification time of a file in epoch seconds.
#
# Arguments:
#   $1 - path: File or directory path.
# Outputs:
#   Epoch timestamp integer to stdout (or 0 if file does not exist).
# Returns:
#   0 if stat succeeded, 1 otherwise.
#
# Usage Example:
#   mtime="$(os::file_mtime "/tmp/my_file")"
#######################################
os::file_mtime() {
  local target_path="$1"
  [[ -e "${target_path}" ]] || { printf '0\n'; return 1; }
  if os::is_macos; then
    stat -f %m "${target_path}" 2>/dev/null || { printf '0\n'; return 1; }
  else
    stat -c %Y "${target_path}" 2>/dev/null || { printf '0\n'; return 1; }
  fi
}

#######################################
# Base64-encodes standard input without line wraps (portable replacement for GNU
# `base64 -w0`).
#
# Arguments:
#   None (reads from stdin).
# Outputs:
#   Single-line base64 string to stdout.
# Returns:
#   0 on success.
#
# Usage Example:
#   encoded="$(printf '%s' "hello" | os::base64_nowrap)"
#######################################
os::base64_nowrap() {
  base64 | tr -d '\n\r'
}

#######################################
# Executes a command with a timeout duration (portable across GNU `timeout`,
# Homebrew `gtimeout`, and pure-bash fallback).
#
# Arguments:
#   $1 - duration: Timeout in seconds (optionally suffixed with 's', e.g. '5s').
#   $@ - command and arguments to execute.
# Returns:
#   Exit code of the command, or 124 on timeout.
#
# Usage Example:
#   os::timeout 3s ssh gcloud true
#######################################
os::timeout() {
  local duration="$1"
  shift
  if command -v timeout &>/dev/null; then
    timeout "${duration}" "$@"
    return $?
  elif command -v gtimeout &>/dev/null; then
    gtimeout "${duration}" "$@"
    return $?
  fi

  local secs="${duration%s}"
  "$@" &
  local cmd_pid=$!
  (
    sleep "${secs}"
    if kill -0 "${cmd_pid}" 2>/dev/null; then
      kill -TERM "${cmd_pid}" 2>/dev/null || true
    fi
  ) &
  local watcher_pid=$!
  local rc=0
  wait "${cmd_pid}" 2>/dev/null || rc=$?
  kill -TERM "${watcher_pid}" 2>/dev/null || true
  wait "${watcher_pid}" 2>/dev/null || true
  return ${rc}
}

#######################################
# Acquires a non-blocking advisory lock on a file descriptor or lockfile path.
#
# Arguments:
#   $1 - fd_or_path: Numeric file descriptor (when `flock` is installed) or
#        lockfile path.
# Returns:
#   0 if lock was acquired, 1 otherwise.
#
# Usage Example:
#   exec 9>"${lock_file}"
#   os::flock_nb 9 || exit 1
#######################################
os::flock_nb() {
  local target="$1"
  if command -v flock &>/dev/null; then
    flock -n "${target}"
    return $?
  fi
  local lock_dir="/tmp/os_compat_lock_${UID:-$(id -u)}_${target//[^a-zA-Z0-9_.-]/_}.d"
  if mkdir "${lock_dir}" 2>/dev/null; then
    printf '%s\n' "$$" > "${lock_dir}/pid"
    return 0
  fi
  if [[ -f "${lock_dir}/pid" ]]; then
    local owner_pid
    owner_pid="$(head -n 1 "${lock_dir}/pid" 2>/dev/null | tr -d '[:space:]')"
    if [[ -n "${owner_pid}" ]] && ! kill -0 "${owner_pid}" 2>/dev/null; then
      rm -rf "${lock_dir}" 2>/dev/null || true
      if mkdir "${lock_dir}" 2>/dev/null; then
        printf '%s\n' "$$" > "${lock_dir}/pid"
        return 0
      fi
    fi
  fi
  return 1
}

#######################################
# Sends a single ICMP ping with a 1-second timeout and outputs the round-trip
# latency in milliseconds.
#
# Arguments:
#   $1 - host: Target hostname or IP (default: 8.8.8.8).
# Outputs:
#   Round-trip time in ms (or empty string on timeout/error).
# Returns:
#   0 if ping succeeded, 1 otherwise.
#
# Usage Example:
#   rtt="$(os::ping_once_ms 8.8.8.8)"
#######################################
os::ping_once_ms() {
  local host="${1:-8.8.8.8}"
  local out
  if os::is_macos; then
    # On macOS BSD ping, -W wait time is in milliseconds (1000ms = 1s)
    out="$(ping -c 1 -W 1000 "${host}" 2>/dev/null || true)"
  else
    # On Linux iputils ping, -W timeout is in seconds (1s)
    out="$(ping -c 1 -W 1 "${host}" 2>/dev/null || true)"
  fi
  local rtt
  rtt="$(printf '%s\n' "${out}" | sed -n 's/.*time=\([^ ]*\).*/\1/p' | head -n 1)"
  if [[ -n "${rtt}" ]]; then
    printf '%s\n' "${rtt}"
    return 0
  fi
  return 1
}

#######################################
# Checks whether a given PID is stuck in uninterruptible sleep ('D' state on
# Linux /proc/<pid>/status, 'U' state on Darwin `ps`).
#
# Arguments:
#   $1 - pid: Process ID to inspect.
# Returns:
#   0 if the process is in uninterruptible sleep, 1 otherwise.
#
# Usage Example:
#   if os::pid_is_uninterruptible "${pid}"; then echo "stuck"; fi
#######################################
os::pid_is_uninterruptible() {
  local pid="$1"
  [[ -n "${pid}" && "${pid}" =~ ^[0-9]+$ ]] || return 1
  if os::is_macos; then
    local state
    state="$(ps -o state= -p "${pid}" 2>/dev/null | tr -d '[:space:]')"
    [[ "${state}" == U* ]]
    return $?
  fi
  if [[ -r "/proc/${pid}/status" ]]; then
    grep -qE '^State:[[:space:]]+D' "/proc/${pid}/status" 2>/dev/null
    return $?
  fi
  return 1
}

#######################################
# Retrieves the full command line of a running process.
#
# Arguments:
#   $1 - pid: Process ID to inspect.
# Outputs:
#   Space-separated command line string to stdout.
# Returns:
#   0 if process exists and command line was retrieved, 1 otherwise.
#
# Usage Example:
#   cmdline="$(os::pid_cmdline "$$")"
#######################################
os::pid_cmdline() {
  local pid="$1"
  [[ -n "${pid}" && "${pid}" =~ ^[0-9]+$ ]] || return 1
  kill -0 "${pid}" 2>/dev/null || return 1

  if os::is_linux && [[ -r "/proc/${pid}/cmdline" ]]; then
    local cmd
    cmd="$({ tr '\0' ' ' < "/proc/${pid}/cmdline"; } 2>/dev/null || true)"
    if [[ -n "${cmd}" ]]; then
      printf '%s\n' "${cmd}"
      return 0
    fi
  fi

  ps -o command= -p "${pid}" 2>/dev/null
}

#######################################
# Lists resolved absolute file paths currently held open by a process.
# Strips `lsof -Fn` field prefixes ('p<PID>' and 'n') on Darwin so every emitted
# line is a clean absolute filesystem path starting with '/'.
#
# Arguments:
#   $1 - pid: Process ID to inspect.
# Outputs:
#   Newline-separated absolute file paths (each starting with '/') to stdout.
# Returns:
#   0 always.
#
# Usage Example:
#   while IFS= read -r path; do
#     echo "Open file: ${path}"
#   done < <(os::pid_open_files "$$")
#######################################
os::pid_open_files() {
  local pid="$1"
  [[ -n "${pid}" && "${pid}" =~ ^[0-9]+$ ]] || return 0

  if os::is_linux && [[ -d "/proc/${pid}/fd" ]]; then
    local fd_link target_path
    for fd_link in "/proc/${pid}/fd"/*; do
      [[ -e "${fd_link}" || -L "${fd_link}" ]] || continue
      target_path="$(readlink "${fd_link}" 2>/dev/null || true)"
      if [[ "${target_path}" == /* ]]; then
        printf '%s\n' "${target_path}"
      fi
    done
    return 0
  fi

  if command -v lsof &>/dev/null; then
    lsof -n -P -p "${pid}" -Fn 2>/dev/null | sed -n 's/^n\(\/.*\)/\1/p'
  fi
  return 0
}

#######################################
# Checks whether a directory path is currently an active filesystem mountpoint.
#
# Arguments:
#   $1 - mount_dir: Directory path to check.
# Returns:
#   0 if mounted, 1 otherwise.
#
# Usage Example:
#   if os::is_mounted "${HOME}/notes"; then echo "Mounted"; fi
#######################################
os::is_mounted() {
  local mount_dir="${1%/}"
  [[ -n "${mount_dir}" ]] || return 1

  if os::is_macos; then
    mount 2>/dev/null | grep -Fq " on ${mount_dir} ("
    return $?
  fi

  if command -v findmnt &>/dev/null; then
    findmnt -M "${mount_dir}" &>/dev/null && return 0
  fi
  if command -v mountpoint &>/dev/null; then
    mountpoint -q "${mount_dir}" 2>/dev/null && return 0
  fi
  return 1
}

#######################################
# Forcefully/lazily unmounts a FUSE/SSHFS mountpoint across Linux and macOS.
#
# Arguments:
#   $1 - mount_dir: Directory path to unmount.
# Returns:
#   0 if unmounted (or not mounted), non-zero on failure.
#
# Usage Example:
#   os::unmount_lazy "${HOME}/notes"
#######################################
os::unmount_lazy() {
  local mount_dir="${1%/}"
  [[ -n "${mount_dir}" ]] || return 1

  if os::is_macos; then
    diskutil unmount force "${mount_dir}" &>/dev/null || umount -f "${mount_dir}" &>/dev/null || true
    return 0
  fi

  if command -v fusermount3 &>/dev/null; then
    fusermount3 -u -z "${mount_dir}" 2>/dev/null && return 0
  fi
  if command -v fusermount &>/dev/null; then
    fusermount -u -z "${mount_dir}" 2>/dev/null && return 0
  fi
  umount -l "${mount_dir}" 2>/dev/null || umount "${mount_dir}" 2>/dev/null || true
  return 0
}

#######################################
# Plays an audio file asynchronously or synchronously using the native OS audio
# player (`afplay` on macOS; `pw-play`, `ffplay`, `paplay`, or `aplay` on Linux).
#
# Arguments:
#   $1 - audio_file: Path to the sound file to play.
# Returns:
#   0 if playback command succeeded, 1 otherwise.
#
# Usage Example:
#   os::play_audio "/System/Library/Sounds/Sosumi.aiff"
#######################################
os::play_audio() {
  local audio_file="$1"
  if os::is_macos; then
    if [[ ! -f "${audio_file}" || "${audio_file}" == *.oga ]]; then
      audio_file="/System/Library/Sounds/Sosumi.aiff"
    fi
    if command -v afplay &>/dev/null && [[ -f "${audio_file}" ]]; then
      afplay "${audio_file}" &>/dev/null
      return $?
    fi
    return 1
  fi

  [[ -f "${audio_file}" ]] || return 1
  if command -v pw-play &>/dev/null; then
    pw-play "${audio_file}" &>/dev/null && return 0
  fi
  if command -v ffplay &>/dev/null; then
    ffplay -nodisp -autoexit -loglevel quiet "${audio_file}" &>/dev/null && return 0
  fi
  if command -v paplay &>/dev/null; then
    paplay "${audio_file}" &>/dev/null && return 0
  fi
  if command -v aplay &>/dev/null; then
    aplay -q "${audio_file}" &>/dev/null && return 0
  fi
  return 1
}

#######################################
# Alias for os::play_audio.
#
# Arguments:
#   $1 - audio_file: Path to the sound file to play.
# Returns:
#   0 if playback command succeeded, 1 otherwise.
#
# Usage Example:
#   os::play_sound "${NOTIFICATION_SOUND}"
#######################################
os::play_sound() {
  os::play_audio "$@"
}
