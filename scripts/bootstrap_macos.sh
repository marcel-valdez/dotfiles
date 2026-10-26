#!/usr/bin/env bash
# ==============================================================================
# bootstrap_macos.sh: Idempotent macOS M5 Pro Bootstrap & Verification Script
# ==============================================================================
# Sets up a new Apple Silicon MacBook Pro (Darwin arm64) with Marcel's complete
# Linux power-user environment, Homebrew packages, fonts, FUSE-T user-space
# sshfs, compiled C++/Go helper binaries, macOS defaults, and LaunchAgents.
#
# Usage:
#   ~/scripts/bootstrap_macos.sh [--dry-run] [--help|-h]
#   ~/.googlerc.d/scripts/bootstrap_macos.sh [--dry-run] [--help|-h]
# ==============================================================================

set -euo pipefail

if [[ -d "/opt/homebrew/bin" && ":${PATH}:" != *":/opt/homebrew/bin:"* ]]; then
  export PATH="/opt/homebrew/bin:/opt/homebrew/sbin:${PATH}"
fi

DRY_RUN=0

print_usage() {
  cat << 'EOF'
Usage: bootstrap_macos.sh [--dry-run] [--help|-h]

Options:
  --dry-run   Print all planned installation, compilation, symlink, and
              configuration commands without mutating system state.
  -h, --help  Show this help message and exit.
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --dry-run)
      DRY_RUN=1
      shift
      ;;
    -h|--help)
      print_usage
      exit 0
      ;;
    *)
      echo "[ERROR] Unknown argument: $1" >&2
      print_usage >&2
      exit 2
      ;;
  esac
done

run_cmd() {
  if (( DRY_RUN == 1 )); then
    printf '[DRY-RUN] %s\n' "$*"
  else
    printf '[EXEC]    %s\n' "$*"
    "$@"
  fi
}

echo "=============================================================================="
echo "macOS M5 Pro Dual-OS Dotfiles & Daemon Bootstrap (DRY_RUN=${DRY_RUN})"
echo "=============================================================================="

if [[ "$(uname -s)" != "Darwin" ]] && (( DRY_RUN == 0 )); then
  echo "[ERROR] bootstrap_macos.sh must be run on macOS (Darwin), or pass --dry-run." >&2
  exit 1
fi

# ------------------------------------------------------------------------------
# Stage 1: Homebrew Packages, Casks, Fonts, & FUSE-T User-Space SSHFS
# ------------------------------------------------------------------------------
echo ""
echo "--- Stage 1: Homebrew CLI Tools, GUI Casks, Fonts, & FUSE-T ---"
BREW_FORMULAE=(
  bash
  coreutils
  findutils
  gnu-sed
  grep
  gawk
  flock
  cmake
  fzf
  ripgrep
  fd
  bat
  jq
  tmux
  neovim
  emacs
  terminal-notifier
  displayplacer
  sox
)

BREW_CASKS=(
  kitty
  hammerspoon
  karabiner-elements
  linearmouse
  swiftbar
  autoraise
  maccy
  font-azeret-mono
  font-jetbrains-mono
  font-inter
)

run_cmd brew tap macos-fuse-t/homebrew-cask
run_cmd brew install "${BREW_FORMULAE[@]}"
run_cmd brew install --cask "${BREW_CASKS[@]}"
run_cmd brew install --cask macos-fuse-t/homebrew-cask/fuse-t macos-fuse-t/homebrew-cask/fuse-t-sshfs

# ------------------------------------------------------------------------------
# Stage 2: External Git Modules (~/modules/pomodoro, ~/.googlerc.d) & Helpers
# ------------------------------------------------------------------------------
echo ""
echo "--- Stage 2: External Modules & Native C++ Helper Compilation ---"
run_cmd mkdir -p "${HOME}/modules" "${HOME}/.cache" "${HOME}/tmp"
if [[ ! -d "${HOME}/modules/pomodoro" ]]; then
  run_cmd git clone https://github.com/parantapa/pomodoro.git "${HOME}/modules/pomodoro"
fi

if [[ ! -d "${HOME}/.googlerc.d/.git" ]]; then
  if command -v git-remote-sso >/dev/null 2>&1; then
    run_cmd git clone sso://user/marcelvaldez/googlerc "${HOME}/.googlerc.d" || \
      echo "[WARN] Could not clone sso://user/marcelvaldez/googlerc (run gcert first)."
  else
    echo "[INFO] ~/.googlerc.d not cloned yet (git-remote-sso not in PATH)."
  fi
fi

TMUX_MEM_CPU_DIR="${HOME}/.tmux/plugins/tmux-mem-cpu-load"
if [[ -d "${TMUX_MEM_CPU_DIR}" ]] || (( DRY_RUN == 1 )); then
  run_cmd bash -c "cd '${TMUX_MEM_CPU_DIR}' && cmake . && make -j4"
fi

# ------------------------------------------------------------------------------
# Stage 3: Go Daemons (whatsapp-notification & airgradient-monitor) + Santa Hint
# ------------------------------------------------------------------------------
echo ""
echo "--- Stage 3: Go Daemon Compilation (darwin/arm64) & Santa Binary Authorization ---"
if [[ -d "${HOME}/projects/whatsapp-notification" ]] || (( DRY_RUN == 1 )); then
  # whatsapp-notification uses github.com/mattn/go-sqlite3 (cgo) and requires CGO_ENABLED=1 + Xcode CLT
  run_cmd bash -c "cd '${HOME}/projects/whatsapp-notification' && CGO_ENABLED=1 GOOS=darwin GOARCH=arm64 go build -o bin/whatsapp-notification ."
fi
if [[ -d "${HOME}/projects/air_gradient_alarm_daemon" ]] || (( DRY_RUN == 1 )); then
  run_cmd bash -c "cd '${HOME}/projects/air_gradient_alarm_daemon' && CGO_ENABLED=0 GOOS=darwin GOARCH=arm64 go build -o airgradient-monitor ."
  run_cmd mkdir -p "${HOME}/.config/airgradient-monitor"
  if [[ -f "${HOME}/.config/airgradient-monitor/config.macos.json" && ! -f "${HOME}/.config/airgradient-monitor/config.json" ]]; then
    run_cmd cp "${HOME}/.config/airgradient-monitor/config.macos.json" "${HOME}/.config/airgradient-monitor/config.json"
  fi
fi

cat << 'EOF'
[INFO] Note on Google macOS Santa Binary Authorization:
       If Google Santa blocks execution of a locally compiled Go/C++ binary:
         1. Run: santactl fileinfo <path-to-binary>
         2. Open the printed Rule/Upvote URL to self-approve your binary hash.
EOF

# ------------------------------------------------------------------------------
# Stage 4: macOS System Defaults for Linux Power Users
# ------------------------------------------------------------------------------
echo ""
echo "--- Stage 4: macOS System Defaults (Key Repeat, Spaces, Window Drag, Maccy) ---"
# Ultra-fast key repeat and disable press-and-hold accent menu for Emacs/Vim
run_cmd defaults write NSGlobalDomain KeyRepeat -int 1
run_cmd defaults write NSGlobalDomain InitialKeyRepeat -int 10
run_cmd defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false

# Native Ctrl+Cmd click-drag window move backup alongside Hammerspoon Alt+Drag
run_cmd defaults write -g NSWindowShouldDragOnGesture -bool true

# Disable Mission Control auto-reordering of Spaces 1-6 based on MRU
run_cmd defaults write com.apple.dock mru-spaces -bool false

# Synchronize Spaces across external monitors & allow dual-monitor window spanning (both-screens)
run_cmd defaults write com.apple.spaces spans-displays -bool true

# Configure SwiftBar plugin directory
run_cmd defaults write com.ameba.SwiftBar PluginDirectory -string "${HOME}/.config/swiftbar"

# Restart Dock/SystemUIServer to apply Space & key repeat settings
run_cmd killall Dock || true

# ------------------------------------------------------------------------------
# Stage 5: Symlink & Load LaunchAgents (knock, clipboard, whatsapp, airgradient)
# ------------------------------------------------------------------------------
echo ""
echo "--- Stage 5: LaunchAgent Registration (~/Library/LaunchAgents) ---"
run_cmd mkdir -p "${HOME}/Library/LaunchAgents"

PLISTS=(
  "${HOME}/.googlerc.d/launchd/com.marcelvaldez.knock-daemon.plist"
  "${HOME}/.config/launchd/com.marcelvaldez.clipboard-daemon.plist"
  "${HOME}/.config/launchd/com.marcelvaldez.whatsapp-notification.plist"
  "${HOME}/.config/launchd/com.marcelvaldez.airgradient-monitor.plist"
)

for plist_src in "${PLISTS[@]}"; do
  if [[ ! -f "${plist_src}" ]] && (( DRY_RUN == 0 )); then
    echo "[WARN] Skipping missing LaunchAgent plist: ${plist_src}"
    continue
  fi
  plist_name="$(basename "${plist_src}")"
  plist_dst="${HOME}/Library/LaunchAgents/${plist_name}"
  run_cmd ln -sfn "${plist_src}" "${plist_dst}"
  run_cmd launchctl bootout "gui/$(id -u)" "${plist_dst}" || true
  run_cmd launchctl bootstrap "gui/$(id -u)" "${plist_dst}" || true
done

# ------------------------------------------------------------------------------
# Stage 6: MacBook Pro 14" (M5 Pro) + ThinkPad TB4 Dock (40B0) Cabling Checklist
# ------------------------------------------------------------------------------
echo ""
cat << 'EOF'
==============================================================================
WFH Hardware Checklist: MacBook Pro 14" (M5 Pro) + ThinkPad TB4 Dock (40B0)
==============================================================================
Target Mac: MacBook Pro 14-inch (15-core CPU / 16-core GPU M5 Pro, 48 GB RAM,
1 TB SSD, 3024x1964 Liquid Retina XDR, 3x Thunderbolt 5, HDMI, SDXC, MagSafe 3).
Displays: 2x HGC CR240DM 24" 1920x1080 @ 120/144 Hz (currently on dock MST
connectors DP-1-5 & DP-1-6).

Because macOS does NOT support DisplayPort Multi-Stream Transport (DP-MST)
extended mode over a single DisplayPort stream, plugging both HGC 24" monitors
into the dock's MST-split video outputs will cause macOS to MIRROR them.

Working Cabling Layouts (pick one):
  Option A (Recommended):
    - Keep Monitor A plugged into the ThinkPad TB4 Dock's DisplayPort output.
    - Plug Monitor B directly into the MacBook Pro 14-inch's built-in HDMI port
      (or into a second Thunderbolt 5 port on the Mac via USB-C-to-DisplayPort).
  Option B:
    - Keep Monitor A plugged into the dock's DisplayPort output.
    - Plug Monitor B into the dock's rear Thunderbolt 4 Downstream USB-C port
      using a USB-C-to-DisplayPort cable so Thunderbolt tunnels a second
      independent DisplayPort stream.
==============================================================================
Bootstrap complete!
EOF
