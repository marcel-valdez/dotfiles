#!/usr/bin/env bash
# ==============================================================================
# bootstrap_macos.sh: Idempotent gMac M5 Pro Bootstrap & Verification Script
# ==============================================================================
# Sets up a new Google corporate Apple Silicon MacBook Pro (gMac Darwin arm64)
# using Google's internal `mule` package manager (/usr/local/bin/mule),
# user-space CLI binaries in ~/bin, compiled Go/C++ helpers, macOS system
# defaults, and LaunchAgents (without requiring Homebrew).
#
# Usage:
#   ~/scripts/bootstrap_macos.sh [--dry-run] [--help|-h]
#   ~/.googlerc.d/scripts/bootstrap_macos.sh [--dry-run] [--help|-h]
# ==============================================================================

set -euo pipefail

# Ensure gMac Mule paths (/usr/local/bin, /usr/local/go/bin, ~/bin) and optional
# Homebrew paths are in PATH before checking tool availability.
for _dir in "${HOME}/bin" "${HOME}/.local/bin" "/usr/local/go/bin" "/usr/local/bin" "/opt/homebrew/bin" "/opt/homebrew/sbin"; do
  if [[ -d "${_dir}" && ":${PATH}:" != *":${_dir}:"* ]]; then
    export PATH="${_dir}:${PATH}"
  fi
done
unset _dir

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

install_github_tarball_bin() {
  local bin_name="$1"
  local repo="$2"
  local asset_Substring="$3"

  if command -v "${bin_name}" >/dev/null 2>&1 && (( DRY_RUN == 0 )); then
    echo "[INFO] ${bin_name} already installed at $(command -v "${bin_name}")"
    return 0
  fi

  if (( DRY_RUN == 1 )); then
    printf '[DRY-RUN] install_github_tarball_bin %s (%s -> ~/bin/%s)\n' "${bin_name}" "${repo}" "${bin_name}"
    return 0
  fi

  echo "[INFO] Installing ${bin_name} from GitHub release (${repo}) into ${HOME}/bin/${bin_name}..."
  local tmp_dir
  tmp_dir="$(mktemp -d)"
  local download_url
  download_url="$(python3 - "${repo}" "${asset_Substring}" << 'PY' || true
import json, sys, urllib.request
repo, substr = sys.argv[1], sys.argv[2]
req = urllib.request.Request(
    f"https://api.github.com/repos/{repo}/releases/latest",
    headers={"User-Agent": "bootstrap_macos.sh"}
)
with urllib.request.urlopen(req, timeout=15) as resp:
    data = json.load(resp)
for asset in data.get("assets", []):
    name = asset.get("name", "")
    if substr in name and (name.endswith(".tar.gz") or not "." in name):
        print(asset.get("browser_download_url", ""))
        break
PY
)"
  if [[ -z "${download_url}" ]]; then
    echo "[WARN] Could not resolve GitHub release URL for ${bin_name} (${repo}); skipping."
    rm -rf "${tmp_dir}"
    return 0
  fi

  if [[ "${download_url}" == *.tar.gz ]]; then
    if curl -fsSL "${download_url}" -o "${tmp_dir}/pkg.tar.gz"; then
      tar -xzf "${tmp_dir}/pkg.tar.gz" -C "${tmp_dir}"
      local found_bin
      found_bin="$(find "${tmp_dir}" -type f -name "${bin_name}" | head -n 1)"
      if [[ -n "${found_bin}" ]]; then
        cp "${found_bin}" "${HOME}/bin/${bin_name}"
        chmod +x "${HOME}/bin/${bin_name}"
      fi
    fi
  else
    if curl -fsSL "${download_url}" -o "${HOME}/bin/${bin_name}"; then
      chmod +x "${HOME}/bin/${bin_name}"
    fi
  fi
  rm -rf "${tmp_dir}"
}

echo "=============================================================================="
echo "gMac M5 Pro Dual-OS Dotfiles & Daemon Bootstrap (DRY_RUN=${DRY_RUN})"
echo "=============================================================================="

if [[ "$(uname -s)" != "Darwin" ]] && (( DRY_RUN == 0 )); then
  echo "[ERROR] bootstrap_macos.sh must be run on macOS (Darwin), or pass --dry-run." >&2
  exit 1
fi

run_cmd mkdir -p "${HOME}/bin" "${HOME}/.local/bin" "${HOME}/modules" "${HOME}/.cache" "${HOME}/tmp" "${HOME}/notes" "${HOME}/gtd"

# ------------------------------------------------------------------------------
# Stage 1: Google Internal `mule` Package Manager (/usr/local/bin/mule)
# ------------------------------------------------------------------------------
echo ""
echo "--- Stage 1: Google Internal Mule Packages (/usr/local/bin/mule) ---"
MULE_BIN="/usr/local/bin/mule"
if [[ ! -x "${MULE_BIN}" && -x "/Library/GoogleCorpSupport/bin/mule" ]]; then
  MULE_BIN="/Library/GoogleCorpSupport/bin/mule"
fi

# Packages verified in Google's corp-mule repository for gMac:
# - CLI & Toolchain: tmux, emacs, go, gmosh, mosh, git, gh, jj, gosso, bash-preexec
# - Desktop & Input: kitty, karabiner-elements, maccy, scroll-reverser, betterdisplay, rectangle
# - Fonts: roboto-font, google-sans
MULE_PACKAGES=(
  tmux
  emacs
  kitty
  karabiner-elements
  maccy
  scroll-reverser
  betterdisplay
  rectangle
  go
  gmosh
  mosh
  git
  gh
  jj
  gosso
  bash-preexec
  roboto-font
  google-sans
)

if [[ -x "${MULE_BIN}" ]] || (( DRY_RUN == 1 )); then
  run_cmd sudo "${MULE_BIN}" update || echo "[WARN] 'sudo mule update' returned non-zero; continuing with existing /etc/mule/mule.db."
  for pkg in "${MULE_PACKAGES[@]}"; do
    run_cmd sudo "${MULE_BIN}" install -noprompt "${pkg}" || \
      echo "[WARN] Mule package '${pkg}' could not be installed (may already be managed or unavailable on this track)."
  done
else
  echo "[WARN] ${MULE_BIN} not found on this machine; skipping Stage 1 Mule installation."
fi

# NOTE: Never chown /usr/local on gMac (breaks mrmagoo CheckExecutableSafety).
# Symlink Kitty and Emacs CLI binaries from /Applications into ~/bin instead.
if [[ -d "/Applications/kitty.app/Contents/MacOS" ]] || (( DRY_RUN == 1 )); then
  run_cmd ln -sfn "/Applications/kitty.app/Contents/MacOS/kitty" "${HOME}/bin/kitty"
  run_cmd ln -sfn "/Applications/kitty.app/Contents/MacOS/kitten" "${HOME}/bin/kitten"
fi
if [[ -d "/Applications/Emacs.app/Contents/MacOS" ]] || (( DRY_RUN == 1 )); then
  run_cmd ln -sfn "/Applications/Emacs.app/Contents/MacOS/Emacs" "${HOME}/bin/emacs"
  run_cmd ln -sfn "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient" "${HOME}/bin/emacsclient"
fi

# ------------------------------------------------------------------------------
# Stage 2: User-Space CLI Tools (~/bin) & Optional Homebrew Fallback
# ------------------------------------------------------------------------------
# Note: fzf, rg, fd, bat, and jq are NOT available in Google's corp-mule catalog;
# they are installed directly from upstream GitHub darwin_arm64 releases into
# ~/.fzf/bin and ~/bin.
echo ""
echo "--- Stage 2: User-Space CLI Binaries (fzf, rg, fd, bat, jq) & Optional Brew ---"
if [[ ! -d "${HOME}/.fzf" ]]; then
  run_cmd git clone --depth 1 https://github.com/junegunn/fzf.git "${HOME}/.fzf" || true
fi
# Remove any dangling ~/bin/fzf symlink before running ~/.fzf/install so
# link_fzf_in_path() does not short-circuit on a broken symlink in PATH.
if [[ -L "${HOME}/bin/fzf" && ! -e "${HOME}/bin/fzf" ]]; then
  rm -f "${HOME}/bin/fzf"
fi
if [[ -x "${HOME}/.fzf/install" ]] || (( DRY_RUN == 1 )); then
  run_cmd "${HOME}/.fzf/install" --bin --key-bindings --completion --no-update-rc || true
fi
# Fallback: if ~/.fzf/install could not download ~/.fzf/bin/fzf, fetch the
# prebuilt darwin_arm64 release binary directly from GitHub releases.
if [[ ! -x "${HOME}/.fzf/bin/fzf" ]] && (( DRY_RUN == 0 )); then
  install_github_tarball_bin "fzf" "junegunn/fzf" "darwin_arm64.tar.gz"
  if [[ -x "${HOME}/bin/fzf" && ! -L "${HOME}/bin/fzf" ]]; then
    mkdir -p "${HOME}/.fzf/bin"
    mv "${HOME}/bin/fzf" "${HOME}/.fzf/bin/fzf"
  fi
fi
if [[ -d "${HOME}/.fzf/bin" ]] || (( DRY_RUN == 1 )); then
  run_cmd ln -sfn "${HOME}/.fzf/bin/fzf" "${HOME}/bin/fzf"
  run_cmd ln -sfn "${HOME}/.fzf/bin/fzf-tmux" "${HOME}/bin/fzf-tmux"
fi

install_github_tarball_bin "rg" "BurntSushi/ripgrep" "aarch64-apple-darwin.tar.gz"
install_github_tarball_bin "fd" "sharkdp/fd" "aarch64-apple-darwin.tar.gz"
install_github_tarball_bin "bat" "sharkdp/bat" "aarch64-apple-darwin.tar.gz"
install_github_tarball_bin "jq" "jqlang/jq" "jq-macos-arm64"

if command -v brew >/dev/null 2>&1; then
  echo "[INFO] Optional Homebrew detected; installing supplementary packages not in Mule..."
  run_cmd brew install flock cmake fzf ripgrep fd bat jq terminal-notifier displayplacer sox || true
  run_cmd brew install --cask hammerspoon linearmouse swiftbar autoraise font-azeret-mono font-jetbrains-mono font-inter || true
else
  cat << 'EOF'
[INFO] Homebrew is not installed (standard on gMac).
       All core packages were installed via Google's internal `mule` tool and
       user-space `~/bin` binaries, and `~/lib/os_compat.sh` uses native macOS
       fallbacks (`osascript` notifications, `afplay` audio, `pbcopy`/`pbpaste`).
EOF
fi

# ------------------------------------------------------------------------------
# Stage 3: External Git Modules (~/modules/pomodoro, ~/.googlerc.d) & Helpers
# ------------------------------------------------------------------------------
echo ""
echo "--- Stage 3: External Modules & Native C++ Helper Compilation ---"
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

# Ensure ~/.ssh/config contains the Cloudtop (gcloud) and office host aliases
if ! grep -q "^Host gcloud$" "${HOME}/.ssh/config" 2>/dev/null || (( DRY_RUN == 1 )); then
  run_cmd mkdir -p -m 700 "${HOME}/.ssh"
  if (( DRY_RUN == 1 )); then
    printf '[DRY-RUN] Ensure ~/.ssh/config contains Host gcloud, gcloud_clipboard_tunnel, gcloud_aw_tunnel, and office (mode 0600)\n'
  elif [[ -f "${HOME}/.googlerc.d/versioned.ssh_config" ]]; then
    cat "${HOME}/.googlerc.d/versioned.ssh_config" >> "${HOME}/.ssh/config"
    chmod 600 "${HOME}/.ssh/config"
  else
    cat << 'EOF' >> "${HOME}/.ssh/config"
Match host "*.c.googlers.com,*.corp.google.com"
  ControlMaster auto
  ControlPath ~/.ssh/%r@%h:%p
  ControlPersist 19h
  ForwardAgent yes
  # Check connection every 15 seconds.
  ServerAliveInterval 15
  # Four failed retries before declaring disconnection.
  ServerAliveCountMax 4


Host gcloud_clipboard_tunnel
  HostName marcelvaldez.c.googlers.com
  Port 22
  User marcelvaldez

Host gcloud_aw_tunnel
  HostName marcelvaldez.c.googlers.com
  Port 22
  User marcelvaldez
  RemoteForward 5601 localhost:5600

Host gcloud
  HostName marcelvaldez.c.googlers.com
  Port 22
  User marcelvaldez
  ForwardX11 yes
  # RemoteForward 3333 localhost:3333
  # RemoteForward 5601 localhost:5600

Host office
  HostName marcelvaldez.mtv.corp.google.com
  Port 22
  User marcelvaldez
  ForwardX11 yes
EOF
    chmod 600 "${HOME}/.ssh/config"
  fi
fi

TMUX_MEM_CPU_DIR="${HOME}/.tmux/plugins/tmux-mem-cpu-load"
if [[ -d "${TMUX_MEM_CPU_DIR}" ]] || (( DRY_RUN == 1 )); then
  if command -v cmake >/dev/null 2>&1 || (( DRY_RUN == 1 )); then
    run_cmd bash -c "cd '${TMUX_MEM_CPU_DIR}' && cmake . && make -j4"
  else
    echo "[INFO] cmake not installed; skipping optional tmux-mem-cpu-load compilation."
  fi
fi

# ------------------------------------------------------------------------------
# Stage 4: Go Daemons (whatsapp-notification & airgradient-monitor) + Santa Hint
# ------------------------------------------------------------------------------
echo ""
echo "--- Stage 4: Go Daemon Compilation (darwin/arm64) & Santa Binary Authorization ---"
if [[ -d "/usr/local/go/bin" && ":${PATH}:" != *":/usr/local/go/bin:"* ]]; then
  export PATH="/usr/local/go/bin:${PATH}"
fi

if command -v go >/dev/null 2>&1 || (( DRY_RUN == 1 )); then
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
else
  echo "[WARN] 'go' binary not found in PATH (/usr/local/go/bin/go); skipping Go daemon builds."
fi

cat << 'EOF'
[INFO] Note on Google macOS Santa Binary Authorization:
       If Google Santa blocks execution of a locally compiled Go/C++ binary:
         1. Run: santactl fileinfo <path-to-binary>
         2. Open the printed Rule/Upvote URL to self-approve your binary hash.
EOF

# ------------------------------------------------------------------------------
# Stage 5: macOS System Defaults for Linux Power Users
# ------------------------------------------------------------------------------
echo ""
echo "--- Stage 5: macOS System Defaults (Key Repeat, Spaces, Window Drag, Maccy) ---"
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
# Stage 6: Symlink & Load LaunchAgents (knock, clipboard, whatsapp, airgradient)
# ------------------------------------------------------------------------------
echo ""
echo "--- Stage 6: LaunchAgent Registration (~/Library/LaunchAgents) ---"
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
# Stage 7: MacBook Pro 14" (M5 Pro) + ThinkPad TB4 Dock (40B0) Cabling Checklist
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
