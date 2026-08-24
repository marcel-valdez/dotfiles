# google_office → main_ubuntu merge tracking

This file tracks selective, feature-by-feature merges of the `google_office`
branch into `main_ubuntu`, so the next merge only has to look at what changed
*since* the point recorded here, and doesn't have to re-litigate decisions
already made.

## Last synced

- **google_office commit:** `4c2e54b2543730c74b6ee1a5121db0a05fe8101f`
  (2026-08-21, ".config/kitty/kitty.conf: Add special launcher to create an
  SSH session in a new window.")
- **Merged into main_ubuntu via commits:** `b631026`..`2030534` (9 commits,
  listed at the bottom), applied on top of main_ubuntu `5bcde07`.
- **Merge-base at time of this merge:** `f217ad257f5aa382bc4ac9beb2dee5ba35dec73b`

## How to re-merge next time

1. `git fetch origin google_office`
2. `git log 4c2e54b2543730c74b6ee1a5121db0a05fe8101f..origin/google_office --oneline`
   to see exactly what's new since this sync.
3. Branch from `main_ubuntu`, work cluster-by-cluster as before (see decisions
   below for what each cluster covers and why some things were skipped —
   re-check skipped items in case they've since become fixes worth taking).
4. Verify each cluster before committing: `bash -n` / `ruby -c` new scripts,
   `check-parens` + a batch emacs load for `.el` changes, load kitty/tmux/rofi
   configs live (`kitty --config`, `tmux -f .tmux.conf new-session -d`,
   `rofi -config ... -dump-config`).
5. Update the "Last synced" section above with the new google_office tip SHA
   and date, and append a dated entry under "Decisions" for anything new.

## Decisions by cluster (2026-08-23 merge)

**1. AI-agent tooling & logging** — `lib/log_lib.sh`, `scripts/jetski_*`,
`scripts/gemini_*`, `agent_notify.sh`, `run_workflows_sync.sh`,
`wait_for_guitar_workflow.sh`, `bisect_using_cls_file.sh`,
`retry_after_key.sh`, `ssh_keep_alive.sh`. Ported wholesale — new files, zero
conflict with main_ubuntu.

**2. Bash / ble.sh shell integration** — hand-merged into `.bashrc` rather
than overwritten, since main_ubuntu's fzf keybinding block was already ahead
of google_office's. Added: bash-preexec/precmd/preexec hooks, ble.sh loading
with `ble-attach` fast path, carapace completion engine, `.blerc`, the
dracula ble.sh color scheme, cargo PATH/env setup, and the `rg`-based Gemini
CLI search config. All additions are guarded by file/type existence checks —
no-op until ble.sh/carapace are actually installed on this machine.

**3. Kitty + termit terminal** — adopted kitty wholesale as the primary
terminal (user's explicit call — main_ubuntu had moved on from wezterm at
the office). Brought in termit too even though it's secondary, per explicit
request. `.wezterm.lua` left alone (not deleted; google_office deleted it
entirely, so there was nothing to port back). Two fixes made during the
port: commented out two SSH-launcher keybindings that shelled out to the
corp-only `~/.googlerc.d/bin/kitty-ssh-launch`; dropped the `"ease-out
linear"` easing suffix from `visual_bell_duration` since it isn't supported
by kitty 0.32.2 (installed here) and failed config parsing outright.

**4. tmux improvements** — adopted the async status-bar daemon
(`scripts/tmux_status_daemon.sh`, pushes `status-right` on its own loop
instead of recomputing it every `status-interval` tick), per-pane titles
with a zoom-aware `pane-border-format`, zoom-preserving pane/window
navigation, the kitty `terminal-features` entry, and `bin/emacs_resurrect.sh`
for tmux-resurrect (with its corp-only `/google/src/cloud/` google3
client-directory branches commented out — dead code outside the office
machine, and it referenced the corp-only
`~/.googlerc.d/.google_functions`). Removed a leftover duplicate
`pane-border-status 'off'` from main_ubuntu's old config that would have
silently overridden the new `'top'` setting.

**5. Emacs** — ported `action-logger.el` and the small `in-tmux.el` mouse fix
as-is (both generic, no corp dependency). Ported `at-office.el` but commented
out *every* corp-only piece: the `google`/`google3-eglot`/`llm-goose`/
`cider-agent`/`ubdiff`/`compilation-colorization` requires, the
google3-eglot/eldoc python setup, the entire llm-goose chat workflow
(`at-office/llm-goose`, `llm-goose-gen`), and the `/google/src/cloud/`
desktop-save hook. Left active: the three hostname-detection vars (harmless,
evaluate to nil off corp) and the generic buffer/region helper functions
(currently unused now that the LLM workflow is disabled, but kept as a
template in case a gptel-based replacement gets wired up later).
**Skipped `org-mode-config.el` entirely** — main_ubuntu's inline `.emacs`
org-agenda config is already a *more evolved* version of the same GTD setup
(TODAY/WEEK/MONTH/QUARTER-based tag filters vs. google_office's older
NOW/UNPLANNED/MUST_DO version). **Re-check this specifically on the next
merge** in case google_office's org-mode-config.el has since caught up or
diverged further in a way worth taking.

**6. Rofi window switcher** — `.config/rofi/{common_config,config,window}.rasi`
and `bin/rofi-alt-tab`. Ported wholesale, new files, no corp references.

**7. Misc quality-of-life scripts** — new: `scripts/zen-mode.sh`,
`.local/bin/pixel-buds-plugin.sh`, `scripts/whatsapp-notification.sh` (fixed
a stray `a#!/usr/bin/env bash` shebang typo present in google_office's
source itself), `scripts/restart_sound_service.sh`,
`scripts/detect_display_config.sh`. Hand-picked real bug fixes into existing
scripts:
  - `bin/beep`: fixed a quoting bug where the numeric-argument regex was
    quoted (`'^[0-9]+$'`), so `=~` matched it literally and the validation
    always fell through.
  - `bin/notify-tmux`: fixed the arg-parsing loop consuming the first
    positional argument (the title) before the `*) break` case could see it.
  - `bin/remind-me`: added `DISPLAY` passthrough and a 4-hour
    `notify-send --expire-time` so reminders don't vanish instantly.
    **Explicitly did not** take google_office's `-s/--start` → `-a/--at`
    rename, since `-a` was already taken by `--alarm` in this script —
    verified with a Ruby `OptionParser` repro that this collision makes
    `-a` resolve ambiguously to `--alarm`, silently breaking time-setting.
  - `bin/start-conky`: delayed startup by 20s and properly detached it as a
    background daemon (`nohup` + `disown`) instead of blocking `.xinitrc`.
  - `scripts/measure_ping_latency.sh`: dropped the `SIGKILL` trap (bash
    rejects trapping `SIGKILL`; it was a silent no-op).

  **Skipped:** `bin/watchfile` (google_office's version is a feature
  regression — drops `--silent` and the `--directory` long alias, and
  changes regex matching to drop the directory prefix); `bin/get-line`
  (google_office's shebang there is the broken `# !/bin/bash`, a
  commented-out shebang); `scripts/setup_work_environment.sh` (already a
  corp-only gcloud/SSH-tunnel/gcert script; google_office's changes gate its
  entire `main` behind a hard `gcertstatus`/`gcert` check, making it *more*
  corp-locked, not less — not worth taking here).

**8. Small dotfile tweaks + pomodoro sounds** — merged `.gitignore`
carefully: added missing unignore entries for files already tracked
(`.profile`, `.quotes`, `.XCompose`, `.Xmodmap`, `.xinitrc`, `.xsession`,
`.xprofile`, `.xsessionrc`, `res/`), added scoped unignores for the new
kitty/rofi/termit configs and `.local/bin/pixel-buds-plugin.sh` — deliberately
scoped that last one to the single file rather than unignoring all of
`.local/bin/*`, which would have exposed dozens of unrelated real binaries
(`pip`, `go`, `tmux`, `wezterm`, etc.) already sitting in `~/.local/bin`.
Removed the now-stale `.emacs.d/lisp/at-office.el` re-ignore rule since it's
intentionally tracked now. Kept main_ubuntu's `.wezterm.lua`/`.viminfo`/
`.vimrc` unignores and its recipes-folder/emacs-backup ignore rules, which
google_office's `.gitignore` had simply dropped. Also merged: `.profile`
(quoting consistency + cargo env sourcing), `.quotes` (one new quote),
`.pomodororc` (per-event sound files played via `systemd-run --user ffplay`
instead of a single generic sound via a commented-out `play` call) plus the
four new mp3 resources it references.

  **Skipped:** `.quotes.conkyrc` and `.xbindkeysrc.scm` (both tuned to
  google_office's specific monitor layout / keyboard-remap setup rather than
  portable fixes; `.xbindkeysrc.scm` controls live X11 global hotkeys, not
  worth the regression risk); `.nanorc` (google_office's version is just
  missing the findnext/findprevious bindings main_ubuntu added later — not a
  fix, just older); `.minttyrc` (Cygwin/Windows terminal config, not
  applicable on Linux).

## Explicitly not ported at all (re-evaluate each time)

- **Legacy `bin/` scripts** google_office deleted: `wine-use-win32`,
  `wine-use-win64`, `start-steam`, `restart-steam`, `kill-steam`,
  `remote-desktop`, `remote-desktop-performance`,
  `remote-desktop-single-monitor`, `mail-notify`, `mail-notify-impl`,
  `crypto-folder`, `encrypt-folder`, `decrypt-folder`, `reinstall_modules`,
  `ps-nice`, `ps-show-memory-hogs`, `rename-file`, `netcat-file`,
  `show-wifi-report`, `run-with-log`, `monodevelop-tdd`,
  `make-todo-window`, `tex-update-fonts`, `bin/functions`. Kept on
  main_ubuntu because it's treated as still needing them (gaming/legacy
  personal machine). Reconsider if that stops being true.
- termit config (`.config/termit/rc.lua`) was ported per explicit request
  even though kitty is primary — termit itself isn't installed on this
  machine, so beyond being copied faithfully it's unverified.

## Commits that implemented this merge

```
b631026 Add AI-agent tooling & logging scripts from google_office
b563da8 Add ble.sh + carapace shell integration from google_office
81eab0c Adopt kitty terminal config from google_office; bring in termit too
6919af2 Port tmux improvements from google_office: async status bar, per-pane titles, zoom-aware navigation
9081f16 Port emacs additions from google_office: action-logger, at-office helpers, mouse fix
24d0af3 Add rofi window switcher config from google_office
c8ebfbd Add misc quality-of-life scripts and small fixes from google_office
dd1b2c6 Small dotfile tweaks + pomodoro sounds from google_office
2030534 scripts/whatsapp-notification.sh: fix stray "a" typo breaking the shebang
```
