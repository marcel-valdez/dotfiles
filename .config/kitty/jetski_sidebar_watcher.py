"""Kitty 0.48.2 watcher for the Jetski AI Session Sidebar Workbench.

Handles:
  1. `on_set_user_var` with `key == "jetski_sidebar_action"` (`focus_main:*`):
     focuses the paired main Kitty window (`jetski_main_win_id`).
  2. `on_set_user_var` with `key == "jetski_sidebar_place"` (`<side>:<bias>`):
     atomically activates the sidebar window within its own tab and places it
     at the requested screen edge with the requested split bias without global
     focus races.
  3. `on_close`: directly unlinks the per-OS-window state file when Kitty is
     shutting down or the OS window has already closed; otherwise spawns
     `kitty-jetski-sidebar restore-only <os_win_id> <sidebar_win_id>` with
     `KITTY_LISTEN_ON` and `KITTY_PID` propagated so manual `C-q` exits restore
     tab layout and OS-window geometry cleanly.

Usage Example:
  # Registered per-window via:
  #   kitten @ launch --watcher="~/.config/kitty/jetski_sidebar_watcher.py" ...
"""

from collections.abc import Mapping
import os
import pathlib
import subprocess
from typing import Protocol

CACHE_DIR = pathlib.Path.home() / ".cache" / "kitty_jetski_sidebar"
SIDEBAR_SCRIPT = (
    pathlib.Path.home() / ".googlerc.d" / "bin" / "kitty-jetski-sidebar"
)
_VALID_SIDES: frozenset[str] = frozenset({"left", "right"})


class _TabLike(Protocol):

  def set_active_window(self, window: "_WindowLike") -> object: ...

  def layout_action(
      self, action_name: str, args: tuple[str, ...]
  ) -> object: ...


class _WindowLike(Protocol):
  id: int
  user_vars: Mapping[str, str]

  def tabref(self) -> _TabLike | None: ...


class _BossLike(Protocol):
  window_id_map: Mapping[int, _WindowLike]
  os_window_map: Mapping[int, object]
  shutting_down: bool
  listening_on: str

  def set_active_window(self, window: _WindowLike) -> object: ...


def _parse_int(raw: str | None) -> int | None:
  if raw is None:
    return None
  stripped = str(raw).strip()
  if not stripped or not stripped.isdigit():
    return None
  return int(stripped)


def _handle_focus_main(boss: _BossLike, window: _WindowLike) -> None:
  """Focuses the paired main Kitty window recorded in `jetski_main_win_id`."""
  main_win_id = _parse_int(window.user_vars.get("jetski_main_win_id"))
  if main_win_id is None:
    return
  target = boss.window_id_map.get(main_win_id)
  if target is not None:
    boss.set_active_window(target)


def _handle_place_sidebar(window: _WindowLike, value: str) -> None:
  """Atomically moves and biases `window` within its owning Kitty tab."""
  side, sep, bias_raw = value.partition(":")
  if not sep or side not in _VALID_SIDES:
    return
  bias_pct = _parse_int(bias_raw)
  if bias_pct is None or not (1 <= bias_pct <= 99):
    return
  tab = window.tabref()
  if tab is None:
    return
  tab.set_active_window(window)
  tab.layout_action("move_to_screen_edge", (side,))
  tab.layout_action("bias", (str(bias_pct),))


def on_set_user_var(
    boss: _BossLike,
    window: _WindowLike,
    data: Mapping[str, str],
) -> None:
  """Dispatches `jetski_sidebar_action` and `jetski_sidebar_place` user vars.

  Args:
    boss: Kitty Boss controller instance.
    window: Kitty Window emitting or receiving the user variable update.
    data: Event dictionary containing `"key"` and `"value"`.

  Usage Example:
    >>> # Invoked automatically by Kitty when a user variable changes.
  """
  key = str(data.get("key", ""))
  value = str(data.get("value", "") or "")
  if key == "jetski_sidebar_action" and value.startswith("focus_main:"):
    _handle_focus_main(boss, window)
  elif key == "jetski_sidebar_place":
    _handle_place_sidebar(window, value)


def on_close(
    boss: _BossLike,
    window: _WindowLike,
    data: Mapping[str, object],
) -> None:
  """Restores layout/geometry or cleans up orphaned state on sidebar close.

  Args:
    boss: Kitty Boss controller instance.
    window: Closing Kitty Window.
    data: Kitty close event metadata (unused).

  Usage Example:
    >>> # Invoked automatically by Kitty when a watched window closes.
  """
  del data
  if window.user_vars.get("jetski_sidebar") != "1":
    return
  os_win_id = _parse_int(window.user_vars.get("jetski_os_win_id"))
  if os_win_id is None:
    return

  kitty_pid = os.getpid()
  is_shutting_down = bool(getattr(boss, "shutting_down", False))
  os_win_present = os_win_id in getattr(boss, "os_window_map", {})

  # If Kitty is exiting or the OS window is already gone, unlink any orphaned
  # state file directly in Python and skip WM/kitten calls completely.
  if is_shutting_down or not os_win_present:
    state_file = CACHE_DIR / f"oswin_{kitty_pid}_{os_win_id}.json"
    try:
      state_file.unlink(missing_ok=True)
    except OSError:
      pass
    return

  env = dict(os.environ)
  env["KITTY_PID"] = str(kitty_pid)
  listening_on = str(getattr(boss, "listening_on", "") or "")
  if listening_on:
    env["KITTY_LISTEN_ON"] = listening_on

  try:
    subprocess.Popen(
        [
            str(SIDEBAR_SCRIPT),
            "restore-only",
            str(os_win_id),
            str(window.id),
        ],
        env=env,
        stdin=subprocess.DEVNULL,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
        start_new_session=True,
    )
  except OSError:
    pass
