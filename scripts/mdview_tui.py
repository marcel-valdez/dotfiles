"""Flicker-free raw-TTY Markdown + Kitty HiDPI viewer and Jetski commenter.

Uses synchronized terminal updates (`CSI ? 2026 h/l`) and alternate-screen mode
to scroll `mdcat` 24-bit typography alongside Kitty `U+10EEEE` virtual image
placements without mutating image-ID SGR foreground bits, while providing a
bottom-bar line commenter, `Ctrl+G` external Emacs editor support, and exact
Jetski review feedback export.

Usage Example:
  outcome = run_interactive_tui("/path/to/plan.md", rendered_doc)
"""

from __future__ import annotations

from collections.abc import Callable
import dataclasses
import enum
import os
import re
import select
import shutil
import signal
import subprocess
import tempfile
import termios
import tty

import mdview
import mdview_constants
import mdview_kitty
import mdview_review_store
import mdview_source_map


_SGR_MOUSE_RE = re.compile(r"^\x1b\[<(\d+);(\d+);(\d+)([Mm])$")
_CSI_U_RE = re.compile(r"^\x1b\[(\d+)(?::\d+)*(?:;(\d+)(?::\d+)*)?u$")

_HELP_SECTIONS: tuple[tuple[str, tuple[tuple[str, str], ...]], ...] = (
    (
        "Navigation",
        (
            ("j / k, Up / Down, C-n / C-p", "Move caret down / up one row"),
            ("d / u, C-d / C-u", "Scroll half-page down / up"),
            ("Space / b, PgDn / PgUp", "Scroll full page down / up"),
            ("g / G, Home / End", "Jump to top / bottom of document"),
            ("/ , n / N", "Search text, next / previous match"),
            ("Mouse wheel / click", "Scroll viewport / move caret to row"),
        ),
    ),
    (
        "Commenting & Review",
        (
            ("c / Enter", "Add or edit inline comment at caret line"),
            ("E / C-g", "Edit comment in emacs-client.sh (also in inline prompt)"),
            ("x / D", "Delete comment on current line"),
            ("X", "Clear ALL saved draft comments for this file"),
            ("Tab / ] , Shift+Tab / [", "Jump to next / previous comment"),
            ("C", "Open Comments Drawer (Enter:jump e:edit E:emacs r:raw x:del)"),
        ),
    ),
    (
        "View & Diagrams",
        (
            ("+ / = , - , 0", "Zoom diagrams +25% / -25% / reset 100% (or Kitty Ctrl+Shift+=)"),
            ("r / R", "Toggle Raw Markdown vs Rendered / reload file from disk"),
            ("o", "Open another file via ranked fzf picker"),
            ("? / h", "Toggle this Keyboard Shortcuts help overlay"),
        ),
    ),
    (
        "Finish & Export",
        (
            ("q", "Copy [file.md] comments to clipboard & exit (keeps draft)"),
            ("A", "Copy [Approved] file.md to clipboard & exit (clears draft)"),
            ("Q", "Quit without copying to clipboard (keeps draft)"),
        ),
    ),
)


class InputMode(enum.StrEnum):
  """Active keyboard interaction sub-mode inside the viewer."""

  NORMAL = "normal"
  COMMENT = "comment"
  SEARCH = "search"
  DRAWER = "drawer"
  HELP = "help"


@dataclasses.dataclass(frozen=True)
class ExitOutcome:
  """Terminal outcome returned when the user exits the interactive viewer.

  Attributes:
    decision: Approval status (`comment_only`, `approved`, or `rejected`).
    feedback_text: Formatted Jetski review block (empty if no comments/decision).
    copy_to_clipboard: Whether `feedback_text` should be exported to clipboards.
    switch_to_picker: Whether the user pressed `o` to pick another file.
  """

  decision: mdview_review_store.ReviewStatus
  feedback_text: str
  copy_to_clipboard: bool
  switch_to_picker: bool = False


def write_all_fd(fd: int, data: bytes) -> None:
  """Writes every byte in `data` to `fd`, looping across partial PTY writes.

  Args:
    fd: Open writable file descriptor (e.g., `/dev/tty`).
    data: Byte payload to transmit completely.

  Usage Example:
    write_all_fd(tty_fd, b"\\x1b[?2026h...\\x1b[?2026l")
  """
  view = memoryview(data)
  while view:
    try:
      written = os.write(fd, view)
    except InterruptedError:
      continue
    except BlockingIOError:
      select.select([], [fd], [], 0.05)
      continue
    if written <= 0:
      break
    view = view[written:]


def resolve_comment_editor_cmd(
    emacs_client_script: str | None = None,
) -> list[str]:
  """Resolves the external TTY editor command for multi-line review comments.

  Prefers `MDVIEW_COMMENT_EDITOR`, then `~/scripts/emacs-client.sh` (fast
  `tty-server` Emacs daemon client), falling back to standalone `emacs -nw` or
  `vi`.

  Args:
    emacs_client_script: Optional override path for `emacs-client.sh`.

  Returns:
    Command argv prefix list to which the temporary comment file path is
    appended.

  Usage Example:
    cmd = resolve_comment_editor_cmd()
  """
  if editor_override := os.environ.get("MDVIEW_COMMENT_EDITOR"):
    return editor_override.split()

  client_script = (
      os.path.expanduser("~/scripts/emacs-client.sh")
      if emacs_client_script is None
      else emacs_client_script
  )
  if os.path.isfile(client_script) and os.access(client_script, os.X_OK):
    return [client_script]

  emacs_bin = shutil.which("emacs")
  if not emacs_bin and os.access(
      "/Applications/Emacs.app/Contents/MacOS/Emacs", os.X_OK
  ):
    emacs_bin = "/Applications/Emacs.app/Contents/MacOS/Emacs"
  editor_cmd = [emacs_bin or "vi"]
  if editor_cmd[0].endswith(("emacs", "Emacs")):
    editor_cmd.append("--no-window-system")
  return editor_cmd


def resolve_real_tty_path() -> str:
  """Resolves the concrete PTY device path (e.g. `/dev/pts/42`) for child TUIs.

  `emacsclient --create-frame --tty` calls `ttyname(0)` and sends that path to
  the background `emacs --daemon` server. Passing a descriptor opened directly
  on `/dev/tty` makes `ttyname(0)` return `"/dev/tty"`, which the detached
  daemon cannot open (`ENXIO`: `Could not open file: /dev/tty`). Opening the
  underlying `/dev/pts/<N>` device instead lets the daemon attach cleanly.

  Returns:
    Concrete TTY device path if available on stdio FDs, else `"/dev/tty"`.

  Usage Example:
    tty_path = resolve_real_tty_path()
  """
  for fd in (0, 1, 2):
    try:
      if os.isatty(fd):
        name = os.ttyname(fd)
        if name and name != "/dev/tty":
          return name
    except OSError:
      continue
  return "/dev/tty"


def _default_external_editor(initial_text: str, source_line: int) -> str | None:
  """Opens `~/scripts/emacs-client.sh` (or fallback editor) on a temp buffer."""
  editor_cmd = resolve_comment_editor_cmd()

  with tempfile.NamedTemporaryFile(
      mode="w",
      encoding="utf-8",
      prefix=f"mdview-L{source_line}-",
      suffix=".md",
      delete=False,
  ) as tmp_f:
    tmp_f.write(initial_text)
    tmp_path = tmp_f.name

  try:
    tty_path = resolve_real_tty_path()
    with open(tty_path, "rb+", buffering=0) as tty_io:
      proc = subprocess.run(
          [*editor_cmd, tmp_path],
          stdin=tty_io,
          stdout=tty_io,
          stderr=tty_io,
          check=False,
      )
    if proc.returncode != 0:
      return None
    with open(tmp_path, encoding="utf-8") as f:
      return f.read().strip()
  finally:
    try:
      os.remove(tmp_path)
    except OSError:
      pass


class ViewerController:
  """Pure state machine and frame renderer for the interactive viewer/commenter."""

  def __init__(
      self,
      filepath: str,
      document: mdview.RenderedDocument,
      reviews_dir: str | None = None,
      term_cols: int = 120,
      term_rows: int = 40,
      external_editor_fn: Callable[[str, int], str | None] | None = None,
      reload_fn: Callable[[int, int, float], mdview.RenderedDocument] | None = None,
  ) -> None:
    """Initializes the viewer state and restores any saved draft comments.

    Args:
      filepath: Path to the Markdown document being viewed.
      document: Pre-rendered `RenderedDocument`.
      reviews_dir: Optional directory override for draft review JSON files.
      term_cols: Initial terminal width in character cells.
      term_rows: Initial terminal height in character cells.
      external_editor_fn: Optional callback `(initial_text, line_num) -> edited`
        invoked on `Ctrl+G` / `E`.
      reload_fn: Optional callback `(prose_cols, diagram_cols, zoom) ->
        RenderedDocument` invoked upon terminal resize, zoom, or `R` reload.
    """
    self.filepath = filepath
    self.document = document
    self.reviews_dir = reviews_dir
    self.term_cols = max(mdview_constants.MIN_COLUMNS, term_cols)
    self.term_rows = max(8, term_rows)
    self.external_editor_fn = external_editor_fn or _default_external_editor
    self.reload_fn = reload_fn
    self.diagram_zoom: float = 1.0

    self.state, self.file_changed_since_draft = (
        mdview_review_store.load_review_state(filepath, reviews_dir=reviews_dir)
    )
    self.mode: InputMode = InputMode.NORMAL
    self.raw_mode: bool = False
    self.input_buffer: str = ""
    self.input_cursor: int = 0
    self.comment_target_line: int = 1
    self.search_query: str = ""
    self.drawer_index: int = 0
    self.flash_message: str = (
        "File updated since saved draft (press X to clear old comments)"
        if self.file_changed_since_draft
        else ""
    )
    self.needs_image_reupload: bool = True
    self._clamp_positions()

  def active_lines(self) -> tuple[str, ...]:
    """Returns either rendered viewport lines or raw Markdown lines."""
    if self.raw_mode:
      sgr = mdview_constants.DRACULA_SGR
      return tuple(
          f"{sgr['fg']}{line}{sgr['reset']}" for line in self.document.raw_lines
      )
    return self.document.lines

  def current_source_line(self) -> int:
    """Returns the 1-based raw source line mapped to the current caret row.

    Returns:
      Positive 1-based line number in the underlying Markdown document.

    Usage Example:
      line_no = ctrl.current_source_line()
    """
    if self.raw_mode:
      total = max(1, len(self.document.raw_lines))
      return max(1, min(total, self.state.cursor_row + 1))
    if not self.document.source_lines:
      return 1
    idx = max(0, min(len(self.document.source_lines) - 1, self.state.cursor_row))
    return self.document.source_lines[idx]

  def _collapse_span_for_row(
      self, row_idx: int
  ) -> mdview_source_map.CollapseSpan | None:
    """Returns the diagram `CollapseSpan` covering `row_idx` in rendered mode."""
    if self.raw_mode:
      return None
    for span in self.document.collapse_spans:
      if span.start_row <= row_idx < span.end_row:
        return span
    return None

  def _comments_for_row(
      self, row_idx: int, src_line: int
  ) -> list[tuple[int, str]]:
    """Returns sorted `(line_no, comment)` pairs covering `row_idx`."""
    if (span := self._collapse_span_for_row(row_idx)) is not None:
      hi = (
          span.end_source_line
          if span.end_source_line >= span.source_line
          else span.source_line
      )
      return sorted(
          (ln, txt)
          for ln, txt in self.state.comments.items()
          if span.source_line <= ln <= hi
      )
    if (txt := self.state.comments.get(src_line)) is not None:
      return [(src_line, txt)]
    return []

  def viewport_height(self) -> int:
    """Calculates available document viewport rows above the bottom bars."""
    if self.mode == InputMode.DRAWER:
      drawer_rows = min(8, max(3, len(self.state.comments) + 1))
      return max(3, self.term_rows - drawer_rows - 2)
    src_line = self.current_source_line()
    has_preview = (
        self.mode in (InputMode.COMMENT, InputMode.SEARCH)
        or bool(self._comments_for_row(self.state.cursor_row, src_line))
        or bool(self.flash_message)
    )
    footer_rows = 3 if has_preview else 2
    return max(3, self.term_rows - footer_rows)

  def _clamp_positions(self) -> None:
    """Clamps `cursor_row` and `scroll_top` within active document bounds."""
    lines = self.active_lines()
    max_row = max(0, len(lines) - 1)
    self.state.cursor_row = max(0, min(max_row, self.state.cursor_row))
    vis = self.viewport_height()
    if self.state.cursor_row < self.state.scroll_top:
      self.state.scroll_top = self.state.cursor_row
    elif self.state.cursor_row >= self.state.scroll_top + vis:
      self.state.scroll_top = self.state.cursor_row - vis + 1
    max_scroll = max(0, len(lines) - vis)
    self.state.scroll_top = max(0, min(max_scroll, self.state.scroll_top))

  def _persist(self) -> None:
    """Saves draft state atomically to disk."""
    mdview_review_store.save_review_state(
        self.state, reviews_dir=self.reviews_dir
    )

  def _layout_column_budgets(self) -> tuple[int, int]:
    """Computes `(prose_cols, diagram_cols)` for current terminal dimensions."""
    avail_cols = max(
        mdview_constants.MIN_COLUMNS,
        self.term_cols - mdview_constants.GUTTER_WIDTH,
    )
    prose_cols = min(mdview_constants.DEFAULT_MAX_COLUMNS, avail_cols)
    diagram_cols = max(mdview_constants.MIN_COLUMNS, avail_cols - 2)
    return prose_cols, diagram_cols

  def _reload_document(self) -> None:
    """Re-renders the document at current layout budgets and diagram zoom."""
    if self.reload_fn is None:
      return
    target_src = self.current_source_line()
    prose_cols, diagram_cols = self._layout_column_budgets()
    self.document = self.reload_fn(prose_cols, diagram_cols, self.diagram_zoom)
    self._jump_to_source_line(target_src)
    self.needs_image_reupload = True

  def resize(self, new_cols: int, new_rows: int) -> None:
    """Updates terminal dimensions and re-renders prose and diagram sizing.

    Triggers reload on either column or row changes so Kitty font-size zoom
    (`Ctrl+Shift+=` / `Ctrl+Shift+-`) rescales HiDPI diagrams smoothly.

    Args:
      new_cols: Updated terminal column count.
      new_rows: Updated terminal row count.

    Usage Example:
      ctrl.resize(140, 50)
    """
    prev_cols = self.term_cols
    prev_rows = self.term_rows
    self.term_cols = max(mdview_constants.MIN_COLUMNS, new_cols)
    self.term_rows = max(8, new_rows)
    if self.reload_fn is not None and (
        self.term_cols != prev_cols or self.term_rows != prev_rows
    ):
      self._reload_document()
    self._clamp_positions()

  def _jump_to_source_line(self, target_line: int) -> None:
    """Moves `cursor_row` to the first viewport row matching `target_line`."""
    if self.raw_mode:
      self.state.cursor_row = max(0, target_line - 1)
      self._clamp_positions()
      return
    for span in self.document.collapse_spans:
      hi = (
          span.end_source_line
          if span.end_source_line >= span.source_line
          else span.source_line
      )
      if span.source_line <= target_line <= hi:
        self.state.cursor_row = span.start_row
        self._clamp_positions()
        return
    for idx, src_line in enumerate(self.document.source_lines):
      if src_line >= target_line:
        self.state.cursor_row = idx
        self._clamp_positions()
        return
    self.state.cursor_row = max(0, len(self.document.lines) - 1)
    self._clamp_positions()

  def _jump_comment(self, forward: bool) -> None:
    """Jumps caret to the next or previous commented source line."""
    if not self.state.comments:
      self.flash_message = "No comments recorded yet (press c to add one)"
      return
    sorted_lines = sorted(self.state.comments)
    cur = self.current_source_line()
    cur_lo = cur
    cur_hi = cur
    if (span := self._collapse_span_for_row(self.state.cursor_row)) is not None:
      cur_lo = span.source_line
      cur_hi = (
          span.end_source_line
          if span.end_source_line >= span.source_line
          else span.source_line
      )
    if forward:
      for line_no in sorted_lines:
        if line_no > cur_hi:
          self._jump_to_source_line(line_no)
          return
      self._jump_to_source_line(sorted_lines[0])
    else:
      for line_no in reversed(sorted_lines):
        if line_no < cur_lo:
          self._jump_to_source_line(line_no)
          return
      self._jump_to_source_line(sorted_lines[-1])

  def _run_search(self, forward: bool) -> None:
    """Moves caret to the next or previous case-insensitive match."""
    if not self.search_query:
      return
    lines = self.active_lines()
    total = len(lines)
    if total == 0:
      return
    needle = self.search_query.lower()
    step = 1 if forward else -1
    for offset in range(1, total + 1):
      idx = (self.state.cursor_row + offset * step) % total
      plain = mdview_source_map.strip_terminal_escapes(lines[idx]).lower()
      if needle in plain:
        self.state.cursor_row = idx
        self._clamp_positions()
        self.flash_message = f"/{self.search_query}"
        return
    self.flash_message = f"Pattern not found: {self.search_query}"

  def _open_external_comment_editor(self, initial_text: str, line_no: int) -> None:
    """Invokes external Emacs editor and updates comment on `line_no`."""
    edited = self.external_editor_fn(initial_text, line_no)
    self.mode = InputMode.NORMAL
    self.needs_image_reupload = True
    if edited is None:
      self.flash_message = "External comment edit cancelled"
      return
    if edited.strip():
      self.state.comments[line_no] = edited.strip()
      self.flash_message = f"Saved comment on L{line_no}"
    else:
      self.state.comments.pop(line_no, None)
      self.flash_message = f"Removed empty comment on L{line_no}"
    self._persist()

  def _handle_readline_key(self, key: str) -> str | None:
    """Processes Emacs-style editing keys in `COMMENT` or `SEARCH` mode."""
    if key in ("\r", "\n"):
      return "SUBMIT"
    if key == "ESC":
      return "CANCEL"
    if key in ("\x7f", "\x08"):  # Backspace
      if self.input_cursor > 0:
        self.input_buffer = (
            self.input_buffer[: self.input_cursor - 1]
            + self.input_buffer[self.input_cursor :]
        )
        self.input_cursor -= 1
      return None
    if key in ("DELETE", "\x04"):  # Delete / C-d
      if self.input_cursor < len(self.input_buffer):
        self.input_buffer = (
            self.input_buffer[: self.input_cursor]
            + self.input_buffer[self.input_cursor + 1 :]
        )
      return None
    if key in ("LEFT", "\x02"):  # Left / C-b
      self.input_cursor = max(0, self.input_cursor - 1)
      return None
    if key in ("RIGHT", "\x06"):  # Right / C-f
      self.input_cursor = min(len(self.input_buffer), self.input_cursor + 1)
      return None
    if key in ("HOME", "\x01"):  # Home / C-a
      self.input_cursor = 0
      return None
    if key in ("END", "\x05"):  # End / C-e
      self.input_cursor = len(self.input_buffer)
      return None
    if key == "\x0b":  # C-k: kill to end
      self.input_buffer = self.input_buffer[: self.input_cursor]
      return None
    if key == "\x15":  # C-u: kill to start
      self.input_buffer = self.input_buffer[self.input_cursor :]
      self.input_cursor = 0
      return None
    if key == "\x17":  # C-w: kill previous word
      left = self.input_buffer[: self.input_cursor].rstrip()
      cut_idx = left.rfind(" ")
      new_pos = 0 if cut_idx < 0 else cut_idx + 1
      self.input_buffer = (
          self.input_buffer[:new_pos] + self.input_buffer[self.input_cursor :]
      )
      self.input_cursor = new_pos
      return None
    if len(key) == 1 and key.isprintable():
      self.input_buffer = (
          self.input_buffer[: self.input_cursor]
          + key
          + self.input_buffer[self.input_cursor :]
      )
      self.input_cursor += 1
    return None

  def handle_key(self, key: str) -> ExitOutcome | None:
    """Processes one parsed key or mouse token and returns `ExitOutcome` on quit.

    Args:
      key: Parsed key string (single char, `"UP"`, `"ESC"`, `"MOUSE:..."`, etc.).

    Returns:
      `ExitOutcome` when exiting the viewer, or `None` to continue rendering.

    Usage Example:
      outcome = ctrl.handle_key("j")
    """
    if key.startswith("MOUSE:"):
      parts = key.split(":")
      btn, _, row_s, kind = int(parts[1]), int(parts[2]), int(parts[3]), parts[4]
      if btn == 64:  # Wheel up
        self.state.cursor_row -= 3
        self.state.scroll_top -= 3
        self._clamp_positions()
      elif btn == 65:  # Wheel down
        self.state.cursor_row += 3
        self.state.scroll_top += 3
        self._clamp_positions()
      elif btn == 0 and kind == "M" and self.mode == InputMode.NORMAL:
        clicked_idx = self.state.scroll_top + (row_s - 1)
        if 0 <= clicked_idx < len(self.active_lines()):
          self.state.cursor_row = clicked_idx
          self._clamp_positions()
      return None

    if self.mode == InputMode.COMMENT:
      if key == "\x07":  # Ctrl+G -> external Emacs editor
        self._open_external_comment_editor(
            self.input_buffer, self.comment_target_line
        )
        return None
      status = self._handle_readline_key(key)
      if status == "SUBMIT":
        cleaned = self.input_buffer.strip()
        if cleaned:
          self.state.comments[self.comment_target_line] = cleaned
          self.flash_message = f"Saved comment on L{self.comment_target_line}"
        else:
          self.state.comments.pop(self.comment_target_line, None)
          self.flash_message = f"Cleared comment on L{self.comment_target_line}"
        self.mode = InputMode.NORMAL
        self._persist()
      elif status == "CANCEL":
        self.mode = InputMode.NORMAL
        self.flash_message = ""
      return None

    if self.mode == InputMode.SEARCH:
      if key == "\x07":
        self.mode = InputMode.NORMAL
        return None
      status = self._handle_readline_key(key)
      if status == "SUBMIT":
        self.search_query = self.input_buffer.strip()
        self.mode = InputMode.NORMAL
        self._run_search(forward=True)
      elif status == "CANCEL":
        self.mode = InputMode.NORMAL
      return None

    if self.mode == InputMode.HELP:
      if key in ("?", "h", "q", "ESC", "\x07", "\r", "\n", " "):
        self.mode = InputMode.NORMAL
        self.needs_image_reupload = True
      return None

    if self.mode == InputMode.DRAWER:
      sorted_lines = sorted(self.state.comments)
      if key in ("ESC", "C", "q", "\x07"):
        self.mode = InputMode.NORMAL
      elif key in ("j", "DOWN", "\x0e") and sorted_lines:
        self.drawer_index = min(len(sorted_lines) - 1, self.drawer_index + 1)
      elif key in ("k", "UP", "\x10") and sorted_lines:
        self.drawer_index = max(0, self.drawer_index - 1)
      elif key in ("\r", "\n") and sorted_lines:
        target = sorted_lines[min(self.drawer_index, len(sorted_lines) - 1)]
        self.mode = InputMode.NORMAL
        self._jump_to_source_line(target)
      elif key in ("e", "c") and sorted_lines:
        target = sorted_lines[min(self.drawer_index, len(sorted_lines) - 1)]
        self._jump_to_source_line(target)
        self.comment_target_line = target
        existing = self.state.comments.get(target, "")
        self.input_buffer = existing
        self.input_cursor = len(existing)
        self.mode = InputMode.COMMENT
      elif key == "E" and sorted_lines:
        target = sorted_lines[min(self.drawer_index, len(sorted_lines) - 1)]
        self._jump_to_source_line(target)
        existing = self.state.comments.get(target, "")
        self._open_external_comment_editor(existing, target)
      elif key == "r" and sorted_lines:
        target = sorted_lines[min(self.drawer_index, len(sorted_lines) - 1)]
        self.mode = InputMode.NORMAL
        self.raw_mode = True
        self._jump_to_source_line(target)
        self.flash_message = f"Raw Markdown view (L{target})"
      elif key in ("x", "D") and sorted_lines:
        target = sorted_lines[min(self.drawer_index, len(sorted_lines) - 1)]
        self.state.comments.pop(target, None)
        self.drawer_index = max(0, self.drawer_index - 1)
        self._persist()
        if not self.state.comments:
          self.mode = InputMode.NORMAL
      return None

    # NORMAL mode dispatch
    self.flash_message = ""
    vis = self.viewport_height()
    half = max(1, vis // 2)

    if key in ("j", "DOWN", "\x0e"):
      self.state.cursor_row += 1
      self._clamp_positions()
    elif key in ("k", "UP", "\x10"):
      self.state.cursor_row -= 1
      self._clamp_positions()
    elif key in (" ", "PGDN", "\x16", "f"):
      self.state.cursor_row += vis
      self.state.scroll_top += vis
      self._clamp_positions()
    elif key in ("b", "PGUP", "ALT_V"):
      self.state.cursor_row -= vis
      self.state.scroll_top -= vis
      self._clamp_positions()
    elif key in ("\x04", "d"):  # C-d / d: half-page down (less muscle memory)
      self.state.cursor_row += half
      self.state.scroll_top += half
      self._clamp_positions()
    elif key in ("\x15", "u"):  # C-u / u: half-page up
      self.state.cursor_row -= half
      self.state.scroll_top -= half
      self._clamp_positions()
    elif key in ("g", "HOME", "<", "ALT_LT"):
      self.state.cursor_row = 0
      self._clamp_positions()
    elif key in ("G", "END", ">", "ALT_GT"):
      self.state.cursor_row = max(0, len(self.active_lines()) - 1)
      self._clamp_positions()
    elif key in ("c", "\r", "\n"):
      line_no = self.current_source_line()
      self.comment_target_line = line_no
      existing = self.state.comments.get(line_no, "")
      self.input_buffer = existing
      self.input_cursor = len(existing)
      self.mode = InputMode.COMMENT
    elif key in ("E", "\x07"):
      line_no = self.current_source_line()
      existing = self.state.comments.get(line_no, "")
      self._open_external_comment_editor(existing, line_no)
    elif key in ("x", "D"):
      line_no = self.current_source_line()
      if line_no in self.state.comments:
        self.state.comments.pop(line_no, None)
        self._persist()
        self.flash_message = f"Deleted comment on L{line_no}"
      else:
        self.flash_message = f"No comment on L{line_no}"
    elif key == "X":
      count = len(self.state.comments)
      self.state.comments.clear()
      mdview_review_store.clear_review_state(
          self.filepath, reviews_dir=self.reviews_dir
      )
      self.flash_message = f"Cleared all {count} draft comment(s)"
    elif key == "C":
      if self.state.comments:
        self.drawer_index = 0
        self.mode = InputMode.DRAWER
      else:
        self.flash_message = "No comments yet (press c to comment on a line)"
    elif key in ("\t", "]"):
      self._jump_comment(forward=True)
    elif key in ("SHIFT_TAB", "["):
      self._jump_comment(forward=False)
    elif key == "/":
      self.input_buffer = ""
      self.input_cursor = 0
      self.mode = InputMode.SEARCH
    elif key == "n":
      self._run_search(forward=True)
    elif key == "N":
      self._run_search(forward=False)
    elif key == "r":
      target_src = self.current_source_line()
      self.raw_mode = not self.raw_mode
      self._jump_to_source_line(target_src)
      if not self.raw_mode:
        self.needs_image_reupload = True
      self.flash_message = "Raw Markdown view" if self.raw_mode else "Rendered view"
    elif key == "R" and self.reload_fn is not None:
      self._reload_document()
      self.flash_message = "Reloaded from disk"
    elif key in ("+", "="):
      new_zoom = min(
          mdview_constants.MAX_DIAGRAM_ZOOM,
          round(self.diagram_zoom + mdview_constants.DIAGRAM_ZOOM_STEP, 2),
      )
      self.diagram_zoom = new_zoom
      self._reload_document()
      self.flash_message = f"Diagram zoom: {round(self.diagram_zoom * 100)}%"
    elif key in ("-", "_"):
      new_zoom = max(
          mdview_constants.MIN_DIAGRAM_ZOOM,
          round(self.diagram_zoom - mdview_constants.DIAGRAM_ZOOM_STEP, 2),
      )
      self.diagram_zoom = new_zoom
      self._reload_document()
      self.flash_message = f"Diagram zoom: {round(self.diagram_zoom * 100)}%"
    elif key == "0":
      self.diagram_zoom = 1.0
      self._reload_document()
      self.flash_message = "Diagram zoom: 100%"
    elif key in ("?", "h"):
      self.mode = InputMode.HELP
    elif key == "o":
      self._persist()
      return ExitOutcome(
          decision=mdview_review_store.ReviewStatus.COMMENT_ONLY,
          feedback_text="",
          copy_to_clipboard=False,
          switch_to_picker=True,
      )
    elif key == "q":
      self._persist()
      feedback = mdview_review_store.format_jetski_feedback(
          self.state.filename,
          self.state.comments,
          decision=mdview_review_store.ReviewStatus.COMMENT_ONLY,
      )
      return ExitOutcome(
          decision=mdview_review_store.ReviewStatus.COMMENT_ONLY,
          feedback_text=feedback,
          copy_to_clipboard=bool(feedback),
      )
    elif key == "A":
      feedback = mdview_review_store.format_jetski_feedback(
          self.state.filename,
          self.state.comments,
          decision=mdview_review_store.ReviewStatus.APPROVED,
      )
      mdview_review_store.clear_review_state(
          self.filepath, reviews_dir=self.reviews_dir
      )
      self.state.comments.clear()
      return ExitOutcome(
          decision=mdview_review_store.ReviewStatus.APPROVED,
          feedback_text=feedback,
          copy_to_clipboard=True,
      )
    elif key == "Q":
      self._persist()
      return ExitOutcome(
          decision=mdview_review_store.ReviewStatus.COMMENT_ONLY,
          feedback_text="",
          copy_to_clipboard=False,
      )

    return None

  def _format_gutter(self, row_idx: int, src_line: int, is_caret: bool) -> str:
    """Renders the 7-column gutter (`●▸ 34┃ ` or `2▸ 45┃ `) cleanly."""
    sgr = mdview_constants.DRACULA_SGR
    row_cmts = self._comments_for_row(row_idx, src_line)
    count = len(row_cmts)
    if count == 1:
      badge = f"{sgr['cyan']}●"
    elif count >= 2:
      count_char = str(count) if count < 10 else "+"
      badge = f"{sgr['yellow']}{count_char}"
    else:
      badge = " "
    if is_caret:
      return (
          f"{badge}{sgr['bold_purple']}▸{src_line:>3d}┃{sgr['reset']} "
      )
    return f"{badge}{sgr['comment']} {src_line:>3d}│{sgr['reset']} "

  def consume_pending_image_uploads(self) -> tuple[str, ...]:
    """Returns pending Kitty upload escapes once so they run outside `?2026h`.

    Returns:
      Tuple of Kitty APC/DCS upload strings to transmit before drawing the
      synchronized screen frame, or an empty tuple if already uploaded.

    Usage Example:
      uploads = ctrl.consume_pending_image_uploads()
    """
    if self.needs_image_reupload and not self.raw_mode:
      self.needs_image_reupload = False
      return self.document.upload_escapes
    return ()

  def _render_help_frame(self) -> str:
    """Renders the centered Keyboard Shortcuts help overlay frame."""
    sgr = mdview_constants.DRACULA_SGR
    box_w = min(max(50, self.term_cols - 4), 96)
    inner_w = box_w - 4
    key_col = min(28, max(16, inner_w // 3))
    desc_col = max(10, inner_w - key_col - 2)
    left_pad = " " * max(0, (self.term_cols - box_w) // 2)

    body_rows: list[str] = []
    title = " mdview Keyboard Shortcuts "
    top_rule = "─" * max(0, box_w - 2 - len(title))
    body_rows.append(
        f"{left_pad}{sgr['purple']}╭─{sgr['bold_purple']}{title}"
        f"{sgr['purple']}{top_rule[:-1] if len(top_rule) > 0 else ''}╮{sgr['reset']}"
    )
    for sec_idx, (sec_name, bindings) in enumerate(_HELP_SECTIONS):
      if sec_idx > 0:
        body_rows.append(
            f"{left_pad}{sgr['purple']}│{' ' * (box_w - 2)}│{sgr['reset']}"
        )
      sec_pad = " " * max(0, inner_w - len(sec_name))
      body_rows.append(
          f"{left_pad}{sgr['purple']}│ {sgr['yellow']}{sec_name}{sec_pad}"
          f"{sgr['purple']} │{sgr['reset']}"
      )
      for keys_label, desc in bindings:
        k_txt = keys_label[:key_col].ljust(key_col)
        d_txt = desc[:desc_col].ljust(desc_col)
        body_rows.append(
            f"{left_pad}{sgr['purple']}│ {sgr['cyan']}{k_txt}  "
            f"{sgr['fg']}{d_txt}{sgr['purple']} │{sgr['reset']}"
        )
    body_rows.append(
        f"{left_pad}{sgr['purple']}╰{'─' * (box_w - 2)}╯{sgr['reset']}"
    )

    out: list[str] = ["\x1b[?2026h\x1b[H"]
    avail_rows = max(3, self.term_rows - 1)
    if len(body_rows) > avail_rows and avail_rows >= 3:
      # Preserve top and bottom borders on compact terminal heights.
      visible_body = body_rows[: avail_rows - 1] + [body_rows[-1]]
    else:
      visible_body = body_rows
    top_margin = max(0, (avail_rows - len(visible_body)) // 2)
    for r_idx in range(avail_rows):
      out.append("\x1b[2K")
      content_idx = r_idx - top_margin
      if 0 <= content_idx < len(visible_body):
        out.append(visible_body[content_idx])
      out.append("\r\n")

    footer = " Help — press ?, h, Esc, Enter, or q to return to document "
    pad_len = max(0, self.term_cols - len(footer))
    out.append(
        f"\x1b[2K{sgr['bar_bg']}{sgr['bold_purple']}{footer}"
        f"{' ' * pad_len}{sgr['reset']}\x1b[?25l\x1b[?2026l"
    )
    return "".join(out)

  def render_frame(self) -> str:
    """Renders a complete synchronized-update ANSI terminal screen frame.

    Never alters SGR foreground colors on `U+10EEEE` image rows so Kitty's
    encoded 24-bit `image_id` remains 100% intact.

    Returns:
      Complete ANSI frame string wrapped in `CSI ? 2026 h` ... `CSI ? 2026 l`.

    Usage Example:
      ansi_frame = ctrl.render_frame()
    """
    sgr = mdview_constants.DRACULA_SGR
    if self.mode == InputMode.HELP:
      return self._render_help_frame()

    lines = self.active_lines()
    vis = self.viewport_height()
    out: list[str] = ["\x1b[?2026h\x1b[H"]

    if self.needs_image_reupload and not self.raw_mode:
      out.extend(self.document.upload_escapes)
      self.needs_image_reupload = False

    prev_src = -1
    for screen_r in range(vis):
      doc_idx = self.state.scroll_top + screen_r
      out.append("\x1b[2K")
      if doc_idx >= len(lines):
        out.append(f"{sgr['comment']}    ~ │{sgr['reset']}\r\n")
        continue

      if self.raw_mode:
        src_line = doc_idx + 1
      elif doc_idx < len(self.document.source_lines):
        src_line = self.document.source_lines[doc_idx]
      else:
        src_line = 1

      is_caret = doc_idx == self.state.cursor_row
      if src_line == prev_src and not is_caret and not self.raw_mode:
        gutter = f" {sgr['comment']}   ·│{sgr['reset']} "
      else:
        gutter = self._format_gutter(doc_idx, src_line, is_caret)
      prev_src = src_line

      out.append(gutter)
      out.append(lines[doc_idx])
      out.append("\r\n")

    # Separator bar
    horiz = "─" * max(0, self.term_cols - 6)
    out.append(
        f"\x1b[2K{sgr['comment']}─────┴{horiz}{sgr['reset']}\r\n"
    )

    cur_src = self.current_source_line()
    total_src = max(1, len(self.document.raw_lines))
    num_cmts = len(self.state.comments)

    cur_row_cmts = self._comments_for_row(self.state.cursor_row, cur_src)
    cur_span = self._collapse_span_for_row(self.state.cursor_row)

    if self.mode == InputMode.DRAWER:
      sorted_cmts = sorted(self.state.comments.items())
      drawer_rows = min(8, max(3, len(sorted_cmts) + 1))
      out.append(
          f"\x1b[2K{sgr['bold_purple']} Comments ({num_cmts}) — "
          f"j/k:select  Enter:jump  e:edit  E:emacs  r:raw  x:del  Esc:close"
          f"{sgr['reset']}\r\n"
      )
      for d_idx in range(drawer_rows - 1):
        out.append("\x1b[2K")
        if d_idx < len(sorted_cmts):
          c_line, c_text = sorted_cmts[d_idx]
          flat = c_text.replace("\n", " ↵ ")
          marker = f"{sgr['cyan']}▸ " if d_idx == self.drawer_index else "  "
          snippet = flat[: max(10, self.term_cols - 14)]
          out.append(
              f"{marker}{sgr['yellow']}L{c_line:<4d}{sgr['fg']} {snippet}{sgr['reset']}"
          )
        out.append("\r\n")
    elif self.mode == InputMode.COMMENT:
      prefix = f" Comment (L{self.comment_target_line}) [Enter:save C-g:emacs Esc:cancel]: "
      avail = max(8, self.term_cols - len(prefix) - 1)
      start = max(0, self.input_cursor - avail + 2)
      visible_in = self.input_buffer[start : start + avail]
      out.append(
          f"\x1b[2K{sgr['prompt_bg']}{sgr['cyan']}{prefix}"
          f"{sgr['fg']}{visible_in}{sgr['reset']}\r\n"
      )
    elif self.mode == InputMode.SEARCH:
      prefix = " Search /: "
      visible_in = self.input_buffer[: max(8, self.term_cols - len(prefix) - 1)]
      out.append(
          f"\x1b[2K{sgr['prompt_bg']}{sgr['yellow']}{prefix}"
          f"{sgr['fg']}{visible_in}{sgr['reset']}\r\n"
      )
    elif len(cur_row_cmts) == 1:
      c_line, c_text = cur_row_cmts[0]
      flat = c_text.replace("\n", " ↵ ")
      action_hint = " [c:edit E/C-g:emacs x:del]"
      prefix = f" 💬 L{c_line}: "
      limit = max(12, self.term_cols - len(prefix) - len(action_hint) - 2)
      clipped = flat[:limit] + ("…" if len(flat) > limit else "")
      out.append(
          f"\x1b[2K{sgr['cyan']}{prefix}"
          f"{sgr['yellow']}{clipped}{sgr['comment']}{action_hint}"
          f"{sgr['reset']}\r\n"
      )
    elif len(cur_row_cmts) > 1:
      span_hi = (
          cur_span.end_source_line
          if cur_span and cur_span.end_source_line >= cur_span.source_line
          else cur_row_cmts[-1][0]
      )
      span_lo = cur_span.source_line if cur_span else cur_row_cmts[0][0]
      joined = " | ".join(
          f"L{ln}: {txt.replace(chr(10), ' ↵ ')}" for ln, txt in cur_row_cmts
      )
      header = f" 💬 L{span_lo}–{span_hi} ({len(cur_row_cmts)} cmts, C:edit): "
      limit = max(12, self.term_cols - len(header) - 2)
      clipped = joined[:limit] + ("…" if len(joined) > limit else "")
      out.append(
          f"\x1b[2K{sgr['cyan']}{header}"
          f"{sgr['yellow']}{clipped}{sgr['reset']}\r\n"
      )
    elif self.flash_message:
      out.append(
          f"\x1b[2K{sgr['orange']} {self.flash_message[:self.term_cols - 2]}"
          f"{sgr['reset']}\r\n"
      )

    # Bottom status bar (no trailing \r\n so last row never scrolls screen!)
    cmt_label = f"{num_cmts} cmt" if num_cmts == 1 else f"{num_cmts} cmts"
    left_stat = f" {self.state.filename} · L{cur_src}/{total_src} ({cmt_label}) "
    hints = (
        "j/k:move c:cmt E/C-g:emacs x:del C:list +/-:zoom r:raw o:open "
        "?:help A:approve q:done"
    )
    max_hints = max(0, self.term_cols - len(left_stat) - 1)
    status_line = f"{sgr['bar_bg']}{sgr['bold_purple']}{left_stat}{sgr['comment']}{hints[:max_hints]}"
    pad_len = max(0, self.term_cols - len(left_stat) - len(hints[:max_hints]))
    out.append(f"\x1b[2K{status_line}{' ' * pad_len}{sgr['reset']}")

    if self.mode in (InputMode.COMMENT, InputMode.SEARCH):
      prefix_len = (
          len(f" Comment (L{self.comment_target_line}) [Enter:save C-g:emacs Esc:cancel]: ")
          if self.mode == InputMode.COMMENT
          else len(" Search /: ")
      )
      avail = max(8, self.term_cols - prefix_len - 1)
      start = max(0, self.input_cursor - avail + 2)
      col_one_based = min(self.term_cols, prefix_len + (self.input_cursor - start) + 1)
      prompt_row_one_based = self.term_rows - 1
      out.append(f"\x1b[{prompt_row_one_based};{col_one_based}H\x1b[?25h")
    else:
      out.append("\x1b[?25l")

    out.append("\x1b[?2026l")
    return "".join(out)


def read_key_token(tty_fd: int) -> str:
  """Reads and decodes a single keystroke or mouse event from raw `tty_fd`.

  Args:
    tty_fd: Open file descriptor for `/dev/tty` in raw mode.

  Returns:
    Normalized key string (`"j"`, `"UP"`, `"ESC"`, `"MOUSE:..."`, etc.).

  Usage Example:
    token = read_key_token(fd)
  """
  first = os.read(tty_fd, 1)
  if not first:
    return "q"

  if first != b"\x1b":
    # Decode multibyte UTF-8 scalar if high bit is set.
    b0 = first[0]
    extra = 0
    if (b0 & 0xE0) == 0xC0:
      extra = 1
    elif (b0 & 0xF0) == 0xE0:
      extra = 2
    elif (b0 & 0xF8) == 0xF0:
      extra = 3
    if extra > 0:
      first += os.read(tty_fd, extra)
    return first.decode("utf-8", errors="replace")

  ready, _, _ = select.select([tty_fd], [], [], 0.03)
  if not ready:
    return "ESC"

  buf = bytearray(first)
  while len(buf) < 32:
    chunk = os.read(tty_fd, 1)
    if not chunk:
      break
    buf.extend(chunk)
    byte_val = chunk[0]
    if len(buf) == 2 and byte_val not in (ord("["), ord("O")):
      break
    if len(buf) >= 3 and 0x40 <= byte_val <= 0x7E:
      break
    more, _, _ = select.select([tty_fd], [], [], 0.005)
    if not more:
      break

  seq = bytes(buf).decode("latin1")
  if m_mouse := _SGR_MOUSE_RE.match(seq):
    btn, col, row, kind = m_mouse.groups()
    return f"MOUSE:{btn}:{col}:{row}:{kind}"

  if m_csiu := _CSI_U_RE.match(seq):
    codepoint = int(m_csiu.group(1))
    modifier = int(m_csiu.group(2) or "1")
    if codepoint == 27:
      return "ESC"
    if codepoint == 13:
      return "\r"
    if modifier == 5 and 97 <= codepoint <= 122:
      return chr(codepoint - 96)  # Ctrl+letter -> control byte (\x07 for Ctrl+G)

  csi_map = {
      "\x1b[A": "UP",
      "\x1bOA": "UP",
      "\x1b[B": "DOWN",
      "\x1bOB": "DOWN",
      "\x1b[C": "RIGHT",
      "\x1bOC": "RIGHT",
      "\x1b[D": "LEFT",
      "\x1bOD": "LEFT",
      "\x1b[5~": "PGUP",
      "\x1b[6~": "PGDN",
      "\x1b[H": "HOME",
      "\x1bOH": "HOME",
      "\x1b[1~": "HOME",
      "\x1b[F": "END",
      "\x1bOF": "END",
      "\x1b[4~": "END",
      "\x1b[3~": "DELETE",
      "\x1b[Z": "SHIFT_TAB",
      "\x1bv": "ALT_V",
      "\x1b<": "ALT_LT",
      "\x1b>": "ALT_GT",
  }
  return csi_map.get(seq, "ESC" if seq == "\x1b" else seq)


def run_interactive_tui(
    filepath: str,
    diagram_mode: mdview_constants.DiagramMode = mdview_constants.DiagramMode.AUTO,
    tmux_passthrough: bool = False,
    cache_dir: str | None = None,
) -> ExitOutcome:
  """Runs the interactive alternate-screen Markdown viewer and commenter on `/dev/tty`.

  Args:
    filepath: Path to the Markdown file on disk.
    diagram_mode: Diagram rendering backend (`auto`, `kitty`, `unicode`, `raw`).
    tmux_passthrough: Whether Kitty uploads require `tmux` DCS wrapping.
    cache_dir: Optional directory path for cached Mermaid PNGs.

  Returns:
    `ExitOutcome` describing approval status, formatted Jetski comments, and
    whether the user requested switching files via `--pick`.

  Usage Example:
    outcome = run_interactive_tui("/path/to/plan.md")
  """
  tty_fd = os.open("/dev/tty", os.O_RDWR)
  orig_termios = termios.tcgetattr(tty_fd)
  resize_pending = False

  def _on_sigwinch(signum: int, frame: object) -> None:
    del signum, frame
    nonlocal resize_pending
    resize_pending = True

  def _load_and_render(
      prose_cols: int, diagram_cols: int, diagram_zoom: float
  ) -> mdview.RenderedDocument:
    with open(filepath, encoding="utf-8") as f:
      raw_md = f.read()
    return mdview.render_document(
        markdown_text=raw_md,
        columns=prose_cols,
        diagram_mode=diagram_mode,
        tmux_passthrough=tmux_passthrough,
        cache_dir=cache_dir,
        diagram_columns=diagram_cols,
        diagram_zoom=diagram_zoom,
    )

  def _enter_raw() -> None:
    tty.setraw(tty_fd)
    write_all_fd(
        tty_fd,
        b"\x1b[?1049h\x1b[?25l\x1b[?1000h\x1b[?1006h",
    )

  def _leave_raw() -> None:
    write_all_fd(
        tty_fd,
        b"\x1b[?1006l\x1b[?1000l\x1b[?25h\x1b[?1049l",
    )
    termios.tcsetattr(tty_fd, termios.TCSADRAIN, orig_termios)

  def _suspended_editor(initial_text: str, line_no: int) -> str | None:
    _leave_raw()
    try:
      return _default_external_editor(initial_text, line_no)
    finally:
      _enter_raw()

  prev_handler = signal.signal(signal.SIGWINCH, _on_sigwinch)
  wakeup_r, wakeup_w = os.pipe()
  os.set_blocking(wakeup_r, False)
  os.set_blocking(wakeup_w, False)
  prev_wakeup_fd = signal.set_wakeup_fd(wakeup_w, warn_on_full_buffer=False)
  try:
    geom = mdview_kitty.query_terminal_geometry()
    avail_cols = max(
        mdview_constants.MIN_COLUMNS, geom.cols - mdview_constants.GUTTER_WIDTH
    )
    prose_cols = min(mdview_constants.DEFAULT_MAX_COLUMNS, avail_cols)
    diagram_cols = max(mdview_constants.MIN_COLUMNS, avail_cols - 2)
    initial_doc = _load_and_render(prose_cols, diagram_cols, 1.0)
    ctrl = ViewerController(
        filepath=filepath,
        document=initial_doc,
        term_cols=geom.cols,
        term_rows=geom.rows,
        external_editor_fn=_suspended_editor,
        reload_fn=_load_and_render,
    )
    _enter_raw()
    try:
      while True:
        if resize_pending:
          resize_pending = False
          new_geom = mdview_kitty.query_terminal_geometry()
          ctrl.resize(new_geom.cols, new_geom.rows)
        # Transmit chunked Kitty image uploads BEFORE entering synchronized
        # update (`?2026h`) using write-all so SSH PTY buffers never truncate.
        for upload_seq in ctrl.consume_pending_image_uploads():
          write_all_fd(tty_fd, upload_seq.encode("utf-8"))
        frame_bytes = ctrl.render_frame().encode("utf-8")
        write_all_fd(tty_fd, frame_bytes)
        try:
          readable, _, _ = select.select([tty_fd, wakeup_r], [], [])
        except InterruptedError:
          continue
        if wakeup_r in readable:
          try:
            os.read(wakeup_r, 1024)
          except BlockingIOError:
            pass
          if tty_fd not in readable:
            continue
        try:
          token = read_key_token(tty_fd)
        except InterruptedError:
          continue
        outcome = ctrl.handle_key(token)
        if outcome is not None:
          return outcome
    finally:
      _leave_raw()
  finally:
    signal.set_wakeup_fd(prev_wakeup_fd)
    signal.signal(signal.SIGWINCH, prev_handler)
    os.close(wakeup_r)
    os.close(wakeup_w)
    os.close(tty_fd)
