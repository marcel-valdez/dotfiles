"""Persistent review draft storage, Jetski formatting, and clipboard export.

Persists per-document review comments and viewport scroll positions across
sessions under `~/.local/share/mdview/reviews/`, formats feedback identically
to Jetski CLI (`artifact_detail.go` & `artifactreview/format.go`), and exports
to both OSC-52 local Kitty clipboard and `async_clipboard_copy.sh`.

Usage Example:
  state, changed = load_review_state("/path/to/plan.md")
  state.comments[34] = "Clarify fallback behavior."
  save_review_state(state)
  text = format_jetski_feedback(state.filename, state.comments)
"""

from __future__ import annotations

import base64
from collections.abc import Callable, Mapping
import dataclasses
import enum
import hashlib
import json
import logging
import os
import subprocess
import tempfile
import time

import mdview_constants


class ReviewStatus(enum.StrEnum):
  """Submission decision recorded when exiting a Markdown review."""

  COMMENT_ONLY = "comment_only"
  APPROVED = "approved"
  REJECTED = "rejected"


@dataclasses.dataclass
class ReviewState:
  """Mutable in-memory and persisted review draft state for one document.

  Attributes:
    realpath: Canonical resolved filesystem path of the Markdown document.
    filename: Basename of the document used in Jetski `[filename.md]` headers.
    file_sha256: SHA-256 hex digest of file contents when comments were saved.
    cursor_row: Zero-based rendered viewport row index of the caret.
    scroll_top: Zero-based top visible viewport row index.
    comments: Mapping from 1-based raw source line number to comment string.
    updated_at: Unix epoch timestamp of last state save.
  """

  realpath: str
  filename: str
  file_sha256: str
  cursor_row: int = 0
  scroll_top: int = 0
  comments: dict[int, str] = dataclasses.field(default_factory=dict)
  updated_at: float = 0.0


def default_reviews_dir() -> str:
  """Returns the default `~/.local/share/mdview/reviews` directory path.

  Returns:
    Absolute path to the review drafts directory.

  Usage Example:
    reviews_dir = default_reviews_dir()
  """
  return os.path.join(os.path.expanduser("~"), mdview_constants.REVIEWS_REL_DIR)


def default_last_review_path() -> str:
  """Returns `~/.local/share/mdview/last_review.txt` for backup inspection.

  Returns:
    Absolute path to the last exported review text file.

  Usage Example:
    backup = default_last_review_path()
  """
  return os.path.join(
      os.path.expanduser("~"), mdview_constants.LAST_REVIEW_REL_PATH
  )


def compute_file_sha256(filepath: str) -> str:
  """Computes the SHA-256 hex digest of `filepath` if readable, else `""`.

  Args:
    filepath: Path to file on disk.

  Returns:
    Hexadecimal SHA-256 string or empty string when missing/unreadable.

  Usage Example:
    digest = compute_file_sha256("/tmp/plan.md")
  """
  try:
    hasher = hashlib.sha256()
    with open(filepath, "rb") as f:
      while chunk := f.read(65536):
        hasher.update(chunk)
    return hasher.hexdigest()
  except OSError:
    return ""


def _state_json_path(realpath: str, reviews_dir: str) -> str:
  """Computes the JSON state file path keyed by SHA-256 of `realpath`."""
  key = hashlib.sha256(realpath.encode("utf-8")).hexdigest()
  return os.path.join(reviews_dir, f"{key}.json")


def load_review_state(
    filepath: str,
    reviews_dir: str | None = None,
) -> tuple[ReviewState, bool]:
  """Loads persisted `ReviewState` for `filepath` and checks for file edits.

  Args:
    filepath: Target Markdown file path.
    reviews_dir: Optional directory override for hermetic testing.

  Returns:
    Tuple `(state, file_changed_since_draft)` where `file_changed_since_draft`
    is `True` only when non-empty draft comments exist from an earlier file
    hash.

  Usage Example:
    state, mutated = load_review_state("~/notes/design.md")
  """
  realpath = os.path.realpath(os.path.expanduser(filepath))
  filename = os.path.basename(realpath) or "stdin.md"
  current_hash = compute_file_sha256(realpath)
  resolved_dir = reviews_dir or default_reviews_dir()
  json_path = _state_json_path(realpath, resolved_dir)

  if not os.path.isfile(json_path):
    return (
        ReviewState(
            realpath=realpath,
            filename=filename,
            file_sha256=current_hash,
        ),
        False,
    )

  try:
    with open(json_path, encoding="utf-8") as f:
      raw = json.load(f)
    raw_comments = raw.get("comments", {})
    comments = {
        int(k): str(v)
        for k, v in raw_comments.items()
        if str(k).isdigit() and str(v).strip()
    }
    saved_hash = str(raw.get("file_sha256", ""))
    changed = bool(
        comments and saved_hash and current_hash and saved_hash != current_hash
    )
    return (
        ReviewState(
            realpath=realpath,
            filename=filename,
            file_sha256=current_hash or saved_hash,
            cursor_row=max(0, int(raw.get("cursor_row", 0))),
            scroll_top=max(0, int(raw.get("scroll_top", 0))),
            comments=comments,
            updated_at=float(raw.get("updated_at", 0.0)),
        ),
        changed,
    )
  except (OSError, ValueError, TypeError, json.JSONDecodeError) as exc:
    logging.warning("Ignoring unreadable review draft %s: %s", json_path, exc)
    return (
        ReviewState(
            realpath=realpath,
            filename=filename,
            file_sha256=current_hash,
        ),
        False,
    )


def save_review_state(
    state: ReviewState,
    reviews_dir: str | None = None,
) -> str:
  """Atomically persists `state` to disk via write-and-rename.

  Args:
    state: `ReviewState` to persist.
    reviews_dir: Optional directory override for hermetic testing.

  Returns:
    Absolute path of the written JSON state file.

  Usage Example:
    path = save_review_state(state)
  """
  resolved_dir = reviews_dir or default_reviews_dir()
  os.makedirs(resolved_dir, exist_ok=True)
  state.updated_at = time.time()
  if current_hash := compute_file_sha256(state.realpath):
    state.file_sha256 = current_hash

  payload = {
      "realpath": state.realpath,
      "filename": state.filename,
      "file_sha256": state.file_sha256,
      "cursor_row": state.cursor_row,
      "scroll_top": state.scroll_top,
      "comments": {
          str(line): text
          for line, text in sorted(state.comments.items())
          if text.strip()
      },
      "updated_at": state.updated_at,
  }
  target_path = _state_json_path(state.realpath, resolved_dir)
  with tempfile.NamedTemporaryFile(
      mode="w",
      encoding="utf-8",
      dir=resolved_dir,
      prefix=".mdview_draft_",
      suffix=".tmp",
      delete=False,
  ) as tmp_f:
    json.dump(payload, tmp_f, indent=2)
    tmp_path = tmp_f.name
  os.replace(tmp_path, target_path)
  return target_path


def clear_review_state(
    filepath: str,
    reviews_dir: str | None = None,
) -> None:
  """Removes the persisted draft state file for `filepath` if present.

  Args:
    filepath: Target Markdown file path.
    reviews_dir: Optional directory override for hermetic testing.

  Usage Example:
    clear_review_state("/path/to/plan.md")
  """
  realpath = os.path.realpath(os.path.expanduser(filepath))
  resolved_dir = reviews_dir or default_reviews_dir()
  target_path = _state_json_path(realpath, resolved_dir)
  try:
    os.remove(target_path)
  except FileNotFoundError:
    pass


def list_draft_reviews(
    reviews_dir: str | None = None,
) -> tuple[ReviewState, ...]:
  """Lists existing files that currently hold at least one draft comment.

  Args:
    reviews_dir: Optional directory override for hermetic testing.

  Returns:
    Tuple of `ReviewState` objects sorted most-recently-updated first.

  Usage Example:
    drafts = list_draft_reviews()
  """
  resolved_dir = reviews_dir or default_reviews_dir()
  if not os.path.isdir(resolved_dir):
    return ()

  found: list[ReviewState] = []
  for entry in os.listdir(resolved_dir):
    if not entry.endswith(".json"):
      continue
    full_json = os.path.join(resolved_dir, entry)
    try:
      with open(full_json, encoding="utf-8") as f:
        raw = json.load(f)
      realpath = str(raw.get("realpath", ""))
      if not realpath or not os.path.isfile(realpath):
        continue
      raw_comments = raw.get("comments", {})
      comments = {
          int(k): str(v)
          for k, v in raw_comments.items()
          if str(k).isdigit() and str(v).strip()
      }
      if not comments:
        continue
      found.append(
          ReviewState(
              realpath=realpath,
              filename=str(raw.get("filename", os.path.basename(realpath))),
              file_sha256=str(raw.get("file_sha256", "")),
              cursor_row=int(raw.get("cursor_row", 0)),
              scroll_top=int(raw.get("scroll_top", 0)),
              comments=comments,
              updated_at=float(raw.get("updated_at", 0.0)),
          )
      )
    except (OSError, ValueError, TypeError, json.JSONDecodeError):
      continue

  found.sort(key=lambda item: item.updated_at, reverse=True)
  return tuple(found)


def format_jetski_feedback(
    filename: str,
    comments: Mapping[int, str],
    decision: ReviewStatus = ReviewStatus.COMMENT_ONLY,
) -> str:
  """Formats decisions and line comments identically to Jetski CLI.

  Produces `[<basename>]` blocks with `L<1-based-line>: <comment>` lines and
  `[Approved] <basename>` approval headers.

  Args:
    filename: Basename of the reviewed Markdown file.
    comments: Mapping of 1-based raw source line numbers to comment strings.
    decision: Approval decision (`comment_only`, `approved`, or `rejected`).

  Returns:
    Formatted review feedback string ready to paste into Jetski.

  Usage Example:
    out = format_jetski_feedback("plan.md", {34: "Use vsplit"}, ReviewStatus.COMMENT_ONLY)
  """
  parts: list[str] = []
  if decision == ReviewStatus.APPROVED:
    parts.append(f"[Approved] {filename}")
  elif decision == ReviewStatus.REJECTED:
    parts.append(f"[Rejected] {filename}")

  cleaned = {
      line: text.strip()
      for line, text in comments.items()
      if text.strip()
  }
  if cleaned:
    comment_lines = [f"[{filename}]"]
    for line_num in sorted(cleaned):
      comment_lines.append(f"L{line_num}: {cleaned[line_num]}")
    block = "\n".join(comment_lines) + "\n"
    if parts:
      return parts[0] + "\n\n" + block
    return block

  return parts[0] if parts else ""


def build_osc52_sequence(payload: str, in_tmux: bool = False) -> bytes:
  """Encodes `payload` as an OSC-52 clipboard write escape sequence.

  Args:
    payload: UTF-8 text to place on the system clipboard.
    in_tmux: Whether to wrap the OSC-52 sequence in `tmux` DCS passthrough.

  Returns:
    Raw ASCII escape sequence bytes.

  Usage Example:
    seq = build_osc52_sequence("[Approved] plan.md")
  """
  encoded = base64.b64encode(payload.encode("utf-8")).decode("ascii")
  osc = f"\x1b]52;c;{encoded}\x07"
  if in_tmux:
    osc = f"\x1bPtmux;\x1b{osc}\x1b\\"
  return osc.encode("ascii")


def export_feedback_to_clipboards(
    payload: str,
    tty_writer: Callable[[bytes], None] | None = None,
    last_review_path: str | None = None,
    in_tmux: bool | None = None,
    run_async_helper: bool = True,
) -> bool:
  """Exports `payload` to OSC-52, `last_review.txt`, and `async_clipboard_copy`.

  Args:
    payload: Formatted Jetski feedback text.
    tty_writer: Optional callable writing raw OSC-52 bytes to `/dev/tty`.
    last_review_path: Optional override path for `last_review.txt`.
    in_tmux: Optional override indicating whether `$TMUX` is active.
    run_async_helper: Whether to invoke `~/.googlerc.d/scripts/async_clipboard_copy.sh`
      when installed.

  Returns:
    `True` if `payload` was non-empty and exported, `False` otherwise.

  Usage Example:
    export_feedback_to_clipboards("[Approved] plan.md")
  """
  if not payload:
    return False

  target_last = last_review_path or default_last_review_path()
  try:
    os.makedirs(os.path.dirname(target_last), exist_ok=True)
    with open(target_last, "w", encoding="utf-8") as f:
      f.write(payload)
  except OSError as exc:
    logging.warning("Could not write %s: %s", target_last, exc)

  tmux_active = bool(os.environ.get("TMUX")) if in_tmux is None else in_tmux
  osc52_bytes = build_osc52_sequence(payload, in_tmux=tmux_active)

  if tty_writer is not None:
    tty_writer(osc52_bytes)
  else:
    try:
      fd = os.open("/dev/tty", os.O_WRONLY)
      try:
        os.write(fd, osc52_bytes)
      finally:
        os.close(fd)
    except OSError:
      pass

  if run_async_helper:
    helper = os.path.expanduser(
        "~/.googlerc.d/scripts/async_clipboard_copy.sh"
    )
    if os.access(helper, os.X_OK):
      try:
        subprocess.run(
            [helper, "--sync", "--notify-tmux"],
            input=payload,
            text=True,
            capture_output=True,
            check=False,
            timeout=3,
        )
      except (OSError, subprocess.SubprocessError):
        pass

  return True
