"""Zero-typing Markdown target queue (`mdq`) and 4-tier smart `fzf` picker.

Resolves target Markdown documents on Cloudtop without manual path typing:
1. Instant 60-second TTL queue (`mdview --queue <file>` / `mdq <file>`).
2. Ranked `fzf` selector combining:
   - Tier 1 (`[DRAFT]`): Documents with saved review comments.
   - Tier 2 (`[CLIPBOARD]`): Valid `.md` path in the active paste buffer.
   - Tier 3 (`[JETSKI]`): Recent plans & scratch docs in `~/.gemini/jetski/brain`.
   - Tier 4 (`[WORKSPACE]` / `[NOTES]`): Active workspace & `~/notes` `.md` docs
     (using `hg status` inside CitC so `/google/src` is never walked).

Usage Example:
  candidates = collect_markdown_candidates()
  chosen = run_fzf_picker(candidates)
"""

from __future__ import annotations

from collections.abc import Sequence
import dataclasses
import json
import os
import shutil
import subprocess
import tempfile
import time
from urllib import parse as urllib_parse

import mdview
import mdview_constants
import mdview_review_store


@dataclasses.dataclass(frozen=True)
class CandidateEntry:
  """Single ranked Markdown file candidate for the interactive `fzf` picker.

  Attributes:
    realpath: Canonical resolved path to the `.md` file on disk.
    category: Source tier label (`DRAFT`, `CLIPBOARD`, `JETSKI`, `WORKSPACE`,
      or `NOTES`).
    label: Formatted single-line display string shown in `fzf`.
    sort_score: Descending numeric rank score.
  """

  realpath: str
  category: str
  label: str
  sort_score: float


def default_queue_path() -> str:
  """Returns `~/.cache/mdview/queued_target.json` for `mdq` handoffs.

  Returns:
    Absolute path to the queued target JSON file.

  Usage Example:
    qpath = default_queue_path()
  """
  return os.path.join(os.path.expanduser("~"), mdview_constants.QUEUE_REL_PATH)


def queue_target_file(
    filepath: str,
    queue_path: str | None = None,
    now: float | None = None,
) -> str:
  """Stages `filepath` for instant one-shot consumption by `mdview --pick`.

  Args:
    filepath: Path to an existing Markdown document.
    queue_path: Optional override path for the queue JSON file.
    now: Optional epoch timestamp override for deterministic tests.

  Returns:
    Canonical `realpath` of the queued Markdown file.

  Raises:
    FileNotFoundError: If `filepath` does not exist as a regular file.

  Usage Example:
    staged = queue_target_file("~/notes/architecture.md")
  """
  expanded = os.path.expanduser(filepath)
  if not os.path.isfile(expanded):
    raise FileNotFoundError(f"Markdown file not found: {filepath}")
  realpath = os.path.realpath(expanded)
  target_q = queue_path or default_queue_path()
  q_dir = os.path.dirname(target_q)
  os.makedirs(q_dir, exist_ok=True)
  ts = time.time() if now is None else now

  with tempfile.NamedTemporaryFile(
      mode="w",
      encoding="utf-8",
      dir=q_dir,
      prefix=".mdview_q_",
      suffix=".tmp",
      delete=False,
  ) as tmp_f:
    json.dump({"realpath": realpath, "queued_at": ts}, tmp_f)
    tmp_name = tmp_f.name
  os.replace(tmp_name, target_q)
  return realpath


def consume_queued_target(
    queue_path: str | None = None,
    ttl_seconds: float = mdview_constants.QUEUE_TTL_SECONDS,
    now: float | None = None,
) -> str | None:
  """Atomically consumes and deletes a fresh queued Markdown target if valid.

  Args:
    queue_path: Optional override path for the queue JSON file.
    ttl_seconds: Maximum age in seconds for a queued file to auto-open.
    now: Optional epoch timestamp override for deterministic tests.

  Returns:
    Canonical file path if present and within `ttl_seconds`, else `None`.

  Usage Example:
    if path := consume_queued_target():
      open_viewer(path)
  """
  target_q = queue_path or default_queue_path()
  if not os.path.isfile(target_q):
    return None
  try:
    with open(target_q, encoding="utf-8") as f:
      data = json.load(f)
  except (OSError, ValueError, json.JSONDecodeError):
    data = {}
  finally:
    try:
      os.remove(target_q)
    except OSError:
      pass

  realpath = str(data.get("realpath", ""))
  queued_at = float(data.get("queued_at", 0.0))
  ts_now = time.time() if now is None else now
  if not realpath or (ts_now - queued_at) > ttl_seconds:
    return None
  return realpath if os.path.isfile(realpath) else None


def is_citc_path(path: str) -> bool:
  """Returns `True` if `path` resides on a CitC / SrcFS FUSE mount.

  Args:
    path: Directory or file path string to inspect.

  Returns:
    `True` when `path` starts with `/google/src`, `/cloud/`, or `/google_src`.

  Usage Example:
    citc = is_citc_path("/google/src/cloud/user/ws/google3")  # True
  """
  normalized = os.path.abspath(os.path.expanduser(path))
  return normalized.startswith(
      ("/google/src/", "/cloud/", "/google_src/", "/files/", "/head/")
  )


def _shorten_display_path(path: str) -> str:
  """Shortens `$HOME` and Jetski brain UUIDs for compact `fzf` labels."""
  home = os.path.expanduser("~")
  short = f"~{path[len(home):]}" if path.startswith(home + "/") else path
  return short


def _parse_clipboard_md_path(raw_clip: str) -> str | None:
  """Extracts an existing `.md` path from single-line clipboard text or URI."""
  cleaned = raw_clip.strip().strip("'\"")
  if not cleaned or "\n" in cleaned or len(cleaned) > 1024:
    return None
  if cleaned.startswith("file://"):
    parsed = urllib_parse.urlparse(cleaned)
    cleaned = urllib_parse.unquote(parsed.path)
  # Strip optional #L10-L20 line anchors copied from Markdown links.
  if "#" in cleaned:
    cleaned = cleaned.split("#", 1)[0]
  expanded = os.path.expanduser(cleaned)
  if expanded.lower().endswith((".md", ".markdown")) and os.path.isfile(expanded):
    return os.path.realpath(expanded)
  return None


def _read_tmux_clipboard() -> str:
  """Reads the current `tmux` paste buffer when `tmux` server is reachable."""
  if not shutil.which("tmux"):
    return ""
  try:
    proc = subprocess.run(
        ["tmux", "show-buffer"],
        capture_output=True,
        text=True,
        check=False,
        timeout=1,
    )
    return proc.stdout if proc.returncode == 0 else ""
  except (OSError, subprocess.SubprocessError):
    return ""


def query_active_tmux_cwd() -> str:
  """Returns the normalized `pane_current_path` of the active `tmux` client.

  Returns:
    Directory path string, or `""` when no `tmux` client is active.

  Usage Example:
    cwd = query_active_tmux_cwd()
  """
  if not shutil.which("tmux"):
    return ""
  try:
    proc = subprocess.run(
        [
            "tmux",
            "list-clients",
            "-F",
            "#{client_activity}\t#{pane_current_path}",
        ],
        capture_output=True,
        text=True,
        check=False,
        timeout=1,
    )
    if proc.returncode != 0 or not proc.stdout.strip():
      return ""
    best_act = -1
    best_path = ""
    for line in proc.stdout.splitlines():
      parts = line.split("\t", 1)
      if len(parts) == 2 and parts[0].isdigit():
        act = int(parts[0])
        if act > best_act:
          best_act = act
          best_path = parts[1].strip()
    if best_path.startswith("/cloud/"):
      best_path = "/google/src" + best_path
    return best_path if os.path.isdir(best_path) else ""
  except (OSError, subprocess.SubprocessError):
    return ""


def _scan_bounded_md_files(
    root_dir: str,
    max_depth: int = 3,
    extra_excluded: frozenset[str] = frozenset(),
) -> list[tuple[float, str]]:
  """Walks `root_dir` up to `max_depth` collecting `(mtime, realpath)` pairs."""
  if not root_dir or not os.path.isdir(root_dir) or is_citc_path(root_dir):
    return []
  excluded = mdview_constants.SEARCH_EXCLUDED_DIRS | extra_excluded
  root_abs = os.path.abspath(root_dir)
  found: list[tuple[float, str]] = []

  for cur_dir, dirnames, filenames in os.walk(root_abs):
    rel = os.path.relpath(cur_dir, root_abs)
    depth = 0 if rel == "." else rel.count(os.sep) + 1
    if depth >= max_depth:
      dirnames[:] = []
    else:
      dirnames[:] = [
          d for d in dirnames if d not in excluded and not d.startswith(".")
      ]
    for fname in filenames:
      if not fname.lower().endswith((".md", ".markdown")):
        continue
      full = os.path.join(cur_dir, fname)
      try:
        st = os.stat(full)
        found.append((st.st_mtime, os.path.realpath(full)))
      except OSError:
        continue
  found.sort(key=lambda item: item[0], reverse=True)
  return found


def _scan_citc_modified_md_files(citc_cwd: str) -> list[tuple[float, str]]:
  """Lists modified/added `.md` files in a CitC workspace via `hg status`."""
  hg_bin = shutil.which("chg") or shutil.which("hg")
  if not hg_bin or not os.path.isdir(citc_cwd):
    return []
  env = dict(os.environ)
  env["HGPLAIN"] = "1"
  try:
    root_proc = subprocess.run(
        [hg_bin, "root"],
        cwd=citc_cwd,
        env=env,
        capture_output=True,
        text=True,
        check=False,
        timeout=2,
    )
    if root_proc.returncode != 0:
      return []
    repo_root = root_proc.stdout.strip()
    st_proc = subprocess.run(
        [hg_bin, "status", "-mardn"],
        cwd=repo_root,
        env=env,
        capture_output=True,
        text=True,
        check=False,
        timeout=3,
    )
    if st_proc.returncode != 0:
      return []
    results: list[tuple[float, str]] = []
    for rel_path in st_proc.stdout.splitlines():
      rel = rel_path.strip()
      if not rel.lower().endswith((".md", ".markdown")):
        continue
      full = os.path.join(repo_root, rel)
      try:
        mtime = os.path.getmtime(full)
        results.append((mtime, os.path.realpath(full)))
      except OSError:
        continue
    results.sort(key=lambda item: item[0], reverse=True)
    return results
  except (OSError, subprocess.SubprocessError):
    return []


def collect_markdown_candidates(
    reviews_dir: str | None = None,
    jetski_brain_root: str | None = None,
    clipboard_text: str | None = None,
    workspace_cwd: str | None = None,
    notes_dir: str | None = None,
) -> tuple[CandidateEntry, ...]:
  """Builds a deduplicated, 4-tier ranked tuple of Markdown file candidates.

  Args:
    reviews_dir: Optional override directory for saved review drafts.
    jetski_brain_root: Optional override for `~/.gemini/jetski/brain`.
    clipboard_text: Optional explicit clipboard string (queries `tmux` if `None`).
    workspace_cwd: Optional active workspace directory (queries `tmux` if `None`).
    notes_dir: Optional override for `~/notes`.

  Returns:
    Ordered tuple of `CandidateEntry` items ready for `fzf`.

  Usage Example:
    entries = collect_markdown_candidates()
  """
  seen: set[str] = set()
  entries: list[CandidateEntry] = []

  def _add(realpath: str, category: str, tag: str, score: float) -> None:
    if realpath in seen:
      return
    seen.add(realpath)
    base = os.path.basename(realpath)
    short = _shorten_display_path(realpath)
    entries.append(
        CandidateEntry(
            realpath=realpath,
            category=category,
            label=f"{tag:<20s}  {base}  ({short})",
            sort_score=score,
        )
    )

  # Tier 1: Saved draft reviews with unsubmitted comments.
  for idx, draft in enumerate(
      mdview_review_store.list_draft_reviews(reviews_dir=reviews_dir)
  ):
    cnt = len(draft.comments)
    unit = "cmt" if cnt == 1 else "cmts"
    _add(
        draft.realpath,
        "DRAFT",
        f"[DRAFT: {cnt} {unit}]",
        4_000_000_000.0 - idx,
    )

  # Tier 2: Clipboard .md file path.
  raw_clip = _read_tmux_clipboard() if clipboard_text is None else clipboard_text
  if clip_path := _parse_clipboard_md_path(raw_clip):
    _add(clip_path, "CLIPBOARD", "[CLIPBOARD]", 3_000_000_000.0)

  # Tier 3: Recent Jetski artifacts and scratch/plans/*.md files.
  brain_root = (
      os.path.expanduser("~/.gemini/jetski/brain")
      if jetski_brain_root is None
      else jetski_brain_root
  )
  jetski_hits = _scan_bounded_md_files(
      brain_root,
      max_depth=4,
      extra_excluded=frozenset({".system_generated", "media", "logs"}),
  )
  for mtime, rpath in jetski_hits[:30]:
    _add(rpath, "JETSKI", "[JETSKI]", 2_000_000_000.0 + mtime)

  # Tier 4: Active workspace (CitC-safe) and ~/notes.
  active_ws = (
      query_active_tmux_cwd() if workspace_cwd is None else workspace_cwd
  )
  home_real = os.path.realpath(os.path.expanduser("~"))
  ws_hits: list[tuple[float, str]] = []
  if active_ws:
    if is_citc_path(active_ws):
      ws_hits.extend(_scan_citc_modified_md_files(active_ws))
    elif os.path.realpath(active_ws) != home_real:
      ws_hits.extend(_scan_bounded_md_files(active_ws, max_depth=3)[:25])

  resolved_notes = (
      os.path.expanduser("~/notes") if notes_dir is None else notes_dir
  )
  notes_hits = _scan_bounded_md_files(resolved_notes, max_depth=3)[:25]

  combined_tier4 = [
      (mtime, rpath, "WORKSPACE", "[WORKSPACE]") for mtime, rpath in ws_hits
  ] + [(mtime, rpath, "NOTES", "[NOTES]") for mtime, rpath in notes_hits]
  combined_tier4.sort(key=lambda item: item[0], reverse=True)
  for mtime, rpath, cat, tag in combined_tier4[:40]:
    _add(rpath, cat, tag, 1_000_000_000.0 + mtime)

  return tuple(entries)


def run_fzf_picker(
    candidates: Sequence[CandidateEntry],
    fzf_bin: str | None = None,
) -> str | None:
  """Runs interactive `fzf` over `candidates` with live `mdcat` Dracula preview.

  Args:
    candidates: Ranked sequence of `CandidateEntry` items.
    fzf_bin: Optional explicit path to `fzf` binary.

  Returns:
    Selected canonical file path, or `None` if cancelled (`Esc` / `Ctrl+C`).

  Usage Example:
    selected = run_fzf_picker(collect_markdown_candidates())
  """
  if not candidates:
    return None
  resolved_fzf = fzf_bin or shutil.which("fzf")
  if not resolved_fzf:
    fallback = os.path.expanduser("~/.fzf/bin/fzf")
    resolved_fzf = fallback if os.access(fallback, os.X_OK) else None
  if not resolved_fzf:
    return candidates[0].realpath

  mdcat_bin = mdview.find_mdcat_binary()
  if mdcat_bin:
    preview_cmd = (
        f"BAT_THEME=Dracula {mdcat_bin} --no-pager --ansi "
        "--theme=dracula --columns=80 {3}"
    )
  else:
    preview_cmd = "head -n 120 {3}"

  lines_in = "\n".join(
      f"{idx}\t{entry.label}\t{entry.realpath}"
      for idx, entry in enumerate(candidates)
  )

  with open("/dev/tty", "rb+", buffering=0) as tty_io:
    proc = subprocess.run(
        [
            resolved_fzf,
            "--ansi",
            "--no-sort",
            "--layout=reverse",
            "--delimiter=\t",
            "--with-nth=2",
            "--header=Enter: open in mdview  |  Esc: cancel",
            f"--preview={preview_cmd}",
            "--preview-window=right,55%,border-left",
        ],
        input=lines_in.encode("utf-8"),
        stdout=subprocess.PIPE,
        stderr=tty_io,
        check=False,
    )
  if proc.returncode != 0:
    return None
  chosen_line = proc.stdout.decode("utf-8", errors="replace").strip()
  parts = chosen_line.split("\t")
  return parts[2] if len(parts) >= 3 and os.path.isfile(parts[2]) else None
