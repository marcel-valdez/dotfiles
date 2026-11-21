"""Batched Mermaid HiDPI PNG caching and Kitty U+10EEEE virtual placements.

Compiles fenced Mermaid blocks to Dracula-themed 2x HiDPI PNG images cached by
SHA-256 in `~/.cache/mdview/` and converts them via `kitten icat` into virtual
placements (`U=1`) + `U+10EEEE` Unicode placeholder character rows.

Usage Example:
  png_by_idx = render_mermaid_blocks_to_png(("flowchart TD\\nA-->B",), "/tmp/c")
  placement = build_kitty_placement(png_by_idx[0], max_cols=100)
"""

from __future__ import annotations

from collections.abc import Sequence
import dataclasses
import fcntl
import hashlib
import json
import logging
import os
import re
import shutil
import struct
import subprocess
import tempfile
import termios
from typing import Mapping
import zlib

import mdview_constants


_UPLOAD_PREFIX_RE = re.compile(
    r"^\r?((?:\x1bPtmux;.*?\x1b\\|\x1b_G.*?\x1b\\)+)",
    re.DOTALL,
)


class MermaidRenderError(RuntimeError):
  """Raised when Mermaid-to-PNG or Kitty placement generation fails."""


@dataclasses.dataclass(frozen=True)
class TerminalGeometry:
  """Terminal window dimensions in character cells and pixels.

  Attributes:
    cols: Width in character cells.
    rows: Height in character cells.
    width_px: Width in physical pixels.
    height_px: Height in physical pixels.
  """

  cols: int
  rows: int
  width_px: int
  height_px: int


@dataclasses.dataclass(frozen=True)
class KittyPlacement:
  """Kitty virtual image upload sequence paired with `U+10EEEE` rows.

  Attributes:
    image_id: Positive 24-bit Kitty graphics protocol image identifier.
    upload_escape: Escape string (`a=T,U=1,...`) to write directly to the TTY.
    grid_lines: Tuple of SGR-colored `U+10EEEE` rows to splice into viewport.
    png_path: Absolute path of the cached PNG image on disk.
  """

  image_id: int
  upload_escape: str
  grid_lines: tuple[str, ...]
  png_path: str


def default_cache_dir() -> str:
  """Returns the user's default `~/.cache/mdview` directory path.

  Returns:
    Absolute path string to the mdview image cache directory.

  Usage Example:
    cache_dir = default_cache_dir()
  """
  return os.path.join(os.path.expanduser("~"), mdview_constants.CACHE_REL_DIR)


def cache_png_path(cache_dir: str, mermaid_code: str) -> str:
  """Computes the deterministic SHA-256 cached PNG path for `mermaid_code`.

  Args:
    cache_dir: Directory storing cached diagram PNGs.
    mermaid_code: Raw Mermaid diagram source string.

  Returns:
    Absolute path to `<sha256>.png` inside `cache_dir`.

  Usage Example:
    png = cache_png_path("/tmp/mdview_cache", "flowchart TD\\nA-->B")
  """
  normalized = mermaid_code.strip()
  payload = f"{mdview_constants.MERMAID_RENDER_VERSION}\0{normalized}".encode(
      "utf-8"
  )
  digest = hashlib.sha256(payload).hexdigest()
  return os.path.join(cache_dir, f"{digest}.png")


def query_terminal_geometry(
    fallback_cols: int = mdview_constants.DEFAULT_MAX_COLUMNS,
    fallback_rows: int = mdview_constants.DEFAULT_VIEWPORT_ROWS,
) -> TerminalGeometry:
  """Queries TTY cell and pixel dimensions via `TIOCGWINSZ` when available.

  Args:
    fallback_cols: Fallback column width when stdin/stdout are non-TTYs.
    fallback_rows: Fallback row height when stdin/stdout are non-TTYs.

  Returns:
    `TerminalGeometry` with positive cell and pixel dimensions.

  Usage Example:
    geom = query_terminal_geometry(fallback_cols=100)
  """
  for fd in (1, 0, 2):
    try:
      packed = fcntl.ioctl(fd, termios.TIOCGWINSZ, b"\x00" * 8)
      ws_row, ws_col, ws_xpixel, ws_ypixel = struct.unpack("HHHH", packed)
      if ws_col > 0 and ws_row > 0:
        px_w = (
            ws_xpixel
            if ws_xpixel > 0
            else ws_col * mdview_constants.DEFAULT_CELL_PX_WIDTH
        )
        px_h = (
            ws_ypixel
            if ws_ypixel > 0
            else ws_row * mdview_constants.DEFAULT_CELL_PX_HEIGHT
        )
        return TerminalGeometry(
            cols=ws_col, rows=ws_row, width_px=px_w, height_px=px_h
        )
    except OSError:
      continue

  return TerminalGeometry(
      cols=fallback_cols,
      rows=fallback_rows,
      width_px=fallback_cols * mdview_constants.DEFAULT_CELL_PX_WIDTH,
      height_px=fallback_rows * mdview_constants.DEFAULT_CELL_PX_HEIGHT,
  )


def find_chrome_binary() -> str | None:
  """Locates Google Chrome or Chromium across Linux and macOS installations.

  Returns:
    Executable path string if found, or `None`.

  Usage Example:
    chrome = find_chrome_binary()
  """
  if env_bin := os.environ.get("MDVIEW_CHROME_BIN"):
    if os.path.isfile(env_bin) and os.access(env_bin, os.X_OK):
      return env_bin
  for candidate in ("google-chrome", "chromium", "chromium-browser"):
    if found := shutil.which(candidate):
      return found
  macos_chrome = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
  return macos_chrome if os.access(macos_chrome, os.X_OK) else None


def find_node_binary() -> str | None:
  """Locates a Node.js executable from PATH or `~/.nvm` installations.

  Returns:
    Executable path string if found, or `None`.

  Usage Example:
    node_bin = find_node_binary()
  """
  if env_node := os.environ.get("MDVIEW_NODE_BIN"):
    if os.access(env_node, os.X_OK):
      return env_node
  if found := shutil.which("node"):
    return found
  node_exec_file = os.path.expanduser("~/.node_exec")
  if os.path.isfile(node_exec_file):
    with open(node_exec_file, encoding="utf-8") as f:
      recorded = f.read().strip()
    if recorded and os.access(recorded, os.X_OK):
      return recorded
  nvm_versions = os.path.expanduser("~/.nvm/versions/node")
  if os.path.isdir(nvm_versions):
    versions = sorted(os.listdir(nvm_versions), reverse=True)
    for ver in versions:
      candidate = os.path.join(nvm_versions, ver, "bin", "node")
      if os.access(candidate, os.X_OK):
        return candidate
  return None


def find_kitten_binary() -> str | None:
  """Locates Kitty's `kitten` binary across Linux and macOS installations.

  Returns:
    Executable path string if found, or `None`.

  Usage Example:
    kitten = find_kitten_binary()
  """
  if found := shutil.which("kitten"):
    return found
  for candidate in (
      "/usr/bin/kitten",
      "/Applications/kitty.app/Contents/MacOS/kitten",
  ):
    if os.access(candidate, os.X_OK):
      return candidate
  return None


def _process_ancestry_names(
    start_pid: int,
    max_hops: int = 10,
) -> tuple[str, ...]:
  """Walks parent PIDs from `start_pid` returning process command strings."""
  names: list[str] = []
  pid = start_pid
  for _ in range(max_hops):
    if pid <= 1:
      break
    proc_stat = f"/proc/{pid}/stat"
    if os.path.isfile(proc_stat):
      try:
        with open(proc_stat, encoding="utf-8", errors="replace") as f:
          stat_txt = f.read()
        rparen = stat_txt.rfind(")")
        lparen = stat_txt.find("(")
        comm = stat_txt[lparen + 1 : rparen] if lparen >= 0 and rparen > lparen else ""
        rest_fields = stat_txt[rparen + 2 :].split()
        ppid = int(rest_fields[1]) if len(rest_fields) > 1 else 0
        if comm:
          names.append(comm)
        pid = ppid
        continue
      except (OSError, ValueError):
        break
    try:
      ps_proc = subprocess.run(
          ["ps", "-o", "ppid=,comm=", "-p", str(pid)],
          capture_output=True,
          text=True,
          check=False,
          timeout=1,
      )
      if ps_proc.returncode != 0 or not ps_proc.stdout.strip():
        break
      parts = ps_proc.stdout.strip().split(None, 1)
      if len(parts) < 2:
        break
      names.append(parts[1].strip())
      pid = int(parts[0])
    except (OSError, ValueError, subprocess.SubprocessError):
      break
  return tuple(names)


def _query_tmux_transport_hints() -> tuple[int | None, bool, bool]:
  """Queries active `tmux` client PID, `allow-passthrough`, and Kitty hints."""
  if not shutil.which("tmux"):
    return None, False, False
  client_pid: int | None = None
  has_kitty = bool(os.environ.get("KITTY_WINDOW_ID"))
  allow_pass = False
  try:
    cur_proc = subprocess.run(
        [
            "tmux",
            "display-message",
            "-p",
            "#{client_pid}\t#{client_termname}\t#{client_termtype}",
        ],
        capture_output=True,
        text=True,
        check=False,
        timeout=1,
    )
    if cur_proc.returncode == 0 and cur_proc.stdout.strip():
      cur_fields = cur_proc.stdout.strip().split("\t")
      if cur_fields and cur_fields[0].isdigit():
        client_pid = int(cur_fields[0])
        if "kitty" in " ".join(cur_fields[1:]).lower():
          has_kitty = True

    if client_pid is None:
      cl_proc = subprocess.run(
          [
              "tmux",
              "list-clients",
              "-F",
              "#{client_activity}\t#{client_pid}\t#{client_termname}\t#{client_termtype}",
          ],
          capture_output=True,
          text=True,
          check=False,
          timeout=1,
      )
      if cl_proc.returncode == 0 and cl_proc.stdout.strip():
        best_act = -1
        for line in cl_proc.stdout.splitlines():
          fields = line.split("\t")
          if len(fields) >= 2 and fields[0].isdigit() and fields[1].isdigit():
            act = int(fields[0])
            if act > best_act:
              best_act = act
              client_pid = int(fields[1])
              joined_term = " ".join(fields[2:]).lower()
              if "kitty" in joined_term:
                has_kitty = True

    pass_proc = subprocess.run(
        ["tmux", "show", "-gv", "allow-passthrough"],
        capture_output=True,
        text=True,
        check=False,
        timeout=1,
    )
    if pass_proc.returncode == 0:
      allow_pass = pass_proc.stdout.strip().lower() in ("on", "all")

    if not has_kitty:
      env_proc = subprocess.run(
          ["tmux", "show-environment", "-g", "TERM"],
          capture_output=True,
          text=True,
          check=False,
          timeout=1,
      )
      if env_proc.returncode == 0 and "kitty" in env_proc.stdout.lower():
        has_kitty = True
  except (OSError, subprocess.SubprocessError):
    pass
  return client_pid, allow_pass, has_kitty


def detect_terminal_transport(
    env: Mapping[str, str] | None = None,
    is_tty: bool | None = None,
    tmux_client_pid: int | None = None,
    tmux_allow_passthrough: bool | None = None,
    tmux_has_kitty: bool | None = None,
) -> mdview_constants.TransportKind:
  """Classifies whether the active terminal supports Kitty graphics or uses Mosh.

  Args:
    env: Environment dictionary (defaults to `os.environ`).
    is_tty: Whether output is an interactive TTY (defaults to `stdout.isatty()`).
    tmux_client_pid: Optional explicit `tmux` client PID for testing.
    tmux_allow_passthrough: Optional explicit `allow-passthrough` state.
    tmux_has_kitty: Optional explicit Kitty indicator inside `tmux`.

  Returns:
    `TransportKind` indicating `DIRECT_KITTY`, `TMUX_KITTY_SSH`, `MOSH`,
    `GENERIC_TTY`, or `NON_TTY`.

  Usage Example:
    transport = detect_terminal_transport()
  """
  environ = os.environ if env is None else env
  tty_ok = os.isatty(1) if is_tty is None else is_tty
  if not tty_ok:
    return mdview_constants.TransportKind.NON_TTY

  if environ.get("TMUX"):
    if (
        tmux_client_pid is None
        or tmux_allow_passthrough is None
        or tmux_has_kitty is None
    ):
      q_pid, q_pass, q_kitty = _query_tmux_transport_hints()
      c_pid = q_pid if tmux_client_pid is None else tmux_client_pid
      allow_p = q_pass if tmux_allow_passthrough is None else tmux_allow_passthrough
      kitty_ok = q_kitty if tmux_has_kitty is None else tmux_has_kitty
    else:
      c_pid = tmux_client_pid
      allow_p = tmux_allow_passthrough
      kitty_ok = tmux_has_kitty

    if c_pid is not None:
      ancestors = _process_ancestry_names(c_pid)
      if any("mosh-server" in name or "gmosh" in name for name in ancestors):
        return mdview_constants.TransportKind.MOSH
    if allow_p and kitty_ok:
      return mdview_constants.TransportKind.TMUX_KITTY_SSH
    return mdview_constants.TransportKind.GENERIC_TTY

  ancestors = _process_ancestry_names(os.getpid())
  if any("mosh-server" in name or "gmosh" in name for name in ancestors):
    return mdview_constants.TransportKind.MOSH

  term = environ.get("TERM", "").lower()
  term_prog = environ.get("TERM_PROGRAM", "").lower()
  if "kitty" in term or term_prog == "kitty" or environ.get("KITTY_WINDOW_ID"):
    return mdview_constants.TransportKind.DIRECT_KITTY
  return mdview_constants.TransportKind.GENERIC_TTY


def render_mermaid_blocks_to_png(
    blocks: Sequence[str],
    cache_dir: str | None = None,
) -> Mapping[int, str]:
  """Batch-renders Mermaid code strings to cached Dracula HiDPI PNG files.

  Cache hits return immediately in O(1) disk `stat`s without launching Node or
  headless Chrome. Cache misses are rendered together in a single browser pass.

  Args:
    blocks: Sequence of raw Mermaid diagram strings.
    cache_dir: Optional directory override for cached PNG files.

  Returns:
    Mapping from 0-based block index in `blocks` to rendered PNG file path for
    every block that rendered cleanly.

  Raises:
    MermaidRenderError: If required runtime binaries (`node`, `chrome`, or
      `mermaid.min.js`) are missing on cache miss.

  Usage Example:
    pngs = render_mermaid_blocks_to_png(["flowchart TD\\nA-->B"])
  """
  if not blocks:
    return {}

  resolved_cache = cache_dir or default_cache_dir()
  os.makedirs(resolved_cache, exist_ok=True)

  rendered_paths: dict[int, str] = {}
  pending_jobs: list[dict[str, str]] = []
  pending_indices: dict[str, list[int]] = {}

  for idx, code in enumerate(blocks):
    target_png = cache_png_path(resolved_cache, code)
    if os.path.isfile(target_png) and os.path.getsize(target_png) > 0:
      rendered_paths[idx] = target_png
      continue
    job_key = target_png
    if job_key not in pending_indices:
      pending_indices[job_key] = [idx]
      pending_jobs.append({
          "id": job_key,
          "code": code,
          "out_png": target_png,
      })
    else:
      pending_indices[job_key].append(idx)

  if not pending_jobs:
    return rendered_paths

  node_bin = find_node_binary()
  chrome_bin = find_chrome_binary()
  node_modules = os.environ.get(
      "MDVIEW_NODE_MODULES",
      os.path.expanduser("~/modules/mermaid-internal/node_modules"),
  )
  mermaid_js = os.environ.get(
      "MDVIEW_MERMAID_JS",
      os.path.join(node_modules, "mermaid", "dist", "mermaid.min.js"),
  )
  script_mjs = os.path.join(
      os.path.dirname(os.path.abspath(__file__)), "mdview_render_mermaid.mjs"
  )

  if not node_bin or not chrome_bin or not os.path.isfile(mermaid_js):
    raise MermaidRenderError(
        "Missing Mermaid HiDPI dependencies "
        f"(node={node_bin}, chrome={chrome_bin}, mermaid_js={mermaid_js})"
    )

  manifest_payload = {
      "chrome_path": chrome_bin,
      "mermaid_js_path": mermaid_js,
      "node_modules_dir": node_modules,
      "background": mdview_constants.DRACULA_BG_HEX,
      "scale": 2,
      "jobs": pending_jobs,
  }

  with tempfile.NamedTemporaryFile(
      mode="w", suffix=".json", prefix="mdview_mermaid_", delete=False
  ) as tmp_manifest:
    json.dump(manifest_payload, tmp_manifest)
    manifest_path = tmp_manifest.name

  try:
    proc = subprocess.run(
        [node_bin, script_mjs, manifest_path],
        capture_output=True,
        text=True,
        check=False,
        timeout=30,
    )
  finally:
    try:
      os.remove(manifest_path)
    except OSError:
      pass

  if proc.returncode != 0:
    raise MermaidRenderError(
        f"Mermaid batch renderer exited {proc.returncode}: {proc.stderr.strip()}"
    )

  try:
    response = json.loads(proc.stdout.strip())
  except json.JSONDecodeError as exc:
    raise MermaidRenderError(
        f"Invalid JSON from Mermaid renderer: {proc.stdout!r}"
    ) from exc

  for item in response.get("results", ()):
    job_id = item.get("id", "")
    if item.get("ok") and os.path.isfile(job_id) and os.path.getsize(job_id) > 0:
      for orig_idx in pending_indices.get(job_id, ()):
        rendered_paths[orig_idx] = job_id
    else:
      logging.warning(
          "Mermaid block failed graphical render (%s), will fall back.",
          item.get("error", "unknown error"),
      )

  return rendered_paths


def split_kitten_icat_output(raw_output: str) -> tuple[str, tuple[str, ...]]:
  """Splits `kitten icat --unicode-placeholder` output into upload + cell rows.

  Args:
    raw_output: Raw stdout captured from `kitten icat`.

  Returns:
    Tuple `(upload_escape, grid_lines)` where `upload_escape` holds the virtual
    placement APC/DCS upload sequences and `grid_lines` holds the `U+10EEEE`
    character lines without trailing carriage returns.

  Raises:
    MermaidRenderError: If `raw_output` lacks an upload sequence or placeholder
      grid lines.

  Usage Example:
    upload, rows = split_kitten_icat_output(icat_stdout)
  """
  trimmed = raw_output.lstrip("\r\n")
  if not trimmed.startswith(("\x1b_G", "\x1bPtmux;")):
    raise MermaidRenderError(
        "kitten icat output did not start with a Kitty APC or tmux DCS upload."
    )

  sgr_idx = trimmed.find("\x1b[38")
  if sgr_idx <= 0:
    raise MermaidRenderError(
        "kitten icat output missing SGR foreground color for U+10EEEE grid."
    )

  upload_escape = trimmed[:sgr_idx]
  remainder = trimmed[sgr_idx:].replace("\r", "")
  lines = tuple(line for line in remainder.split("\n") if line)
  if not lines or not all(
      mdview_constants.UNICODE_PLACEHOLDER_CHAR in line for line in lines
  ):
    raise MermaidRenderError(
        "kitten icat output did not produce valid U+10EEEE placeholder rows."
    )
  return upload_escape, lines


def deterministic_image_id(png_path: str, salt: int = 0) -> int:
  """Derives a non-zero 24-bit Kitty image ID in `[1, 16777215]`.

  Args:
    png_path: File path of the PNG image.
    salt: Integer disambiguator when the same diagram appears twice.

  Returns:
    Integer in `[1, 16777215]` suitable for `--image-id`.

  Usage Example:
    img_id = deterministic_image_id("/tmp/a.png", salt=0)
  """
  checksum = zlib.crc32(f"{png_path}:{salt}".encode("utf-8")) & 0xFFFFFE
  return checksum + 1


def build_kitty_placement(
    png_path: str,
    max_cols: int,
    tmux_passthrough: bool = False,
    salt: int = 0,
    geometry: TerminalGeometry | None = None,
) -> KittyPlacement:
  """Invokes `kitten icat` to produce a virtual placement and `U+10EEEE` grid.

  Args:
    png_path: Path to rendered PNG file.
    max_cols: Maximum horizontal character cells available for the diagram.
    tmux_passthrough: Whether to wrap upload chunks in `tmux` DCS passthrough.
    salt: Disambiguator index so duplicate diagrams get distinct placement IDs.
    geometry: Optional pre-queried terminal cell/pixel geometry.

  Returns:
    `KittyPlacement` ready for TTY upload and viewport splicing.

  Raises:
    MermaidRenderError: If `kitten` is missing or exits non-zero.

  Usage Example:
    placement = build_kitty_placement("/tmp/d.png", max_cols=100)
  """
  kitten_bin = find_kitten_binary()
  if not kitten_bin:
    raise MermaidRenderError("kitten binary not found on PATH or standard dirs.")

  geom = geometry or query_terminal_geometry(fallback_cols=max_cols)
  cell_px_w = max(4, geom.width_px // max(1, geom.cols))
  cell_px_h = max(8, geom.height_px // max(1, geom.rows))
  bounded_cols = max(16, max_cols)
  bounded_rows = max(20, geom.rows)
  win_size_arg = (
      f"{bounded_cols},{bounded_rows},"
      f"{bounded_cols * cell_px_w},{bounded_rows * cell_px_h}"
  )

  image_id = deterministic_image_id(png_path, salt=salt)
  passthrough_mode = "tmux" if tmux_passthrough else "none"
  cmd = [
      kitten_bin,
      "icat",
      "--transfer-mode=stream",
      f"--passthrough={passthrough_mode}",
      "--unicode-placeholder",
      "--align=left",
      "--scale-up=no",
      "--fit=width",
      f"--image-id={image_id}",
      f"--use-window-size={win_size_arg}",
      png_path,
  ]
  proc = subprocess.run(
      cmd,
      capture_output=True,
      check=False,
      timeout=10,
  )
  if proc.returncode != 0:
    stderr_msg = proc.stderr.decode("utf-8", errors="replace").strip()
    raise MermaidRenderError(
        f"kitten icat failed ({proc.returncode}): {stderr_msg}"
    )

  upload_escape, grid_lines = split_kitten_icat_output(
      proc.stdout.decode("utf-8")
  )
  return KittyPlacement(
      image_id=image_id,
      upload_escape=upload_escape,
      grid_lines=grid_lines,
      png_path=png_path,
  )
