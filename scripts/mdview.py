#!/usr/bin/env python3
"""Markdown + Mermaid terminal renderer and interactive Jetski review CLI.

Combines `mdcat` Dracula Markdown typography/tables with batched HiDPI Kitty
`U+10EEEE` Mermaid PNG placements (and width-bounded Unicode fallback) while
mapping every viewport row to its 1-based raw source line number.

Usage Example:
  doc = render_document("# Title\\n", columns=100)
"""

from __future__ import annotations

import argparse
from collections.abc import Sequence
import dataclasses
import logging
import os
import shutil
import subprocess
import sys

# Guard against google3/calendar stdlib shadowing when cwd is inside CitC.
sys.path[:] = [
    entry
    for entry in sys.path
    if entry and "/google3" not in entry
]
_SCRIPTS_DIR = os.path.dirname(os.path.abspath(__file__))
if _SCRIPTS_DIR not in sys.path:
  sys.path.insert(0, _SCRIPTS_DIR)

import mdview_constants
import mdview_kitty
import mdview_mermaid_unicode
import mdview_source_map


@dataclasses.dataclass(frozen=True)
class RenderedDocument:
  """Complete rendered viewport lines, Kitty uploads, and 1-based line map.

  Attributes:
    lines: Rendered ANSI + `U+10EEEE` lines ready for display.
    source_lines: One-based raw Markdown line number for each entry in `lines`.
    upload_escapes: Kitty `U=1` virtual placement uploads to send to TTY.
    diagram_row_flags: Boolean per row indicating `U+10EEEE` image cells.
    raw_lines: Original raw Markdown lines for raw-source toggle.
  """

  lines: tuple[str, ...]
  source_lines: tuple[int, ...]
  upload_escapes: tuple[str, ...]
  diagram_row_flags: tuple[bool, ...]
  raw_lines: tuple[str, ...]


def find_mdcat_binary() -> str | None:
  """Locates `mdcat` across PATH and `~/.cargo/bin/mdcat`.

  Returns:
    Executable path string if installed, or `None`.

  Usage Example:
    mdcat_bin = find_mdcat_binary()
  """
  if env_bin := os.environ.get("MDVIEW_MDCAT_BIN"):
    if os.access(env_bin, os.X_OK):
      return env_bin
  if found := shutil.which("mdcat"):
    return found
  cargo_mdcat = os.path.expanduser("~/.cargo/bin/mdcat")
  return cargo_mdcat if os.access(cargo_mdcat, os.X_OK) else None


def run_mdcat_ansi(markdown_text: str, columns: int) -> tuple[str, ...]:
  """Renders Markdown prose, tables, alerts, and code blocks via `mdcat`.

  Args:
    markdown_text: Sentinel-substituted Markdown content.
    columns: Target wrap width in terminal columns.

  Returns:
    Tuple of ANSI-formatted lines emitted by `mdcat`.

  Usage Example:
    lines = run_mdcat_ansi("# Hello\\n", columns=80)
  """
  bounded_cols = max(mdview_constants.MIN_COLUMNS, columns)
  mdcat_bin = find_mdcat_binary()
  if not mdcat_bin:
    raw_out_lines = markdown_text.splitlines()
  else:
    env = dict(os.environ)
    env["BAT_THEME"] = mdview_constants.DEFAULT_BAT_THEME
    proc = subprocess.run(
        [
            mdcat_bin,
            "--no-pager",
            "--ansi",
            f"--theme={mdview_constants.DEFAULT_MDCAT_THEME}",
            f"--columns={bounded_cols}",
            "-",
        ],
        input=markdown_text,
        capture_output=True,
        text=True,
        env=env,
        check=False,
        timeout=15,
    )
    if proc.returncode != 0:
      logging.warning(
          "mdcat exited %d: %s", proc.returncode, proc.stderr.strip()
      )
      raw_out_lines = markdown_text.splitlines()
    else:
      raw_out_lines = proc.stdout.splitlines()

  wrapped_lines: list[str] = []
  for raw_line in raw_out_lines:
    wrapped_lines.extend(
        mdview_source_map.wrap_ansi_line_to_width(raw_line, bounded_cols)
    )
  return tuple(wrapped_lines)


def _format_raw_mermaid_fallback(code: str, columns: int) -> tuple[str, ...]:
  """Formats raw Mermaid source inside a subtle Dracula border on fallback."""
  sgr = mdview_constants.DRACULA_SGR
  inner_width = max(16, columns - 4)
  header = f"{sgr['comment']}╭─ mermaid ─{'─' * max(0, inner_width - 10)}╮{sgr['reset']}"
  footer = f"{sgr['comment']}╰{'─' * (inner_width + 2)}╯{sgr['reset']}"
  rows = [header]
  for raw_line in code.splitlines():
    clipped = raw_line[:inner_width]
    pad = " " * max(0, inner_width - len(clipped))
    rows.append(
        f"{sgr['comment']}│ {sgr['cyan']}{clipped}{pad}{sgr['comment']} │{sgr['reset']}"
    )
  rows.append(footer)
  return tuple(rows)


def render_document(
    markdown_text: str,
    columns: int = mdview_constants.DEFAULT_MAX_COLUMNS,
    diagram_mode: mdview_constants.DiagramMode = mdview_constants.DiagramMode.AUTO,
    tmux_passthrough: bool = False,
    cache_dir: str | None = None,
) -> RenderedDocument:
  """Renders a Markdown document and its Mermaid diagrams into viewport lines.

  Args:
    markdown_text: Full raw Markdown source text.
    columns: Maximum width in terminal cells for prose, tables, and diagrams.
    diagram_mode: Desired Mermaid rendering strategy (`auto`, `kitty`,
      `unicode`, or `raw`).
    tmux_passthrough: Whether Kitty uploads require `tmux` DCS wrapping.
    cache_dir: Optional directory path for cached Mermaid PNGs.

  Returns:
    `RenderedDocument` containing rendered lines, Kitty upload sequences, and
    1-based source line numbers.

  Usage Example:
    rendered = render_document("# Title\\n", columns=100)
  """
  bounded_cols = max(mdview_constants.MIN_COLUMNS, columns)
  extracted = mdview_source_map.extract_mermaid_fences(markdown_text)

  effective_mode = diagram_mode
  if effective_mode == mdview_constants.DiagramMode.AUTO:
    transport = mdview_kitty.detect_terminal_transport()
    if transport in (
        mdview_constants.TransportKind.DIRECT_KITTY,
        mdview_constants.TransportKind.TMUX_KITTY_SSH,
    ):
      effective_mode = mdview_constants.DiagramMode.KITTY
    else:
      effective_mode = mdview_constants.DiagramMode.UNICODE
  elif effective_mode == mdview_constants.DiagramMode.KITTY:
    if (
        mdview_kitty.detect_terminal_transport()
        == mdview_constants.TransportKind.MOSH
    ):
      logging.warning(
          "Active session runs under mosh-server (gmosh), which strips Kitty "
          "graphics; falling back to Unicode diagrams."
      )
      effective_mode = mdview_constants.DiagramMode.UNICODE

  placements_by_sentinel: dict[str, tuple[str, ...]] = {}
  is_kitty_by_sentinel: dict[str, bool] = {}
  block_by_sentinel: dict[str, mdview_source_map.MermaidFenceBlock] = {
      block.sentinel: block for block in extracted.blocks
  }
  upload_escapes: list[str] = []
  diag_max_width = max(20, bounded_cols - 2)

  if extracted.blocks:
    if effective_mode == mdview_constants.DiagramMode.KITTY:
      try:
        png_map = mdview_kitty.render_mermaid_blocks_to_png(
            blocks=tuple(b.source_text for b in extracted.blocks),
            cache_dir=cache_dir,
        )
        geom = mdview_kitty.query_terminal_geometry(fallback_cols=bounded_cols)
        for block in extracted.blocks:
          if png_path := png_map.get(block.index):
            placement = mdview_kitty.build_kitty_placement(
                png_path=png_path,
                max_cols=diag_max_width,
                tmux_passthrough=tmux_passthrough,
                salt=block.index,
                geometry=geom,
            )
            upload_escapes.append(placement.upload_escape)
            placements_by_sentinel[block.sentinel] = placement.grid_lines
            is_kitty_by_sentinel[block.sentinel] = True
          else:
            placements_by_sentinel[block.sentinel] = (
                mdview_mermaid_unicode.render_mermaid_unicode(
                    block.source_text, max_width=diag_max_width
                )
            )
            is_kitty_by_sentinel[block.sentinel] = False
      except mdview_kitty.MermaidRenderError as exc:
        logging.warning("Kitty Mermaid rendering unavailable (%s).", exc)
        for block in extracted.blocks:
          placements_by_sentinel[block.sentinel] = (
              mdview_mermaid_unicode.render_mermaid_unicode(
                  block.source_text, max_width=diag_max_width
              )
          )
          is_kitty_by_sentinel[block.sentinel] = False
    elif effective_mode == mdview_constants.DiagramMode.UNICODE:
      for block in extracted.blocks:
        placements_by_sentinel[block.sentinel] = (
            mdview_mermaid_unicode.render_mermaid_unicode(
                block.source_text, max_width=diag_max_width
            )
        )
        is_kitty_by_sentinel[block.sentinel] = False
    else:
      for block in extracted.blocks:
        placements_by_sentinel[block.sentinel] = _format_raw_mermaid_fallback(
            block.source_text, bounded_cols
        )
        is_kitty_by_sentinel[block.sentinel] = False

  mdcat_lines = run_mdcat_ansi(extracted.substituted_markdown, bounded_cols)

  final_lines: list[str] = []
  diagram_flags: list[bool] = []
  collapse_spans: list[mdview_source_map.CollapseSpan] = []

  for line in mdcat_lines:
    plain = mdview_source_map.strip_terminal_escapes(line).strip()
    matched_sentinel = None
    if mdview_constants.SENTINEL_PREFIX in plain:
      for sentinel in block_by_sentinel:
        if sentinel in plain:
          matched_sentinel = sentinel
          break

    if matched_sentinel is None:
      final_lines.append(line)
      diagram_flags.append(False)
      continue

    block = block_by_sentinel[matched_sentinel]
    replacement_rows = placements_by_sentinel.get(matched_sentinel, ())
    is_kitty_diagram = is_kitty_by_sentinel.get(matched_sentinel, False)
    span_start = len(final_lines)
    for diag_row in replacement_rows:
      final_lines.append(diag_row)
      diagram_flags.append(is_kitty_diagram)
    span_end = len(final_lines)
    collapse_spans.append(
        mdview_source_map.CollapseSpan(
            start_row=span_start,
            end_row=span_end,
            source_line=block.start_line,
            end_source_line=block.end_line,
        )
    )

  source_lines = mdview_source_map.map_rendered_lines_to_source(
      raw_markdown=markdown_text,
      rendered_lines=final_lines,
      spans=collapse_spans,
  )

  return RenderedDocument(
      lines=tuple(final_lines),
      source_lines=source_lines,
      upload_escapes=tuple(upload_escapes),
      diagram_row_flags=tuple(diagram_flags),
      raw_lines=extracted.raw_lines,
  )


def _write_uploads_to_tty(upload_escapes: Sequence[str]) -> None:
  """Writes Kitty virtual placement upload sequences directly to `/dev/tty`."""
  if not upload_escapes:
    return
  payload = "".join(upload_escapes).encode("utf-8")
  try:
    tty_fd = os.open("/dev/tty", os.O_WRONLY)
  except OSError:
    sys.stdout.buffer.write(payload)
    sys.stdout.buffer.flush()
    return
  try:
    os.write(tty_fd, payload)
  finally:
    os.close(tty_fd)


def build_arg_parser() -> argparse.ArgumentParser:
  """Constructs the command-line argument parser for `mdview`.

  Returns:
    Configured `argparse.ArgumentParser` instance.

  Usage Example:
    parser = build_arg_parser()
  """
  parser = argparse.ArgumentParser(
      prog="mdview",
      description="HiDPI Kitty Markdown + Mermaid viewer & Jetski commenter.",
  )
  parser.add_argument(
      "file",
      nargs="?",
      default="-",
      help="Markdown file path to render, or '-' for stdin.",
  )
  parser.add_argument(
      "-w",
      "--columns",
      type=int,
      default=0,
      help="Maximum column width (0 auto-detects up to 120 in non-TUI mode).",
  )
  parser.add_argument(
      "--diagrams",
      choices=[m.value for m in mdview_constants.DiagramMode],
      default=mdview_constants.DiagramMode.AUTO.value,
      help="Diagram rendering backend (default: auto).",
  )
  parser.add_argument(
      "--no-tui",
      action="store_true",
      help="Print rendered output directly (or to --pager) without TUI.",
  )
  parser.add_argument(
      "--pager",
      action="store_true",
      help="Pipe --no-tui output through less -R.",
  )
  parser.add_argument(
      "--cache-dir",
      default=None,
      help="Override directory for cached Mermaid PNG images.",
  )
  parser.add_argument(
      "--ephemeral-ssh",
      action="store_true",
      help="Suppress stdout feedback echo when invoked inside ephemeral Kitty SSH.",
  )
  parser.add_argument(
      "-p",
      "--pick",
      action="store_true",
      help="Auto-open queued file or prompt with ranked fzf Markdown picker.",
  )
  parser.add_argument(
      "--queue",
      metavar="FILE",
      default=None,
      help="Queue FILE (60s TTL) for instant open on next kitty_mod+m shortcut.",
  )
  return parser


def _export_outcome(
    outcome: object,
    ephemeral_ssh: bool,
) -> None:
  """Exports non-empty review feedback to clipboards and optional stdout."""
  import mdview_review_store
  import mdview_tui

  assert isinstance(outcome, mdview_tui.ExitOutcome)
  if outcome.copy_to_clipboard and outcome.feedback_text:
    mdview_review_store.export_feedback_to_clipboards(outcome.feedback_text)
    if not ephemeral_ssh:
      sys.stdout.write(outcome.feedback_text)
      if not outcome.feedback_text.endswith("\n"):
        sys.stdout.write("\n")
      sys.stdout.flush()


def main(argv: Sequence[str] | None = None) -> int:
  """CLI entrypoint for `mdview`.

  Args:
    argv: Optional sequence of CLI arguments (excluding program name).

  Returns:
    Process exit status code (`0` on success).

  Usage Example:
    rc = main(["--no-tui", "README.md"])
  """
  args = build_arg_parser().parse_args(argv)
  mode = mdview_constants.DiagramMode(args.diagrams)
  in_tmux = bool(os.environ.get("TMUX"))

  if args.queue is not None:
    import mdview_picker

    try:
      staged = mdview_picker.queue_target_file(args.queue)
    except FileNotFoundError as exc:
      logging.error("%s", exc)
      return 1
    sys.stderr.write(
        f"Queued {os.path.basename(staged)} (60s TTL) — press "
        "Ctrl+Shift+M twice in Kitty to open in HiDPI.\n"
    )
    return 0

  if args.pick:
    import mdview_picker
    import mdview_tui

    first_pass = True
    while True:
      selected: str | None = None
      if first_pass:
        first_pass = False
        selected = mdview_picker.consume_queued_target()
      if selected is None:
        candidates = mdview_picker.collect_markdown_candidates()
        if not candidates:
          logging.error("No Markdown documents found to display.")
          return 1
        selected = mdview_picker.run_fzf_picker(candidates)
      if not selected:
        return 0
      outcome = mdview_tui.run_interactive_tui(
          filepath=selected,
          diagram_mode=mode,
          tmux_passthrough=in_tmux,
          cache_dir=args.cache_dir,
      )
      if outcome.switch_to_picker:
        continue
      _export_outcome(outcome, args.ephemeral_ssh)
      return 0

  if not args.no_tui and not args.pager and args.file != "-" and sys.stdout.isatty():
    if not os.path.isfile(args.file):
      logging.error("File not found: %s", args.file)
      return 1
    import mdview_picker
    import mdview_tui

    current_file: str | None = args.file
    while current_file:
      outcome = mdview_tui.run_interactive_tui(
          filepath=current_file,
          diagram_mode=mode,
          tmux_passthrough=in_tmux,
          cache_dir=args.cache_dir,
      )
      if outcome.switch_to_picker:
        candidates = mdview_picker.collect_markdown_candidates()
        current_file = mdview_picker.run_fzf_picker(candidates)
        continue
      _export_outcome(outcome, args.ephemeral_ssh)
      return 0
    return 0

  if args.file == "-":
    markdown_text = sys.stdin.read()
  else:
    if not os.path.isfile(args.file):
      logging.error("File not found: %s", args.file)
      return 1
    with open(args.file, encoding="utf-8") as f:
      markdown_text = f.read()

  geom = mdview_kitty.query_terminal_geometry()
  target_cols = (
      args.columns
      if args.columns > 0
      else min(geom.cols, mdview_constants.DEFAULT_MAX_COLUMNS)
  )
  rendered = render_document(
      markdown_text=markdown_text,
      columns=target_cols,
      diagram_mode=mode,
      tmux_passthrough=in_tmux,
      cache_dir=args.cache_dir,
  )

  _write_uploads_to_tty(rendered.upload_escapes)
  output_text = "\n".join(rendered.lines) + "\n"
  if args.pager:
    env = {k: v for k, v in os.environ.items() if k != "LESS"}
    subprocess.run(
        [
            "less",
            "-R",
            "--mouse",
            "--status-line",
            "--incsearch",
            "--quit-if-one-screen",
            "--quit-on-intr",
        ],
        input=output_text,
        text=True,
        env=env,
        check=False,
    )
    return 0

  sys.stdout.write(output_text)
  sys.stdout.flush()
  return 0


if __name__ == "__main__":
  sys.exit(main())
