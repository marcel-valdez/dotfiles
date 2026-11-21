"""Unit and integration tests for Task 4: gmosh detection & Unicode Mermaid."""

from __future__ import annotations

import os
import subprocess
import sys
import tempfile
import unittest
from unittest import mock

sys.path.insert(0, os.path.expanduser("~/scripts"))

import mdview
import mdview_constants
import mdview_kitty
import mdview_mermaid_unicode
import mdview_source_map


SAMPLE_COMPLEX_DOC = """# Visual Architecture Guide

### Case 1: Change B Is in a Different Part of the File Than Change A (Clean Merge)

| Column A | Column B | Column C | Column D |
| :--- | :--- | :--- | :--- |
| Clean merge | No conflict | Auto-applied | Verified |
| Overlapping edit | Conflict | Needs manual review | Halts merge |

```mermaid
flowchart LR
  W0["Week 0: Baseline = v0 (Initial synced config)"]
  W1["Week 1: Upstream lands CL A (adds FeatureBanner)"]
  ACK["Run --ack_only: Baseline moves to v1 (includes A); downstream untouched"]
  W0 --> W1 --> ACK
```

```mermaid
sequenceDiagram
  participant Local as Kitty SSH
  participant Remote as mdview TUI
  Local->>Remote: Open architecture_guide.md
  Remote-->>Local: OSC-52 [architecture_guide.md] L14
```
"""


class UnicodeMermaidLayoutTests(unittest.TestCase):
  """Granular tests for Sugiyama Unicode box layout and width ceilings."""

  def test_transposes_wide_lr_flowchart_and_preserves_semicolon_labels(
      self,
  ) -> None:
    code = (
        "flowchart LR\n"
        '  W0["Week 0: Baseline = v0 (Initial synced config)"]\n'
        '  W1["Week 1: Upstream lands CL A (adds FeatureBanner)"]\n'
        '  ACK["Run --ack_only: Baseline moves to v1 (includes A); downstream untouched"]\n'
        "  W0 --> W1 --> ACK\n"
    )
    rows = mdview_mermaid_unicode.render_mermaid_unicode(code, max_width=80)
    self.assertTrue(len(rows) >= 9)
    plain = "\n".join(mdview_source_map.strip_terminal_escapes(r) for r in rows)
    self.assertIn("--ack_only", plain)
    self.assertIn("downstream", plain)
    self.assertIn("untouched", plain)
    self.assertIn("▼", plain)
    for row in rows:
      self.assertLessEqual(mdview_source_map.visible_cell_width(row), 80)

  def test_renders_compact_lr_flowchart_horizontally_when_within_width(
      self,
  ) -> None:
    code = 'flowchart LR\n  A["Start"] --> B["Middle"] --> C["Done"]\n'
    rows = mdview_mermaid_unicode.render_mermaid_unicode(code, max_width=90)
    plain = "\n".join(mdview_source_map.strip_terminal_escapes(r) for r in rows)
    self.assertIn("▶", plain)
    for row in rows:
      self.assertLessEqual(mdview_source_map.visible_cell_width(row), 90)

  def test_renders_sequence_diagram_within_max_width(self) -> None:
    code = (
        "sequenceDiagram\n"
        "  participant CLI as Kitty SSH\n"
        "  participant TUI as mdview TUI\n"
        "  CLI->>TUI: Open plan.md\n"
        "  TUI-->>CLI: OSC-52 [plan.md] L34\n"
    )
    rows = mdview_mermaid_unicode.render_mermaid_unicode(code, max_width=76)
    plain = "\n".join(mdview_source_map.strip_terminal_escapes(r) for r in rows)
    self.assertIn("Kitty SSH", plain)
    self.assertIn("mdview TUI", plain)
    self.assertIn("Open plan.md", plain)
    for row in rows:
      self.assertLessEqual(mdview_source_map.visible_cell_width(row), 76)


class TransportAutoDetectionTests(unittest.TestCase):
  """Granular tests for mosh-server vs direct SSH Kitty transport detection."""

  def test_detects_mosh_when_ancestor_process_is_mosh_server(self) -> None:
    with mock.patch(
        "mdview_kitty._process_ancestry_names",
        return_value=("bash", "tmux: client", "mosh-server", "systemd"),
    ):
      kind = mdview_kitty.detect_terminal_transport(
          env={"TMUX": "/tmp/tmux-1000/default,123,0", "TERM": "xterm-256color"},
          is_tty=True,
          tmux_client_pid=999,
          tmux_allow_passthrough=True,
          tmux_has_kitty=True,
      )
    self.assertEqual(kind, mdview_constants.TransportKind.MOSH)

  def test_detects_direct_kitty_when_xterm_kitty_without_mosh(self) -> None:
    with mock.patch(
        "mdview_kitty._process_ancestry_names",
        return_value=("python3", "sshd", "systemd"),
    ):
      kind = mdview_kitty.detect_terminal_transport(
          env={"TERM": "xterm-kitty"},
          is_tty=True,
      )
    self.assertEqual(kind, mdview_constants.TransportKind.DIRECT_KITTY)

  def test_explicit_kitty_mode_on_mosh_falls_back_cleanly_to_unicode(
      self,
  ) -> None:
    md = "# Doc\n\n```mermaid\nflowchart TD\n  A[One] --> B[Two]\n```\n"
    with mock.patch(
        "mdview_kitty.detect_terminal_transport",
        return_value=mdview_constants.TransportKind.MOSH,
    ):
      doc = mdview.render_document(
          markdown_text=md,
          columns=80,
          diagram_mode=mdview_constants.DiagramMode.KITTY,
      )
    joined = "\n".join(
        mdview_source_map.strip_terminal_escapes(r) for r in doc.lines
    )
    self.assertEqual(doc.upload_escapes, ())
    self.assertIn("╭", joined)
    self.assertIn("One", joined)


class ComplexGuideWidthComplianceTests(unittest.TestCase):
  """Verifies strict 80-col and 120-col rendering on multi-diagram guide."""

  def test_unicode_mode_fits_within_80_and_120_columns(self) -> None:
    for target_cols in (80, 120):
      rendered = mdview.render_document(
          markdown_text=SAMPLE_COMPLEX_DOC,
          columns=target_cols,
          diagram_mode=mdview_constants.DiagramMode.UNICODE,
      )
      self.assertEqual(rendered.upload_escapes, ())
      for idx, line in enumerate(rendered.lines):
        width = mdview_source_map.visible_cell_width(line)
        self.assertLessEqual(
            width,
            target_cols,
            f"Row {idx} exceeded {target_cols} cols ({width}): "
            f"{mdview_source_map.strip_terminal_escapes(line)!r}",
        )

  def test_legacy_wrappers_delegate_cleanly_to_mdview(self) -> None:
    with tempfile.NamedTemporaryFile(
        mode="w", suffix=".md", encoding="utf-8", delete=False
    ) as tmp_md:
      tmp_md.write(SAMPLE_COMPLEX_DOC)
      tmp_path = tmp_md.name
    try:
      for script_name in ("mdcat.sh", "glow-mermaid"):
        script_path = os.path.expanduser(f"~/scripts/{script_name}")
        proc = subprocess.run(
            [script_path, "--no-tui", "--diagrams=unicode", "-w", "80", tmp_path],
            capture_output=True,
            text=True,
            check=False,
            timeout=15,
        )
        self.assertEqual(proc.returncode, 0, proc.stderr)
        self.assertIn("╭", proc.stdout)
    finally:
      os.remove(tmp_path)


if __name__ == "__main__":
  unittest.main()
