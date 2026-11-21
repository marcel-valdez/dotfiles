"""Unit and integration tests for Task 1: mdview rendering & source mapping."""

from __future__ import annotations

import os
import shutil
import sys
import tempfile
import unittest
from unittest import mock

sys.path.insert(0, os.path.expanduser("~/scripts"))

import mdview
import mdview_constants
import mdview_kitty
import mdview_source_map


SAMPLE_COMPLEX_DOC = """# Visual Architecture Guide

> [!IMPORTANT]
> Diagrams include quoted semicolons and long ATX headings exceeding 80 columns.

## Overview Pipeline

```mermaid
flowchart TD
  U0["Upstream v0"] --> U1["Upstream v1"]
  U1 --> Merge["Three-Way Merge"]
```

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

```mermaid
flowchart TD
  Start["Start"] --> Check{"Conflict?"}
  Check -->|No| Apply["Apply Patch"]
  Check -->|Yes| Halt["Halt & Report"]
```
"""


class FenceExtractionTests(unittest.TestCase):
  """Granular tests for CommonMark Mermaid fence extraction."""

  def test_extracts_standard_backtick_and_tilde_mermaid_fences(self) -> None:
    doc = (
        "# Title\n\n"
        "```mermaid\n"
        "flowchart TD\n"
        "  A --> B\n"
        "```\n\n"
        "Paragraph.\n\n"
        "~~~mermaid\n"
        "sequenceDiagram\n"
        "  Alice->>Bob: Hi\n"
        "~~~\n"
    )
    extracted = mdview_source_map.extract_mermaid_fences(doc)
    self.assertEqual(len(extracted.blocks), 2)
    self.assertEqual(extracted.blocks[0].start_line, 3)
    self.assertEqual(extracted.blocks[0].end_line, 6)
    self.assertIn("A --> B", extracted.blocks[0].source_text)
    self.assertEqual(extracted.blocks[1].start_line, 10)
    self.assertEqual(extracted.blocks[1].end_line, 13)
    self.assertIn(extracted.blocks[0].sentinel, extracted.substituted_markdown)
    self.assertIn(extracted.blocks[1].sentinel, extracted.substituted_markdown)

  def test_ignores_mermaid_fence_nested_inside_outer_four_backtick_fence(
      self,
  ) -> None:
    doc = (
        "# Example\n\n"
        "````markdown\n"
        "```mermaid\n"
        "flowchart TD\n"
        "  Inner --> Node\n"
        "```\n"
        "````\n"
    )
    extracted = mdview_source_map.extract_mermaid_fences(doc)
    self.assertEqual(extracted.blocks, ())
    self.assertIn("Inner --> Node", extracted.substituted_markdown)

  def test_handles_empty_and_no_mermaid_document(self) -> None:
    extracted = mdview_source_map.extract_mermaid_fences("# Just Heading\nText")
    self.assertEqual(extracted.blocks, ())
    self.assertEqual(len(extracted.raw_lines), 2)


class SourceMappingTests(unittest.TestCase):
  """Granular tests for terminal escape stripping and 1-based line mapping."""

  def test_strip_terminal_escapes_removes_sgr_and_osc8_links(self) -> None:
    styled = (
        "\x1b[1m\x1b[38;2;189;147;249mHeading\x1b[0m "
        "\x1b]8;;file:///tmp/a.md\x1b\\link\x1b]8;;\x1b\\"
    )
    self.assertEqual(
        mdview_source_map.strip_terminal_escapes(styled), "Heading link"
    )

  def test_visible_cell_width_counts_kitty_unicode_placeholders_accurately(
      self,
  ) -> None:
    placeholder_row = (
        "\x1b[38:2:156:61:15m"
        "\U0010eeee\u0305\u0305\u0305"
        "\U0010eeee\u0305\u030d\u0305"
        "\U0010eeee\u0305\u030e\u0305"
        "\x1b[39m"
    )
    self.assertEqual(mdview_source_map.visible_cell_width(placeholder_row), 3)

  def test_wraps_ansi_styled_long_heading_within_column_budget(self) -> None:
    styled_heading = (
        "\x1b[34m── Case 1: Change B Is in a Different Part of the File "
        "Than Change A (Clean Merge)\x1b[0m"
    )
    wrapped = mdview_source_map.wrap_ansi_line_to_width(styled_heading, 80)
    self.assertEqual(len(wrapped), 2)
    for row in wrapped:
      self.assertLessEqual(mdview_source_map.visible_cell_width(row), 80)
      self.assertIn("\x1b[34m", row)
      self.assertTrue(row.endswith("\x1b[0m"))
    rejoined = " ".join(
        mdview_source_map.strip_terminal_escapes(r) for r in wrapped
    )
    self.assertIn("Clean Merge)", rejoined)

  def test_maps_wrapped_prose_and_pinned_mermaid_spans_monotonically(
      self,
  ) -> None:
    raw = (
        "# Document Title\n"
        "\n"
        "First paragraph with distinctive words alpha beta gamma delta.\n"
        "\n"
        "```mermaid\n"
        "flowchart TD\n"
        "  X --> Y\n"
        "```\n"
        "\n"
        "Final concluding sentence omega.\n"
    )
    rendered = (
        "\x1b[1mDocument Title\x1b[0m",
        "",
        "First paragraph with distinctive words",
        "alpha beta gamma delta.",
        "\x1b[38:2:0:0:1m\U0010eeee\u0305\u0305\x1b[39m",
        "\x1b[38:2:0:0:1m\U0010eeee\u030d\u0305\x1b[39m",
        "",
        "Final concluding sentence omega.",
    )
    spans = (mdview_source_map.CollapseSpan(start_row=4, end_row=6, source_line=5),)
    mapped = mdview_source_map.map_rendered_lines_to_source(
        raw, rendered, spans=spans
    )
    self.assertEqual(len(mapped), len(rendered))
    self.assertEqual(mapped[0], 1)
    self.assertEqual(mapped[2], 3)
    self.assertEqual(mapped[3], 3)
    self.assertEqual(mapped[4], 5)
    self.assertEqual(mapped[5], 5)
    self.assertEqual(mapped[7], 10)
    self.assertEqual(list(mapped), sorted(mapped))


class KittyStreamTests(unittest.TestCase):
  """Granular tests for splitting kitten icat streams and caching PNGs."""

  def setUp(self) -> None:
    self.temp_dir = tempfile.mkdtemp(prefix="test_mdview_kitty_")

  def tearDown(self) -> None:
    shutil.rmtree(self.temp_dir, ignore_errors=True)

  def test_splits_direct_kitty_apc_upload_and_placeholder_grid(self) -> None:
    raw_icat = (
        "\r\x1b_Ga=T,q=2,f=100,U=1,c=2,r=2,i=42;AAAA\x1b\\"
        "\x1b_Gm=0,q=2;BBBB\x1b\\"
        "\x1b[38:2:0:0:42m\U0010eeee\u0305\u0305\U0010eeee\u0305\u030d\x1b[39m\n"
        "\x1b[38:2:0:0:42m\U0010eeee\u030d\u0305\U0010eeee\u030d\u030d\x1b[39m\n"
    )
    upload, grid = mdview_kitty.split_kitten_icat_output(raw_icat)
    self.assertIn("\x1b_Ga=T,q=2,f=100,U=1", upload)
    self.assertNotIn("\U0010eeee", upload)
    self.assertEqual(len(grid), 2)
    self.assertTrue(all("\U0010eeee" in row for row in grid))

  def test_splits_tmux_dcs_passthrough_upload_and_placeholder_grid(self) -> None:
    raw_icat = (
        "\r\x1bPtmux;\x1b\x1b_Ga=T,q=2,f=100,U=1,c=2,r=1,i=7;AAAA\x1b\x1b\\\x1b\\"
        "\x1b[38:2:0:0:7m\U0010eeee\u0305\u0305\U0010eeee\u0305\u030d\x1b[39m\n"
    )
    upload, grid = mdview_kitty.split_kitten_icat_output(raw_icat)
    self.assertTrue(upload.startswith("\x1bPtmux;"))
    self.assertEqual(len(grid), 1)
    self.assertIn("\U0010eeee", grid[0])

  def test_cache_hit_skips_playwright_node_subprocess(self) -> None:
    code = 'flowchart TD\n  A["Cached"] --> B["Node"]'
    png_path = mdview_kitty.cache_png_path(self.temp_dir, code)
    os.makedirs(os.path.dirname(png_path), exist_ok=True)
    with open(png_path, "wb") as f:
      f.write(b"\x89PNG\r\n\x1a\nfake")
    with mock.patch("subprocess.run") as mock_run:
      rendered_map = mdview_kitty.render_mermaid_blocks_to_png(
          blocks=(code,), cache_dir=self.temp_dir
      )
      mock_run.assert_not_called()
    self.assertEqual(rendered_map[0], png_path)


class EndToEndRenderIntegrationTests(unittest.TestCase):
  """Portable integration test covering 4 Mermaid blocks, tables, and headings."""

  def setUp(self) -> None:
    self.cache_dir = tempfile.mkdtemp(prefix="test_mdview_e2e_cache_")

  def tearDown(self) -> None:
    shutil.rmtree(self.cache_dir, ignore_errors=True)

  def test_renders_multi_diagram_guide_with_kitty_placeholders(
      self,
  ) -> None:
    with mock.patch(
        "mdview_kitty.detect_terminal_transport",
        return_value=mdview_constants.TransportKind.DIRECT_KITTY,
    ):
      result = mdview.render_document(
          markdown_text=SAMPLE_COMPLEX_DOC,
          columns=110,
          diagram_mode=mdview_constants.DiagramMode.KITTY,
          tmux_passthrough=False,
          cache_dir=self.cache_dir,
      )
    self.assertEqual(len(result.upload_escapes), 4)
    self.assertEqual(len(result.lines), len(result.source_lines))
    self.assertEqual(len(result.lines), len(result.diagram_row_flags))
    self.assertTrue(any(result.diagram_row_flags))
    joined_text = "\n".join(result.lines)
    self.assertNotIn("@@MDVIEWDIAG", joined_text)
    self.assertIn("\U0010eeee", joined_text)
    cached_pngs = [
        name for name in os.listdir(self.cache_dir) if name.endswith(".png")
    ]
    self.assertEqual(len(cached_pngs), 4)


if __name__ == "__main__":
  unittest.main()
