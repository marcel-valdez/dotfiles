"""Unit tests for Task 2: persistent review store, Jetski export, and TUI."""

from __future__ import annotations

import base64
import os
import shutil
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.expanduser("~/scripts"))

import mdview
import mdview_review_store
import mdview_source_map
import mdview_tui


class ReviewStoreAndFormattingTests(unittest.TestCase):
  """Granular tests for persistence and exact Jetski artifact review format."""

  def setUp(self) -> None:
    self.temp_dir = tempfile.mkdtemp(prefix="test_mdview_reviews_")
    self.md_path = os.path.join(self.temp_dir, "2026-11-20-plan.md")
    with open(self.md_path, "w", encoding="utf-8") as f:
      f.write("# Heading\n\nLine three\nLine four\n")

  def tearDown(self) -> None:
    shutil.rmtree(self.temp_dir, ignore_errors=True)

  def test_formats_comments_matching_jetski_artifact_detail_go(self) -> None:
    formatted = mdview_review_store.format_jetski_feedback(
        filename="2026-11-20-plan.md",
        comments={54: "Second note", 34: "First note"},
        decision=mdview_review_store.ReviewStatus.COMMENT_ONLY,
    )
    self.assertEqual(
        formatted,
        "[2026-11-20-plan.md]\nL34: First note\nL54: Second note\n",
    )

  def test_formats_approval_matching_jetski_format_decision(self) -> None:
    formatted = mdview_review_store.format_jetski_feedback(
        filename="2026-11-20-plan.md",
        comments={},
        decision=mdview_review_store.ReviewStatus.APPROVED,
    )
    self.assertEqual(formatted, "[Approved] 2026-11-20-plan.md")

  def test_persists_and_reloads_comments_and_detects_file_mutation(
      self,
  ) -> None:
    state, changed = mdview_review_store.load_review_state(
        self.md_path, reviews_dir=self.temp_dir
    )
    self.assertFalse(changed)
    state.comments[3] = "Check edge case"
    state.cursor_row = 2
    state.scroll_top = 1
    mdview_review_store.save_review_state(state, reviews_dir=self.temp_dir)

    reloaded, changed_same = mdview_review_store.load_review_state(
        self.md_path, reviews_dir=self.temp_dir
    )
    self.assertFalse(changed_same)
    self.assertEqual(reloaded.comments, {3: "Check edge case"})
    self.assertEqual(reloaded.cursor_row, 2)

    with open(self.md_path, "a", encoding="utf-8") as f:
      f.write("Appended new section\n")
    _, changed_after_edit = mdview_review_store.load_review_state(
        self.md_path, reviews_dir=self.temp_dir
    )
    self.assertTrue(changed_after_edit)

  def test_osc52_clipboard_export_writes_expected_escape_and_file(self) -> None:
    emitted: list[bytes] = []
    last_file = os.path.join(self.temp_dir, "last_review.txt")
    payload = "[doc.md]\nL10: Fix typo\n"
    ok = mdview_review_store.export_feedback_to_clipboards(
        payload=payload,
        tty_writer=emitted.append,
        last_review_path=last_file,
        in_tmux=False,
        run_async_helper=False,
    )
    self.assertTrue(ok)
    expected_b64 = base64.b64encode(payload.encode("utf-8")).decode("ascii")
    self.assertEqual(emitted, [f"\x1b]52;c;{expected_b64}\x07".encode("ascii")])
    with open(last_file, encoding="utf-8") as f:
      self.assertEqual(f.read(), payload)


class ViewerControllerTests(unittest.TestCase):
  """Granular tests for TUI navigation, U+10EEEE safety, and commenting."""

  def setUp(self) -> None:
    self.temp_dir = tempfile.mkdtemp(prefix="test_mdview_tui_")
    self.md_path = os.path.join(self.temp_dir, "sample.md")
    with open(self.md_path, "w", encoding="utf-8") as f:
      f.write("# Title\n\nBody paragraph\n\n```mermaid\nflowchart TD\nA-->B\n```\n")
    self.doc = mdview.RenderedDocument(
        lines=(
            "\x1b[1mTitle\x1b[0m",
            "",
            "Body paragraph",
            "\x1b[38:2:12:34:56m\U0010eeee\u0305\u0305\U0010eeee\u0305\u030d\x1b[39m",
            "\x1b[38:2:12:34:56m\U0010eeee\u030d\u0305\U0010eeee\u030d\u030d\x1b[39m",
        ),
        source_lines=(1, 2, 3, 5, 5),
        upload_escapes=("\x1b_Ga=T,U=1,i=795192;AAAA\x1b\\",),
        diagram_row_flags=(False, False, False, True, True),
        raw_lines=(
            "# Title",
            "",
            "Body paragraph",
            "",
            "```mermaid",
            "flowchart TD",
            "A-->B",
            "```",
        ),
    )

  def tearDown(self) -> None:
    shutil.rmtree(self.temp_dir, ignore_errors=True)

  def test_preserves_kitty_placeholder_sgr_bytes_when_caret_is_on_diagram_row(
      self,
  ) -> None:
    ctrl = mdview_tui.ViewerController(
        filepath=self.md_path,
        document=self.doc,
        reviews_dir=self.temp_dir,
        term_cols=90,
        term_rows=20,
    )
    for _ in range(3):
      ctrl.handle_key("j")
    self.assertEqual(ctrl.state.cursor_row, 3)
    frame = ctrl.render_frame()
    # Exact SGR 38:2:12:34:56m + U+10EEEE sequence must survive unmodified!
    self.assertIn(
        "\x1b[38:2:12:34:56m\U0010eeee\u0305\u0305\U0010eeee\u0305\u030d\x1b[39m",
        frame,
    )

  def test_adds_views_and_exports_comment_keeping_draft_on_q_and_clearing_on_approve(
      self,
  ) -> None:
    ctrl = mdview_tui.ViewerController(
        filepath=self.md_path,
        document=self.doc,
        reviews_dir=self.temp_dir,
        term_cols=90,
        term_rows=20,
    )
    ctrl.handle_key("j")
    ctrl.handle_key("j")
    self.assertEqual(ctrl.current_source_line(), 3)

    ctrl.handle_key("c")
    for ch in "Expand body":
      ctrl.handle_key(ch)
    ctrl.handle_key("\r")
    self.assertEqual(ctrl.state.comments[3], "Expand body")

    frame = ctrl.render_frame()
    self.assertIn("●", frame)
    self.assertIn(
        "L3: Expand body", mdview_source_map.strip_terminal_escapes(frame)
    )

    outcome_q = ctrl.handle_key("q")
    self.assertIsNotNone(outcome_q)
    assert outcome_q is not None
    self.assertEqual(outcome_q.feedback_text, "[sample.md]\nL3: Expand body\n")
    # Draft remains persisted on disk after 'q' per confirmed lifecycle!
    reloaded, _ = mdview_review_store.load_review_state(
        self.md_path, reviews_dir=self.temp_dir
    )
    self.assertEqual(reloaded.comments, {3: "Expand body"})

    # Approving with 'A' emits [Approved] and clears draft on disk!
    ctrl2 = mdview_tui.ViewerController(
        filepath=self.md_path,
        document=self.doc,
        reviews_dir=self.temp_dir,
        term_cols=90,
        term_rows=20,
    )
    outcome_a = ctrl2.handle_key("A")
    self.assertIsNotNone(outcome_a)
    assert outcome_a is not None
    self.assertTrue(outcome_a.feedback_text.startswith("[Approved] sample.md"))
    cleared, _ = mdview_review_store.load_review_state(
        self.md_path, reviews_dir=self.temp_dir
    )
    self.assertEqual(cleared.comments, {})

  def test_ctrl_g_invokes_external_editor_callback_for_multiline_comment(
      self,
  ) -> None:
    ctrl = mdview_tui.ViewerController(
        filepath=self.md_path,
        document=self.doc,
        reviews_dir=self.temp_dir,
        term_cols=90,
        term_rows=20,
        external_editor_fn=lambda initial, line: f"{initial}\n- bullet two",
    )
    ctrl.handle_key("c")
    for ch in "Header":
      ctrl.handle_key(ch)
    ctrl.handle_key("\x07")  # Ctrl+G
    self.assertEqual(ctrl.state.comments[1], "Header\n- bullet two")


if __name__ == "__main__":
  unittest.main()
