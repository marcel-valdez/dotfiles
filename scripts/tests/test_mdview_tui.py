"""Unit tests for Task 2: persistent review store, Jetski export, and TUI."""

from __future__ import annotations

import base64
import os
import shutil
import sys
import tempfile
import unittest
from unittest import mock

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
        collapse_spans=(
            mdview_source_map.CollapseSpan(
                start_row=3, end_row=5, source_line=5, end_source_line=8
            ),
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

  def test_write_all_fd_loops_across_short_pty_writes(self) -> None:
    written_chunks: list[bytes] = []

    def fake_write(fd: int, buf: memoryview) -> int:
      del fd
      chunk = bytes(buf[:16])
      written_chunks.append(chunk)
      return len(chunk)

    payload = b"0123456789abcdef" * 6 + b"tail"
    with mock.patch("os.write", side_effect=fake_write):
      mdview_tui.write_all_fd(99, payload)
    self.assertEqual(b"".join(written_chunks), payload)
    self.assertEqual(len(written_chunks), 7)

  def test_consume_pending_image_uploads_separates_uploads_from_sync_frame(
      self,
  ) -> None:
    ctrl = mdview_tui.ViewerController(
        filepath=self.md_path,
        document=self.doc,
        reviews_dir=self.temp_dir,
        term_cols=90,
        term_rows=20,
    )
    uploads = ctrl.consume_pending_image_uploads()
    self.assertEqual(uploads, self.doc.upload_escapes)
    self.assertEqual(ctrl.consume_pending_image_uploads(), ())
    frame = ctrl.render_frame()
    self.assertNotIn("\x1b_G", frame)

  def test_plus_minus_zero_keys_adjust_diagram_zoom_and_trigger_reload(
      self,
  ) -> None:
    reload_calls: list[tuple[int, int, float]] = []

    def fake_reload(
        prose_cols: int, diagram_cols: int, zoom: float
    ) -> mdview.RenderedDocument:
      reload_calls.append((prose_cols, diagram_cols, zoom))
      return self.doc

    ctrl = mdview_tui.ViewerController(
        filepath=self.md_path,
        document=self.doc,
        reviews_dir=self.temp_dir,
        term_cols=200,
        term_rows=50,
        reload_fn=fake_reload,
    )
    ctrl.consume_pending_image_uploads()

    ctrl.handle_key("+")
    self.assertAlmostEqual(ctrl.diagram_zoom, 1.25)
    self.assertTrue(ctrl.needs_image_reupload)
    self.assertEqual(reload_calls[-1], (120, 191, 1.25))

    ctrl.handle_key("-")
    self.assertAlmostEqual(ctrl.diagram_zoom, 1.0)
    self.assertEqual(reload_calls[-1], (120, 191, 1.0))

    ctrl.handle_key("+")
    ctrl.handle_key("0")
    self.assertAlmostEqual(ctrl.diagram_zoom, 1.0)
    self.assertEqual(reload_calls[-1], (120, 191, 1.0))

  def test_resize_reloads_diagrams_with_full_width_and_on_row_change(
      self,
  ) -> None:
    reload_calls: list[tuple[int, int, float]] = []

    def fake_reload(
        prose_cols: int, diagram_cols: int, zoom: float
    ) -> mdview.RenderedDocument:
      reload_calls.append((prose_cols, diagram_cols, zoom))
      return self.doc

    ctrl = mdview_tui.ViewerController(
        filepath=self.md_path,
        document=self.doc,
        reviews_dir=self.temp_dir,
        term_cols=200,
        term_rows=50,
        reload_fn=fake_reload,
    )
    ctrl.resize(150, 38)
    self.assertEqual(reload_calls[-1], (120, 141, 1.0))
    ctrl.resize(150, 32)
    self.assertEqual(len(reload_calls), 2)

  def test_resolve_comment_editor_cmd_prefers_emacs_client_sh(self) -> None:
    fake_script = os.path.join(self.temp_dir, "emacs-client.sh")
    with open(fake_script, "w", encoding="utf-8") as f:
      f.write("#!/usr/bin/env bash\nexit 0\n")
    os.chmod(fake_script, 0o755)

    with mock.patch.dict(os.environ, {}, clear=False):
      os.environ.pop("MDVIEW_COMMENT_EDITOR", None)
      cmd = mdview_tui.resolve_comment_editor_cmd(
          emacs_client_script=fake_script
      )
    self.assertEqual(cmd, [fake_script])

    with mock.patch.dict(
        os.environ, {"MDVIEW_COMMENT_EDITOR": "nano -w"}, clear=False
    ):
      override_cmd = mdview_tui.resolve_comment_editor_cmd(
          emacs_client_script=fake_script
      )
    self.assertEqual(override_cmd, ["nano", "-w"])

  def test_resolve_real_tty_path_prefers_pts_device_over_dev_tty(self) -> None:
    with (
        mock.patch("os.isatty", side_effect=lambda fd: fd == 1),
        mock.patch("os.ttyname", return_value="/dev/pts/42"),
    ):
      self.assertEqual(mdview_tui.resolve_real_tty_path(), "/dev/pts/42")

    with (
        mock.patch("os.isatty", return_value=True),
        mock.patch("os.ttyname", return_value="/dev/tty"),
    ):
      self.assertEqual(mdview_tui.resolve_real_tty_path(), "/dev/tty")

  def test_diagram_span_shows_bubble_and_count_badge_for_raw_mode_comments(
      self,
  ) -> None:
    ctrl = mdview_tui.ViewerController(
        filepath=self.md_path,
        document=self.doc,
        reviews_dir=self.temp_dir,
        term_cols=100,
        term_rows=20,
    )
    # Place 1 comment on interior Mermaid line L6 (inside L5..L8 span).
    ctrl.state.comments[6] = "Rename node A"
    ctrl.state.cursor_row = 3
    frame_one = mdview_source_map.strip_terminal_escapes(ctrl.render_frame())
    self.assertIn("●▸  5┃", frame_one)
    self.assertIn("L6: Rename node A", frame_one)

    # Add a second interior comment on L7 -> gutter shows count '2' and summary.
    ctrl.state.comments[7] = "Check edge B"
    frame_two = mdview_source_map.strip_terminal_escapes(ctrl.render_frame())
    self.assertIn("2▸  5┃", frame_two)
    self.assertIn("2 cmts", frame_two)
    self.assertIn("L6: Rename node A", frame_two)
    self.assertIn("L7: Check edge B", frame_two)

  def test_drawer_jumps_to_diagram_row_and_edits_raw_interior_comment(
      self,
  ) -> None:
    ctrl = mdview_tui.ViewerController(
        filepath=self.md_path,
        document=self.doc,
        reviews_dir=self.temp_dir,
        term_cols=100,
        term_rows=20,
    )
    ctrl.state.comments[7] = "Initial node note"
    # Open Comments Drawer and press Enter -> lands on diagram row 3 (not EOF).
    ctrl.handle_key("C")
    self.assertEqual(ctrl.mode, mdview_tui.InputMode.DRAWER)
    ctrl.handle_key("\r")
    self.assertEqual(ctrl.mode, mdview_tui.InputMode.NORMAL)
    self.assertEqual(ctrl.state.cursor_row, 3)

    # Re-open Drawer and press 'e' -> edits L7 inline directly!
    ctrl.handle_key("C")
    ctrl.handle_key("e")
    self.assertEqual(ctrl.mode, mdview_tui.InputMode.COMMENT)
    self.assertEqual(ctrl.comment_target_line, 7)
    self.assertEqual(ctrl.input_buffer, "Initial node note")
    ctrl.handle_key("\x15")  # Ctrl+U clear
    for ch in "Updated node note":
      ctrl.handle_key(ch)
    ctrl.handle_key("\r")
    self.assertEqual(ctrl.state.comments[7], "Updated node note")

    # Re-open Drawer and press 'r' -> switches to raw mode directly on L7!
    ctrl.handle_key("C")
    ctrl.handle_key("r")
    self.assertTrue(ctrl.raw_mode)
    self.assertEqual(ctrl.current_source_line(), 7)

  def test_tab_comment_jump_does_not_loop_inside_same_diagram_span(self) -> None:
    ctrl = mdview_tui.ViewerController(
        filepath=self.md_path,
        document=self.doc,
        reviews_dir=self.temp_dir,
        term_cols=100,
        term_rows=20,
    )
    ctrl.state.comments[3] = "Prose note"
    ctrl.state.comments[6] = "Node A note"
    ctrl.state.comments[7] = "Node B note"
    ctrl.state.cursor_row = 2  # On L3
    ctrl.handle_key("\t")
    self.assertEqual(ctrl.state.cursor_row, 3)  # Jumped to diagram span L5..L8
    ctrl.handle_key("\t")
    self.assertEqual(ctrl.state.cursor_row, 2)  # Wrapped cleanly back to L3!

  def test_ctrl_g_in_normal_mode_opens_external_emacs_editor(self) -> None:
    calls: list[tuple[str, int]] = []

    def fake_editor(initial: str, line_no: int) -> str:
      calls.append((initial, line_no))
      return "Edited via Ctrl+G"

    ctrl = mdview_tui.ViewerController(
        filepath=self.md_path,
        document=self.doc,
        reviews_dir=self.temp_dir,
        term_cols=100,
        term_rows=20,
        external_editor_fn=fake_editor,
    )
    ctrl.handle_key("j")
    ctrl.handle_key("j")  # L3
    ctrl.handle_key("\x07")  # Ctrl+G directly from NORMAL mode
    self.assertEqual(calls, [("", 3)])
    self.assertEqual(ctrl.state.comments[3], "Edited via Ctrl+G")

  def test_help_overlay_toggles_on_question_mark_and_h_and_lists_shortcuts(
      self,
  ) -> None:
    ctrl = mdview_tui.ViewerController(
        filepath=self.md_path,
        document=self.doc,
        reviews_dir=self.temp_dir,
        term_cols=100,
        term_rows=26,
    )
    ctrl.consume_pending_image_uploads()
    ctrl.handle_key("?")
    self.assertEqual(ctrl.mode, mdview_tui.InputMode.HELP)
    help_plain = mdview_source_map.strip_terminal_escapes(ctrl.render_frame())
    self.assertIn("Shortcuts", help_plain)
    self.assertIn("emacs-client.sh", help_plain)
    self.assertIn("Delete comment", help_plain)

    # Dismissing help restores NORMAL mode and triggers image re-upload.
    ctrl.handle_key("ESC")
    self.assertEqual(ctrl.mode, mdview_tui.InputMode.NORMAL)
    self.assertTrue(ctrl.needs_image_reupload)

    # 'h' also opens help and 'h' closes it.
    ctrl.handle_key("h")
    self.assertEqual(ctrl.mode, mdview_tui.InputMode.HELP)
    ctrl.handle_key("h")
    self.assertEqual(ctrl.mode, mdview_tui.InputMode.NORMAL)

  def test_status_bar_and_comment_preview_show_delete_and_help_hints(
      self,
  ) -> None:
    ctrl = mdview_tui.ViewerController(
        filepath=self.md_path,
        document=self.doc,
        reviews_dir=self.temp_dir,
        term_cols=120,
        term_rows=20,
    )
    ctrl.state.comments[3] = "Tighten wording"
    ctrl.state.cursor_row = 2
    plain = mdview_source_map.strip_terminal_escapes(ctrl.render_frame())
    self.assertIn("x:del", plain)
    self.assertIn("?:help", plain)


if __name__ == "__main__":
  unittest.main()
