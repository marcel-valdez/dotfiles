"""Unit tests for Task 3: mdq 60s queue and 4-tier smart Markdown picker."""

from __future__ import annotations

import os
import shutil
import sys
import tempfile
import unittest
from unittest import mock

sys.path.insert(0, os.path.expanduser("~/scripts"))

import mdview_picker
import mdview_review_store


class QueueHandoffTests(unittest.TestCase):
  """Granular tests for mdq (mdview --queue) 60-second TTL handoff."""

  def setUp(self) -> None:
    self.temp_dir = tempfile.mkdtemp(prefix="test_mdview_queue_")
    self.queue_file = os.path.join(self.temp_dir, "queued_target.json")
    self.doc_path = os.path.join(self.temp_dir, "guide.md")
    with open(self.doc_path, "w", encoding="utf-8") as f:
      f.write("# Guide\n")

  def tearDown(self) -> None:
    shutil.rmtree(self.temp_dir, ignore_errors=True)

  def test_consumes_fresh_queued_file_once_and_deletes_queue_entry(
      self,
  ) -> None:
    mdview_picker.queue_target_file(
        self.doc_path, queue_path=self.queue_file, now=1000.0
    )
    consumed = mdview_picker.consume_queued_target(
        queue_path=self.queue_file, ttl_seconds=60.0, now=1025.0
    )
    self.assertEqual(consumed, os.path.realpath(self.doc_path))
    second = mdview_picker.consume_queued_target(
        queue_path=self.queue_file, ttl_seconds=60.0, now=1026.0
    )
    self.assertIsNone(second)

  def test_ignores_expired_queued_file_past_ttl(self) -> None:
    mdview_picker.queue_target_file(
        self.doc_path, queue_path=self.queue_file, now=1000.0
    )
    expired = mdview_picker.consume_queued_target(
        queue_path=self.queue_file, ttl_seconds=60.0, now=1065.0
    )
    self.assertIsNone(expired)

  def test_raises_file_not_found_when_queuing_nonexistent_file(self) -> None:
    with self.assertRaises(FileNotFoundError):
      mdview_picker.queue_target_file(
          os.path.join(self.temp_dir, "missing.md"),
          queue_path=self.queue_file,
      )


class CandidateRankingTests(unittest.TestCase):
  """Granular tests for 4-tier candidate discovery and CitC safety."""

  def setUp(self) -> None:
    self.temp_dir = tempfile.mkdtemp(prefix="test_mdview_candidates_")
    self.reviews_dir = os.path.join(self.temp_dir, "reviews")
    self.brain_dir = os.path.join(self.temp_dir, "brain", "conv-1")
    self.workspace_dir = os.path.join(self.temp_dir, "ws")
    self.notes_dir = os.path.join(self.temp_dir, "notes")
    for d in (self.reviews_dir, self.brain_dir, self.workspace_dir, self.notes_dir):
      os.makedirs(d, exist_ok=True)

  def tearDown(self) -> None:
    shutil.rmtree(self.temp_dir, ignore_errors=True)

  def test_ranks_drafts_then_clipboard_then_jetski_then_workspace_without_dups(
      self,
  ) -> None:
    draft_md = os.path.join(self.brain_dir, "draft_plan.md")
    jetski_md = os.path.join(self.brain_dir, "fresh_visual_guide.md")
    clip_md = os.path.join(self.workspace_dir, "clipped.md")
    note_md = os.path.join(self.notes_dir, "todo.md")
    for p in (draft_md, jetski_md, clip_md, note_md):
      with open(p, "w", encoding="utf-8") as f:
        f.write("# Doc\n")

    state, _ = mdview_review_store.load_review_state(
        draft_md, reviews_dir=self.reviews_dir
    )
    state.comments[1] = "Unfinished review note"
    mdview_review_store.save_review_state(state, reviews_dir=self.reviews_dir)

    candidates = mdview_picker.collect_markdown_candidates(
        reviews_dir=self.reviews_dir,
        jetski_brain_root=os.path.join(self.temp_dir, "brain"),
        clipboard_text=f"file://{clip_md}\n",
        workspace_cwd=self.workspace_dir,
        notes_dir=self.notes_dir,
    )
    paths = [c.realpath for c in candidates]
    self.assertEqual(
        paths,
        [
            os.path.realpath(draft_md),
            os.path.realpath(clip_md),
            os.path.realpath(jetski_md),
            os.path.realpath(note_md),
        ],
    )
    self.assertIn("DRAFT: 1 cmt", candidates[0].label)
    self.assertIn("CLIPBOARD", candidates[1].label)
    self.assertIn("JETSKI", candidates[2].label)

  def test_never_walks_citc_filesystem_when_workspace_is_under_google_src(
      self,
  ) -> None:
    self.assertTrue(
        mdview_picker.is_citc_path("/google/src/cloud/user/ws/google3")
    )
    self.assertTrue(mdview_picker.is_citc_path("/cloud/user/ws/google3"))
    self.assertFalse(mdview_picker.is_citc_path(self.workspace_dir))

  def test_active_jetski_session_sets_default_query_and_shows_full_paths(
      self,
  ) -> None:
    session_dir = os.path.join(self.temp_dir, "brain", "conv-active")
    empty_dir = os.path.join(self.temp_dir, "brain", "conv-empty")
    os.makedirs(session_dir, exist_ok=True)
    os.makedirs(empty_dir, exist_ok=True)
    plan_md = os.path.join(session_dir, "plan.md")
    with open(plan_md, "w", encoding="utf-8") as f:
      f.write("# Active Plan\n")

    candidates = mdview_picker.collect_markdown_candidates(
        reviews_dir=self.reviews_dir,
        jetski_brain_root=os.path.join(self.temp_dir, "brain"),
        clipboard_text="",
        workspace_cwd=self.workspace_dir,
        notes_dir=self.notes_dir,
        active_jetski_dir=session_dir,
    )
    self.assertTrue(any(c.realpath == os.path.realpath(plan_md) for c in candidates))
    plan_entry = next(c for c in candidates if c.realpath == os.path.realpath(plan_md))
    self.assertIn("conv-active/plan.md", plan_entry.label)

    query = mdview_picker.resolve_default_picker_query(candidates, session_dir)
    self.assertTrue(query.endswith("conv-active/"), repr(query))
    self.assertEqual(
        mdview_picker.resolve_default_picker_query(candidates, empty_dir), ""
    )

  def test_build_fzf_picker_cmd_includes_wrap_ctrl_slash_and_initial_query(
      self,
  ) -> None:
    cmd = mdview_picker.build_fzf_picker_cmd(
        fzf_bin="/usr/bin/fzf",
        preview_cmd="cat {3}",
        initial_query="~/.gemini/jetski/brain/conv-1/",
    )
    self.assertIn("--wrap=word", cmd)
    self.assertIn("--bind=ctrl-/:toggle-preview", cmd)
    self.assertIn("--bind=alt-w:toggle-wrap", cmd)
    self.assertIn("--query=~/.gemini/jetski/brain/conv-1/", cmd)

  def test_detect_jetski_conversation_from_tty_prioritizes_active_lock(
      self,
  ) -> None:
    fake_ps = mock.Mock(returncode=0, stdout="1001\n")
    uuid_db = "11111111-2222-3333-4444-555555555555"
    uuid_lock = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"

    def fake_readlink(path: str) -> str:
      if path.endswith("/3"):
        return f"/home/u/.gemini/jetski/conversations/{uuid_db}.db"
      if path.endswith("/4"):
        return f"/home/u/.gemini/jetski/presence/{uuid_lock}.lock"
      return "/dev/null"

    with (
        mock.patch("subprocess.run", return_value=fake_ps),
        mock.patch("os.listdir", return_value=["3", "4"]),
        mock.patch("os.readlink", side_effect=fake_readlink),
        mock.patch("os.stat", return_value=mock.Mock(st_mtime=100.0)),
    ):
      conv_id = mdview_picker.detect_jetski_conversation_from_tty("/dev/pts/21")
    self.assertEqual(conv_id, uuid_lock)


if __name__ == "__main__":
  unittest.main()
