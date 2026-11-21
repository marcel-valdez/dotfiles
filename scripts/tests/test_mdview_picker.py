"""Unit tests for Task 3: mdq 60s queue and 4-tier smart Markdown picker."""

from __future__ import annotations

import os
import shutil
import sys
import tempfile
import unittest

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


if __name__ == "__main__":
  unittest.main()
