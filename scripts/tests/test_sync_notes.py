#!/usr/bin/env python3
"""Comprehensive unit tests for sync_notes_engine.py"""

import os
import shutil
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.expanduser("~/scripts"))
from sync_notes_engine import SyncEngine, sha256_file


class TestSyncNotesEngine(unittest.TestCase):

  def setUp(self):
    self.test_dir = tempfile.mkdtemp(prefix="test_sync_notes_")
    self.input_dir = os.path.join(self.test_dir, "notes")
    self.output_dir = os.path.join(self.test_dir, "company")
    self.state_dir = os.path.join(self.test_dir, "state")
    os.makedirs(self.input_dir, exist_ok=True)
    os.makedirs(self.output_dir, exist_ok=True)
    os.makedirs(self.state_dir, exist_ok=True)

  def tearDown(self):
    shutil.rmtree(self.test_dir, ignore_errors=True)

  def _make_engine(self, dry_run=False):
    return SyncEngine(
        input_dir=self.input_dir,
        output_dir=self.output_dir,
        state_dir=self.state_dir,
        dry_run=dry_run,
        skip_vcs=True,
        skip_submit=True,
    )

  def test_initial_sync_local_to_remote(self):
    with open(os.path.join(self.input_dir, "index.md"), "w") as f:
      f.write("# Index\nHello world\n")

    engine = self._make_engine()
    stats = engine.sync()

    self.assertEqual(stats["remote_updated"], 1)
    remote_index = os.path.join(self.output_dir, "index.md")
    self.assertTrue(os.path.exists(remote_index))
    with open(remote_index) as f:
      self.assertEqual(f.read(), "# Index\nHello world\n")

  def test_local_modified_only(self):
    local_file = os.path.join(self.input_dir, "linux_tips.md")
    with open(local_file, "w") as f:
      f.write("Line 1\nLine 2\n")
    engine = self._make_engine()
    engine.sync()

    with open(local_file, "a") as f:
      f.write("Line 3 local\n")

    engine = self._make_engine()
    stats = engine.sync()

    self.assertEqual(stats["remote_updated"], 1)
    remote_file = os.path.join(self.output_dir, "linux_tips.md")
    with open(remote_file) as f:
      self.assertEqual(f.read(), "Line 1\nLine 2\nLine 3 local\n")

  def test_remote_modified_only(self):
    local_file = os.path.join(self.input_dir, "linux_tips.md")
    with open(local_file, "w") as f:
      f.write("Line 1\nLine 2\n")
    engine = self._make_engine()
    engine.sync()

    remote_file = os.path.join(self.output_dir, "linux_tips.md")
    with open(remote_file, "a") as f:
      f.write("Line 3 remote\n")

    engine = self._make_engine()
    stats = engine.sync()

    self.assertEqual(stats["local_updated"], 1)
    with open(local_file) as f:
      self.assertEqual(f.read(), "Line 1\nLine 2\nLine 3 remote\n")

  def test_both_modified_clean_merge(self):
    local_file = os.path.join(self.input_dir, "doc.md")
    with open(local_file, "w") as f:
      f.write("Top line\n\nMiddle line\n\nBottom line\n")
    engine = self._make_engine()
    engine.sync()

    with open(local_file, "w") as f:
      f.write("Top line LOCAL EDIT\n\nMiddle line\n\nBottom line\n")

    remote_file = os.path.join(self.output_dir, "doc.md")
    with open(remote_file, "w") as f:
      f.write("Top line\n\nMiddle line\n\nBottom line REMOTE EDIT\n")

    engine = self._make_engine()
    stats = engine.sync()

    self.assertEqual(stats["merged_clean"], 1)
    self.assertEqual(stats["conflicts"], 0)

    expected = "Top line LOCAL EDIT\n\nMiddle line\n\nBottom line REMOTE EDIT\n"
    with open(local_file) as f:
      self.assertEqual(f.read(), expected)
    with open(remote_file) as f:
      self.assertEqual(f.read(), expected)

  def test_both_modified_conflict(self):
    local_file = os.path.join(self.input_dir, "conflict.md")
    with open(local_file, "w") as f:
      f.write("Original content\n")
    engine = self._make_engine()
    engine.sync()

    with open(local_file, "w") as f:
      f.write("Local change\n")

    remote_file = os.path.join(self.output_dir, "conflict.md")
    with open(remote_file, "w") as f:
      f.write("Remote change\n")

    engine = self._make_engine()
    stats = engine.sync()

    self.assertEqual(stats["conflicts"], 1)
    with open(local_file) as f:
      content = f.read()
      self.assertIn("<<<<<<<", content)
      self.assertIn("Local change", content)
      self.assertIn("Remote change", content)

  def test_new_remote_file_downloaded(self):
    remote_file = os.path.join(self.output_dir, "jetski_cli_notify.md")
    with open(remote_file, "w") as f:
      f.write("# Jetski CLI\nNotification tool docs.\n")

    engine = self._make_engine()
    stats = engine.sync()

    self.assertEqual(stats["local_updated"], 1)
    local_file = os.path.join(self.input_dir, "jetski_cli_notify.md")
    self.assertTrue(os.path.exists(local_file))
    with open(local_file) as f:
      self.assertEqual(f.read(), "# Jetski CLI\nNotification tool docs.\n")

  def test_subdirectories_archive_and_asrf(self):
    os.makedirs(os.path.join(self.input_dir, "archive"), exist_ok=True)
    os.makedirs(os.path.join(self.input_dir, "asrf"), exist_ok=True)

    with open(os.path.join(self.input_dir, "archive", "note1.md"), "w") as f:
      f.write("Historical note 1\n")
    with open(os.path.join(self.input_dir, "asrf", "CURRENT_PLAN.md"), "w") as f:
      f.write("ASRF current plan\n")

    engine = self._make_engine()
    stats = engine.sync()

    self.assertEqual(stats["remote_updated"], 2)
    self.assertTrue(os.path.exists(os.path.join(self.output_dir, "archive", "note1.md")))
    self.assertTrue(os.path.exists(os.path.join(self.output_dir, "asrf", "CURRENT_PLAN.md")))

    with open(os.path.join(self.output_dir, "asrf", "DESIGN_DOC.md"), "w") as f:
      f.write("ASRF design doc\n")

    engine = self._make_engine()
    stats = engine.sync()

    self.assertEqual(stats["local_updated"], 1)
    self.assertTrue(os.path.exists(os.path.join(self.input_dir, "asrf", "DESIGN_DOC.md")))

  def test_excluded_files_and_directories(self):
    with open(os.path.join(self.input_dir, "TODO.org"), "w") as f:
      f.write("* TODO Secret tasks\n")
    with open(os.path.join(self.input_dir, "contacts.md"), "w") as f:
      f.write("Private contacts\n")

    os.makedirs(os.path.join(self.input_dir, "personal"), exist_ok=True)
    with open(os.path.join(self.input_dir, "personal", "finance.md"), "w") as f:
      f.write("Personal finance\n")
    os.makedirs(os.path.join(self.input_dir, "interviews"), exist_ok=True)
    with open(os.path.join(self.input_dir, "interviews", "candidate.md"), "w") as f:
      f.write("Candidate notes\n")

    with open(os.path.join(self.input_dir, ".#active.md"), "w") as f:
      f.write("lock file\n")
    with open(os.path.join(self.input_dir, "#active.md#"), "w") as f:
      f.write("autosave\n")

    with open(os.path.join(self.input_dir, "valid.md"), "w") as f:
      f.write("Valid note\n")

    engine = self._make_engine()
    stats = engine.sync()

    self.assertEqual(stats["remote_updated"], 1)
    self.assertTrue(os.path.exists(os.path.join(self.output_dir, "valid.md")))
    self.assertFalse(os.path.exists(os.path.join(self.output_dir, "TODO.org")))
    self.assertFalse(os.path.exists(os.path.join(self.output_dir, "contacts.md")))
    self.assertFalse(os.path.exists(os.path.join(self.output_dir, "personal")))
    self.assertFalse(os.path.exists(os.path.join(self.output_dir, "interviews")))

  def test_delete_local_removes_remote(self):
    file_path = os.path.join(self.input_dir, "temp.md")
    with open(file_path, "w") as f:
      f.write("Temporary note\n")
    engine = self._make_engine()
    engine.sync()

    self.assertTrue(os.path.exists(os.path.join(self.output_dir, "temp.md")))

    os.remove(file_path)
    engine = self._make_engine()
    stats = engine.sync()

    self.assertEqual(stats["remote_deleted"], 1)
    self.assertFalse(os.path.exists(os.path.join(self.output_dir, "temp.md")))

  def test_dry_run_preserves_everything(self):
    with open(os.path.join(self.input_dir, "dry.md"), "w") as f:
      f.write("Dry run content\n")

    engine = self._make_engine(dry_run=True)
    stats = engine.sync()

    self.assertEqual(stats["remote_updated"], 1)
    self.assertFalse(os.path.exists(os.path.join(self.output_dir, "dry.md")))
    self.assertFalse(os.path.exists(os.path.join(self.state_dir, "state.json")))

  def test_exclude_personal_calibration_and_perf_notes(self):
    os.makedirs(os.path.join(self.input_dir, "personal"), exist_ok=True)
    os.makedirs(os.path.join(self.input_dir, "personal_projects"), exist_ok=True)
    os.makedirs(os.path.join(self.input_dir, "archive"), exist_ok=True)
    os.makedirs(os.path.join(self.input_dir, "team"), exist_ok=True)

    test_files = {
        "personal/secret_plan.md": "Personal plan",
        "personal_projects/tasks.md": "Personal tasks",
        "archive/perf_and_callibrations_knowledge.md": "Ratings and feedback",
        "archive/expectations_ddraft_2023.md": "Draft expectations",
        "draft_calibration_notes_q4_2020": "Calibration notes",
        "team/calibration_summary.md": "Team calibration summary",
        "team/callibration_notes.md": "Callibration notes",
        "ali_one_on_one.md": "1-on-1 with Ali",
        "team/1-on-1_sync.md": "1-on-1 sync note",
        "team/one_on_one_meeting.md": "One on one meeting note",
        "perf_review_2025.md": "Performance review 2025",
        "grad_review_summary.md": "Grad review summary",
        "peak_performance.md": "Peak performance notes",
        "valid_tech_doc.md": "Valid technical documentation",
        "archive/release_workflows.md": "Release workflows note",
    }

    for rel_path, content in test_files.items():
      full_path = os.path.join(self.input_dir, rel_path)
      os.makedirs(os.path.dirname(full_path), exist_ok=True)
      with open(full_path, "w") as f:
        f.write(content)

    engine = self._make_engine()
    stats = engine.sync()

    self.assertEqual(stats["remote_updated"], 2)
    self.assertTrue(
        os.path.exists(os.path.join(self.output_dir, "valid_tech_doc.md"))
    )
    self.assertTrue(
        os.path.exists(
            os.path.join(self.output_dir, "archive", "release_workflows.md")
        )
    )

    # Ensure none of the personal/calibration/1-on-1 files leaked to company docs
    self.assertFalse(os.path.exists(os.path.join(self.output_dir, "personal")))
    self.assertFalse(
        os.path.exists(os.path.join(self.output_dir, "personal_projects"))
    )
    self.assertFalse(
        os.path.exists(
            os.path.join(
                self.output_dir,
                "archive",
                "perf_and_callibrations_knowledge.md",
            )
        )
    )
    self.assertFalse(
        os.path.exists(
            os.path.join(
                self.output_dir, "archive", "expectations_ddraft_2023.md"
            )
        )
    )
    self.assertFalse(
        os.path.exists(
            os.path.join(self.output_dir, "draft_calibration_notes_q4_2020")
        )
    )
    self.assertFalse(
        os.path.exists(
            os.path.join(self.output_dir, "team", "calibration_summary.md")
        )
    )
    self.assertFalse(
        os.path.exists(
            os.path.join(self.output_dir, "team", "callibration_notes.md")
        )
    )
    self.assertFalse(
        os.path.exists(os.path.join(self.output_dir, "ali_one_on_one.md"))
    )
    self.assertFalse(
        os.path.exists(
            os.path.join(self.output_dir, "team", "1-on-1_sync.md")
        )
    )
    self.assertFalse(
        os.path.exists(
            os.path.join(self.output_dir, "team", "one_on_one_meeting.md")
        )
    )
    self.assertFalse(
        os.path.exists(os.path.join(self.output_dir, "perf_review_2025.md"))
    )
    self.assertFalse(
        os.path.exists(os.path.join(self.output_dir, "grad_review_summary.md"))
    )
    self.assertFalse(
        os.path.exists(os.path.join(self.output_dir, "peak_performance.md"))
    )

  def test_remote_ignored_calibration_files_not_downloaded(self):
    os.makedirs(os.path.join(self.output_dir, "archive"), exist_ok=True)
    with open(
        os.path.join(
            self.output_dir, "archive", "perf_and_callibrations_knowledge.md"
        ),
        "w",
    ) as f:
      f.write("Remote calibration doc\n")
    with open(
        os.path.join(self.output_dir, "remote_tech_guide.md"), "w"
    ) as f:
      f.write("Remote tech guide\n")

    engine = self._make_engine()
    stats = engine.sync()

    self.assertEqual(stats["local_updated"], 1)
    self.assertTrue(
        os.path.exists(os.path.join(self.input_dir, "remote_tech_guide.md"))
    )
    self.assertFalse(
        os.path.exists(
            os.path.join(
                self.input_dir,
                "archive",
                "perf_and_callibrations_knowledge.md",
            )
        )
    )

  def test_stale_state_purged_for_newly_ignored_calibration_files(self):
    os.makedirs(os.path.join(self.input_dir, "archive"), exist_ok=True)
    calib_file = os.path.join(
        self.input_dir, "archive", "perf_and_callibrations_knowledge.md"
    )
    with open(calib_file, "w") as f:
        f.write("Calibration content\n")

    engine = self._make_engine()
    engine.state["archive/perf_and_callibrations_knowledge.md"] = {
        "hash": sha256_file(calib_file)
    }
    engine._save_state()

    stats = engine.sync()
    # File in local notes remains untouched, but state entry is purged
    self.assertTrue(os.path.exists(calib_file))
    self.assertNotIn(
        "archive/perf_and_callibrations_knowledge.md", engine.state
    )

  def test_twelve_project_folders_sync_allowed(self):
    """Explicitly tests that all 12 project folders (plus archive and root files)

    are synced without restriction, while meetings*, personal*, interviews*, and
    hidden dot-dirs (.*) are strictly excluded.
    """
    twelve_folders = [
        "agentic_containment",
        "asci_app_offboarding",
        "avid_querylog_staleness",
        "avid_robust_regression_detection",
        "avid_tdp_datastore_monitoring",
        "diagrams",
        "iba_load_test_denoise",
        "iba_loadtest_report",
        "misc_plans",
        "prompts",
        "stochastic_querydiff",
        "supermixer_half_cell_test",
    ]

    # Create test notes in all 12 project folders
    for folder in twelve_folders:
      folder_dir = os.path.join(self.input_dir, folder)
      os.makedirs(folder_dir, exist_ok=True)
      with open(os.path.join(folder_dir, "README.md"), "w") as f:
        f.write(f"# {folder} Documentation\nProject content.\n")

    # Create test notes in archive and root
    os.makedirs(os.path.join(self.input_dir, "archive"), exist_ok=True)
    with open(os.path.join(self.input_dir, "archive", "archived_note.md"), "w") as f:
      f.write("# Archive Note\nHistorical content.\n")
    with open(os.path.join(self.input_dir, "root_overview.md"), "w") as f:
      f.write("# Root Overview\nGeneral doc.\n")

    # Create excluded folders and files matching meetings*, personal*, interviews*, and .*
    excluded_dirs = [
        "meetings",
        "meetings_2026",
        "meetings.weekly",
        "personal",
        "personal_finance",
        "personal.records",
        "interviews",
        "interviews_swe",
        "interviews.2026",
        ".git",
        ".hg",
        ".citc",
        ".snapshot",
        ".jetski",
        ".lumbergh",
        ".sync_state",
        ".hidden_project",
    ]
    for ex_dir in excluded_dirs:
      p = os.path.join(self.input_dir, ex_dir)
      os.makedirs(p, exist_ok=True)
      with open(os.path.join(p, "notes.md"), "w") as f:
        f.write(f"# Excluded from {ex_dir}\n")

    # Excluded files in allowed folders
    with open(os.path.join(self.input_dir, "misc_plans", ".#temp_lock.md"), "w") as f:
      f.write("lock")
    with open(os.path.join(self.input_dir, "prompts", "#autosave.md#"), "w") as f:
      f.write("autosave")
    with open(os.path.join(self.input_dir, "diagrams", ".diagram.dot.~undo-tree~"), "w") as f:
      f.write("undo-tree")

    engine = self._make_engine()
    stats = engine.sync()

    # 12 project folders + archive + root file = 14 files synced
    self.assertEqual(stats["remote_updated"], 14)

    # Validate that all 12 project folders exist in remote company docs
    for folder in twelve_folders:
      remote_doc = os.path.join(self.output_dir, folder, "README.md")
      self.assertTrue(
          os.path.exists(remote_doc),
          f"Expected {remote_doc} to be synced to company docs",
      )
      with open(remote_doc) as f:
        self.assertEqual(
            f.read(), f"# {folder} Documentation\nProject content.\n"
        )

    # Validate archive and root file
    self.assertTrue(
        os.path.exists(os.path.join(self.output_dir, "archive", "archived_note.md"))
    )
    self.assertTrue(
        os.path.exists(os.path.join(self.output_dir, "root_overview.md"))
    )

    # Validate that none of the excluded directories or files leaked to company docs
    for ex_dir in excluded_dirs:
      remote_ex = os.path.join(self.output_dir, ex_dir)
      self.assertFalse(
          os.path.exists(remote_ex),
          f"Excluded directory {ex_dir} must not exist in company docs",
      )

    # Validate temporary/lock files did not sync
    self.assertFalse(
        os.path.exists(
            os.path.join(self.output_dir, "misc_plans", ".#temp_lock.md")
        )
    )
    self.assertFalse(
        os.path.exists(
            os.path.join(self.output_dir, "prompts", "#autosave.md#")
        )
    )
    self.assertFalse(
        os.path.exists(
            os.path.join(
                self.output_dir, "diagrams", ".diagram.dot.~undo-tree~"
            )
        )
    )

  def test_twelve_project_folders_bidirectional_and_concurrent_sync(self):
    """Validates bidirectional sync, clean merges, and deletion across the 12 folders."""
    # Initial setup
    os.makedirs(os.path.join(self.input_dir, "prompts"), exist_ok=True)
    os.makedirs(os.path.join(self.input_dir, "agentic_containment"), exist_ok=True)
    os.makedirs(os.path.join(self.input_dir, "stochastic_querydiff"), exist_ok=True)

    with open(os.path.join(self.input_dir, "prompts", "prompt1.md"), "w") as f:
      f.write("Line A\nLine B\nLine C\n")
    with open(os.path.join(self.input_dir, "agentic_containment", "state.md"), "w") as f:
      f.write("Initial state\n")
    with open(os.path.join(self.input_dir, "stochastic_querydiff", "plan.md"), "w") as f:
      f.write("Initial plan\n")

    engine = self._make_engine()
    engine.sync()

    # 1. New remote doc in misc_plans downloaded locally
    os.makedirs(os.path.join(self.output_dir, "misc_plans"), exist_ok=True)
    with open(os.path.join(self.output_dir, "misc_plans", "remote_plan.md"), "w") as f:
      f.write("# Remote Plan\nCreated from critique review.\n")

    # 2. Concurrent clean edit in prompts/prompt1.md
    with open(os.path.join(self.input_dir, "prompts", "prompt1.md"), "w") as f:
      f.write("Line A LOCAL\nLine B\nLine C\n")
    with open(os.path.join(self.output_dir, "prompts", "prompt1.md"), "w") as f:
      f.write("Line A\nLine B\nLine C REMOTE\n")

    # 3. Local deletion in agentic_containment/state.md
    os.remove(os.path.join(self.input_dir, "agentic_containment", "state.md"))

    engine = self._make_engine()
    stats = engine.sync()

    self.assertEqual(stats["local_updated"], 1)  # remote_plan.md downloaded
    self.assertEqual(stats["merged_clean"], 1)   # prompt1.md merged cleanly
    self.assertEqual(stats["remote_deleted"], 1) # state.md removed remotely

    # Assert downloaded file exists locally
    self.assertTrue(
        os.path.exists(os.path.join(self.input_dir, "misc_plans", "remote_plan.md"))
    )

    # Assert 3-way merge result
    with open(os.path.join(self.input_dir, "prompts", "prompt1.md")) as f:
      self.assertEqual(f.read(), "Line A LOCAL\nLine B\nLine C REMOTE\n")

    # Assert remote deletion occurred
    self.assertFalse(
        os.path.exists(
            os.path.join(self.output_dir, "agentic_containment", "state.md")
        )
    )

  def test_deeply_nested_subdirectories_in_twelve_folders(self):
    """Tests 3+ levels deep nesting and verifies deep exclusions."""
    deep_allowed = [
        "prompts/level1/level2/level3/prompt.md",
        "agentic_containment/phase1/milestone2/task3/state.md",
        "asci_app_offboarding/arch/v1/sub/spec.md",
        "diagrams/2026/q1/flowchart.png",
        "misc_plans/roadmap/infra/storage/plan.md",
    ]
    for rel in deep_allowed:
      p = os.path.join(self.input_dir, rel)
      os.makedirs(os.path.dirname(p), exist_ok=True)
      with open(p, "w") as f:
        f.write(f"# Deep content for {rel}\n")

    deep_excluded = [
        "prompts/level1/level2/meetings_notes/note.md",
        "agentic_containment/phase1/personal_sandbox/test.md",
        "diagrams/2026/interviews_sys_design/diagram.png",
        "iba_loadtest_report/level1/level2/.hidden_sub/secret.md",
        "misc_plans/roadmap/.jetski/config.json",
    ]
    for rel in deep_excluded:
      p = os.path.join(self.input_dir, rel)
      os.makedirs(os.path.dirname(p), exist_ok=True)
      with open(p, "w") as f:
        f.write(f"# Excluded deep content for {rel}\n")

    engine = self._make_engine()
    stats = engine.sync()

    self.assertEqual(stats["remote_updated"], 5)

    for rel in deep_allowed:
      self.assertTrue(
          os.path.exists(os.path.join(self.output_dir, rel)),
          f"Deeply nested file {rel} must sync to company docs",
      )

    for rel in deep_excluded:
      self.assertFalse(
          os.path.exists(os.path.join(self.output_dir, rel)),
          f"Deeply nested excluded file {rel} must NOT sync to company docs",
      )

  def test_binary_image_sync_and_conflict_safety(self):
    """Tests binary image files sync and conflict handling without text corruption."""
    png_bytes_a = b"\x89PNG\r\n\x1a\n\x00\x00\x00\rIHDR\x00\x00\x00\x01\x00\x00\x00\x01\x08\x06\x00\x00\x00\x1f\x15c4"
    png_bytes_b = b"\x89PNG\r\n\x1a\n\x00\x00\x00\rIHDR\x00\x00\x00\x02\x00\x00\x00\x02\x08\x06\x00\x00\x00\x1f\x15c4"
    png_bytes_c = b"\x89PNG\r\n\x1a\n\x00\x00\x00\rIHDR\x00\x00\x00\x03\x00\x00\x00\x03\x08\x06\x00\x00\x00\x1f\x15c4"

    diag_png = os.path.join(self.input_dir, "diagrams", "arch.png")
    os.makedirs(os.path.dirname(diag_png), exist_ok=True)
    with open(diag_png, "wb") as f:
      f.write(png_bytes_a)

    engine = self._make_engine()
    stats = engine.sync()

    self.assertEqual(stats["remote_updated"], 1)
    remote_diag_png = os.path.join(self.output_dir, "diagrams", "arch.png")
    self.assertTrue(os.path.exists(remote_diag_png))
    with open(remote_diag_png, "rb") as f:
      self.assertEqual(f.read(), png_bytes_a)

    # Concurrently modify both sides with different binary bytes
    with open(diag_png, "wb") as f:
      f.write(png_bytes_b)
    with open(remote_diag_png, "wb") as f:
      f.write(png_bytes_c)

    engine = self._make_engine()
    stats = engine.sync()

    # Binary conflict detected without corrupting files
    self.assertEqual(stats["conflicts"], 1)
    with open(diag_png, "rb") as f:
      self.assertEqual(f.read(), png_bytes_b)
    with open(remote_diag_png, "rb") as f:
      self.assertEqual(f.read(), png_bytes_c)

  def test_broken_symlinks_and_emacs_lock_files_ignored(self):
    """Tests that broken symlinks and Emacs lock files do not crash the engine."""
    folder = os.path.join(self.input_dir, "avid_robust_regression_detection")
    os.makedirs(folder, exist_ok=True)

    with open(os.path.join(folder, "real_note.md"), "w") as f:
      f.write("# Real note\n")

    # Create Emacs lock file symlink pointing to non-existent host/pid
    try:
      os.symlink(
          "user@host.12345:999999",
          os.path.join(folder, ".#real_note.md"),
      )
      os.symlink(
          "nonexistent_target.md",
          os.path.join(folder, "broken_link.md"),
      )
    except OSError:
      pass

    engine = self._make_engine()
    stats = engine.sync()

    self.assertEqual(stats["remote_updated"], 1)
    self.assertTrue(
        os.path.exists(
            os.path.join(
                self.output_dir,
                "avid_robust_regression_detection",
                "real_note.md",
            )
        )
    )

  def test_all_twelve_folders_with_diverse_allowed_extensions(self):
    """Tests all 12 project folders with .md, .png, .jpg, .svg, and METADATA files."""
    twelve_folders = [
        "agentic_containment",
        "asci_app_offboarding",
        "avid_querylog_staleness",
        "avid_robust_regression_detection",
        "avid_tdp_datastore_monitoring",
        "diagrams",
        "iba_load_test_denoise",
        "iba_loadtest_report",
        "misc_plans",
        "prompts",
        "stochastic_querydiff",
        "supermixer_half_cell_test",
    ]
    file_types = [
        ("doc.md", "text", "# Markdown\n"),
        ("image.png", "bytes", b"\x89PNG\r\n\x1a\n\x00"),
        ("photo.jpg", "bytes", b"\xff\xd8\xff\xe0\x00"),
        ("vector.svg", "text", "<svg></svg>\n"),
        ("METADATA", "text", "owner: 'marcelvaldez'\n"),
    ]

    total_created = 0
    for folder in twelve_folders:
      folder_dir = os.path.join(self.input_dir, folder)
      os.makedirs(folder_dir, exist_ok=True)
      for fname, mode, content in file_types:
        fpath = os.path.join(folder_dir, fname)
        if mode == "text":
          with open(fpath, "w") as f:
            f.write(content)
        else:
          with open(fpath, "wb") as f:
            f.write(content)
        total_created += 1

    engine = self._make_engine()
    stats = engine.sync()

    self.assertEqual(stats["remote_updated"], total_created)

    for folder in twelve_folders:
      for fname, mode, content in file_types:
        remote_path = os.path.join(self.output_dir, folder, fname)
        self.assertTrue(
            os.path.exists(remote_path),
            f"Expected {remote_path} to exist in company docs",
        )

  def test_syncnotesignore_file_loading(self):
    """Verifies that .syncnotesignore in input_dir is loaded and respected."""
    ignore_file = os.path.join(self.input_dir, ".syncnotesignore")
    with open(ignore_file, "w") as f:
      f.write("# Custom syncnotesignore\n")
      f.write("custom_secret.md\n")
      f.write("secret_dir/\n")
      f.write("secret_pattern_*\n")
      f.write("valid_included.md\n") # Will exclude this exact file name

    with open(os.path.join(self.input_dir, "custom_secret.md"), "w") as f:
      f.write("# Secret\n")
    with open(os.path.join(self.input_dir, "normal_note.md"), "w") as f:
      f.write("# Normal\n")

    secret_dir = os.path.join(self.input_dir, "secret_dir")
    os.makedirs(secret_dir, exist_ok=True)
    with open(os.path.join(secret_dir, "hidden.md"), "w") as f:
      f.write("# Hidden\n")

    pattern_dir = os.path.join(self.input_dir, "secret_pattern_alpha")
    os.makedirs(pattern_dir, exist_ok=True)
    with open(os.path.join(pattern_dir, "sub.md"), "w") as f:
      f.write("# Sub\n")

    engine = self._make_engine()
    stats = engine.sync()

    self.assertEqual(stats["remote_updated"], 1)
    self.assertTrue(os.path.exists(os.path.join(self.output_dir, "normal_note.md")))
    self.assertFalse(os.path.exists(os.path.join(self.output_dir, "custom_secret.md")))
    self.assertFalse(os.path.exists(os.path.join(self.output_dir, "secret_dir")))
    self.assertFalse(os.path.exists(os.path.join(self.output_dir, "secret_pattern_alpha")))

  def test_syncnotesignore_custom_flag(self):
    """Verifies that custom ignore_file path can be passed to SyncEngine."""
    custom_ignore = os.path.join(self.test_dir, "custom_ignore.txt")
    with open(custom_ignore, "w") as f:
      f.write("my_excluded_doc.md\n")

    with open(os.path.join(self.input_dir, "my_excluded_doc.md"), "w") as f:
      f.write("# Excluded\n")
    with open(os.path.join(self.input_dir, "my_published_doc.md"), "w") as f:
      f.write("# Published\n")

    engine = SyncEngine(
        input_dir=self.input_dir,
        output_dir=self.output_dir,
        state_dir=self.state_dir,
        skip_vcs=True,
        skip_submit=True,
        ignore_file=custom_ignore,
    )
    stats = engine.sync()

    self.assertEqual(stats["remote_updated"], 1)
    self.assertTrue(os.path.exists(os.path.join(self.output_dir, "my_published_doc.md")))
    self.assertFalse(os.path.exists(os.path.join(self.output_dir, "my_excluded_doc.md")))


if __name__ == "__main__":
  unittest.main()

