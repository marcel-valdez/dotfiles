#!/usr/bin/env python3
"""Bidirectional 3-way sync engine for personal notes and company docs.

Synchronizes local notes (~/notes/) with company docs
(/google/src/cloud/<user>/<workspace>/company/users/<user>/), preserving
subdirectories (archive/, asrf/, etc.), handling pandoc org-to-md conversion,
and performing 3-way merges when files are modified concurrently on both sides.
"""

import argparse
import fnmatch
import hashlib
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
from typing import Any, Dict, List, Optional, Set, Tuple

DEFAULT_INPUT_DIR = os.path.expanduser("~/notes")
DEFAULT_OUTPUT_DIR = (
    "/google/src/cloud/marcelvaldez/personal_notes/company/users/marcelvaldez"
)
DEFAULT_STATE_DIR = os.path.expanduser("~/.config/sync_notes_to_company")

# Built-in fallback constants if no .syncnotesignore file is found
DEFAULT_EXCLUDED_FILES = {
    'playground.org',
    'TODO.org',
    'misc.org',
    'archive.org',
    'ali_one_on_one.md',
    'contacts.md',
    'peak_performance.md',
    'perf_and_callibrations_knowledge.md',
    'expectations_ddraft_2023.md',
    'draft_calibration_notes_q4_2020',
    'avid_perf_test_quality_for_2024.md',
    'avid_perf_test_highlights_lowlights.md',
    'q1_2022_okrs.md',
    'q1_2025_okrs.md',
    'career.md',
    'separation_progress.md',
    'team_wins.md',
}

DEFAULT_EXCLUDED_DIRS = {
    'personal',
    'interviews',
    'meetings',
    '.git',
    '.hg',
    '.citc',
    '.sync_state',
    '.jetski',
    '.jetskicli',
    '.lumbergh',
    '.snapshot',
}

DEFAULT_DYNAMIC_EXCLUDE_PATTERNS = [
    'meetings',
    'meetings_*',
    'meetings.*',
    'personal',
    'personal_*',
    'personal.*',
    'interviews',
    'interviews_*',
    'interviews.*',
    '*calibration*',
    '*callibration*',
    '*one_on_one*',
    '*1_on_1*',
    '*1-on-1*',
    '*one-on-one*',
    '*perf_review*',
    '*grad_review*',
    '*expectations_ddraft*',
]

ALLOWED_EXTENSIONS = {
    '.md',
    '.org',
    '.png',
    '.jpg',
    '.jpeg',
    '.gif',
    '.svg',
    '.org_settings',
    '.settings',
}

IMAGE_EXTENSIONS = {
    '.png',
    '.jpg',
    '.jpeg',
    '.gif',
    '.svg',
}

STAGING_ONLY_PATTERNS = [
    r'.*\.settings$',
    r'.*\.org_settings$',
]

DEFAULT_IGNORE_PATTERNS = [
    r'^\..*\.~undo-tree~$',
    r'^\.#.*',
    r'^#.*#$',
    r'.*~$',
    r'.*\.org_archive$',
    r'.*\.swp$',
    r'.*\.tmp$',
    r'.*\.py$',
    r'.*\.csv$',
    r'.*\.odt$',
]


def log(msg: str) -> None:
  """Prints formatted log message."""
  print(f'[{sys.argv[0].split("/")[-1]}] {msg}')


def sha256_file(filepath: str) -> str:
  """Computes SHA256 checksum of a file."""
  h = hashlib.sha256()
  with open(filepath, 'rb') as f:
    while chunk := f.read(65536):
      h.update(chunk)
  return h.hexdigest()


class IgnoreRules:
  """Manages file, directory, and pattern ignore rules loaded from .syncnotesignore."""

  def __init__(
      self,
      excluded_files: Optional[Set[str]] = None,
      excluded_dirs: Optional[Set[str]] = None,
      dynamic_patterns: Optional[List[str]] = None,
      ignore_patterns: Optional[List[str]] = None,
      source_file: Optional[str] = None,
  ):
    self.excluded_files: Set[str] = (
        set(excluded_files) if excluded_files is not None else set(DEFAULT_EXCLUDED_FILES)
    )
    self.excluded_dirs: Set[str] = (
        set(excluded_dirs) if excluded_dirs is not None else set(DEFAULT_EXCLUDED_DIRS)
    )
    self.dynamic_patterns: List[str] = (
        list(dynamic_patterns) if dynamic_patterns is not None else list(DEFAULT_DYNAMIC_EXCLUDE_PATTERNS)
    )
    self.ignore_patterns: List[str] = (
        list(ignore_patterns) if ignore_patterns is not None else list(DEFAULT_IGNORE_PATTERNS)
    )
    self.source_file = source_file

  @classmethod
  def from_file(cls, filepath: str) -> "IgnoreRules":
    """Parses ignore rules from a .syncnotesignore file."""
    if not os.path.isfile(filepath):
      return cls()

    excluded_files: Set[str] = set()
    excluded_dirs: Set[str] = set()
    dynamic_patterns: List[str] = []
    ignore_patterns: List[str] = []

    with open(filepath, "r", encoding="utf-8", errors="ignore") as f:
      for raw_line in f:
        line = raw_line.strip()
        if not line or line.startswith("#"):
          continue

        # Directory syntax (ends with '/')
        if line.endswith("/"):
          d = line.rstrip("/")
          excluded_dirs.add(d)
          dynamic_patterns.append(d)
          dynamic_patterns.append(f"{d}/*")
          continue

        # Regex syntax
        if line.startswith("^") or (line.startswith(".*") and line.endswith("$")):
          ignore_patterns.append(line)
        # Glob pattern syntax
        elif any(c in line for c in ["*", "?", "[", "]"]):
          dynamic_patterns.append(line)
        # Exact file or dir name
        else:
          excluded_files.add(line)
          dynamic_patterns.append(line)

    # Always ensure critical hidden VCS and runtime directories remain excluded
    excluded_dirs.update({
        ".git",
        ".hg",
        ".citc",
        ".sync_state",
        ".jetski",
        ".jetskicli",
        ".lumbergh",
        ".snapshot",
    })

    # Always ensure standard lock/backup regex patterns exist
    for pat in DEFAULT_IGNORE_PATTERNS:
      if pat not in ignore_patterns:
        ignore_patterns.append(pat)

    return cls(
        excluded_files=excluded_files,
        excluded_dirs=excluded_dirs,
        dynamic_patterns=dynamic_patterns,
        ignore_patterns=ignore_patterns,
        source_file=filepath,
    )

  def is_ignored_file(self, filename: str) -> bool:
    """Checks if filename matches any ignored patterns."""
    base = os.path.basename(filename)
    if base in self.excluded_files:
      return True
    if base.startswith("."):
      return True
    for pat in self.dynamic_patterns:
      if fnmatch.fnmatch(base, pat) or fnmatch.fnmatch(filename, pat):
        return True
    for pat in self.ignore_patterns:
      if re.match(pat, base):
        return True
    return False

  def is_ignored_dir(self, dirname: str) -> bool:
    """Checks if a directory name matches any excluded directories or patterns."""
    base = os.path.basename(dirname.rstrip('/\\'))
    if base in self.excluded_dirs or base.startswith("."):
      return True
    for pat in self.dynamic_patterns:
      if fnmatch.fnmatch(base, pat) or fnmatch.fnmatch(dirname, pat):
        return True
    for pat in self.ignore_patterns:
      if re.match(pat, base):
        return True
    return False

  def is_path_ignored(self, rel_path: str) -> bool:
    """Checks if any component of a relative path matches ignored dirs or files."""
    norm_path = os.path.normpath(rel_path)
    parts = norm_path.split(os.sep)
    for d in parts[:-1]:
      if not d or d == ".":
        continue
      if self.is_ignored_dir(d):
        return True
    if parts and (self.is_ignored_file(parts[-1]) or self.is_ignored_dir(parts[-1])):
      return True
    for pat in self.dynamic_patterns:
      if fnmatch.fnmatch(norm_path, pat):
        return True
    return False


def load_ignore_rules(
    input_dir: Optional[str] = None,
    state_dir: Optional[str] = None,
    custom_path: Optional[str] = None,
) -> IgnoreRules:
  """Finds and loads ignore rules from .syncnotesignore."""
  candidates = []
  if custom_path:
    candidates.append(os.path.abspath(custom_path))
  if input_dir:
    candidates.append(os.path.join(os.path.abspath(input_dir), ".syncnotesignore"))
  if state_dir:
    candidates.append(os.path.join(os.path.abspath(state_dir), ".syncnotesignore"))
  candidates.append(os.path.expanduser("~/.config/sync_notes_to_company/.syncnotesignore"))
  candidates.append(os.path.expanduser("~/notes/.syncnotesignore"))

  for candidate in candidates:
    if os.path.isfile(candidate):
      return IgnoreRules.from_file(candidate)

  return IgnoreRules()


# Global default instance for backwards-compatible standalone function calls
GLOBAL_IGNORE_RULES = load_ignore_rules()


def is_ignored_file(filename: str, rules: Optional[IgnoreRules] = None) -> bool:
  """Checks if filename matches any ignored patterns."""
  active_rules = rules or GLOBAL_IGNORE_RULES
  return active_rules.is_ignored_file(filename)


def is_ignored_dir(dirname: str, rules: Optional[IgnoreRules] = None) -> bool:
  """Checks if a directory name matches any excluded directories or patterns."""
  active_rules = rules or GLOBAL_IGNORE_RULES
  return active_rules.is_ignored_dir(dirname)


def is_path_ignored(rel_path: str, rules: Optional[IgnoreRules] = None) -> bool:
  """Checks if any component of a relative path matches ignored dirs or files."""
  active_rules = rules or GLOBAL_IGNORE_RULES
  return active_rules.is_path_ignored(rel_path)


def is_staging_only(rel_path: str) -> bool:
  """Checks if file is only for staging and should not be published."""
  for pat in STAGING_ONLY_PATTERNS:
    if re.match(pat, rel_path):
      return True
  return False


def convert_org_to_md(org_path: str, md_out_path: str) -> bool:
  """Converts an .org file to markdown using pandoc."""
  cmd = [
      "pandoc",
      org_path,
      f"--output={md_out_path}",
      "--from=org",
      "--to=markdown_strict",
      "--standalone",
      "--toc",
      "--strip-comments",
      "--toc-depth=2",
      "--tab-stop=4",
  ]
  try:
    res = subprocess.run(cmd, capture_output=True, text=True)
    if res.returncode != 0:
      log(f"Warning: pandoc conversion failed for {org_path}: {res.stderr}")
      return False
    return True
  except FileNotFoundError:
    log(f"Warning: pandoc executable not found when converting {org_path}")
    return False
  except Exception as e:
    log(f"Warning: pandoc conversion exception for {org_path}: {e}")
    return False


class SyncEngine:
  """Manages 3-way synchronization between local notes and company docs."""

  def __init__(
      self,
      input_dir: str,
      output_dir: str,
      state_dir: str,
      dry_run: bool = False,
      skip_vcs: bool = False,
      skip_submit: bool = False,
      ignore_file: Optional[str] = None,
  ):
    self.input_dir = os.path.abspath(input_dir)
    self.output_dir = os.path.abspath(output_dir)
    self.state_dir = os.path.abspath(state_dir)
    self.base_cache_dir = os.path.join(self.state_dir, "base")
    self.state_file = os.path.join(self.state_dir, "state.json")
    self.dry_run = dry_run
    self.skip_vcs = skip_vcs
    self.skip_submit = skip_submit
    self.ignore_rules = load_ignore_rules(
        input_dir=self.input_dir,
        state_dir=self.state_dir,
        custom_path=ignore_file,
    )

    os.makedirs(self.base_cache_dir, exist_ok=True)
    self.state: Dict[str, Dict[str, str]] = self._load_state()

  def _load_state(self) -> Dict[str, Dict[str, str]]:
    """Loads state.json mapping relative paths to hashes."""
    if os.path.exists(self.state_file):
      try:
        with open(self.state_file, "r") as f:
          return json.load(f)
      except Exception as e:
        log(f"Warning: Failed to load {self.state_file}: {e}")
    return {}

  def _save_state(self) -> None:
    """Persists state.json."""
    if self.dry_run:
      return
    with open(self.state_file, "w") as f:
      json.dump(self.state, f, indent=2, sort_keys=True)

  def vcs_sync(self) -> None:
    """Pulls latest changes in output_dir if inside Piper or Hg."""
    if self.skip_vcs or self.dry_run:
      return
    if not os.path.exists(self.output_dir):
      log(f"Output directory does not exist: {self.output_dir}")
      return

    # Check if inside g4 client
    g4_check = subprocess.run(
        ["g4", "info"], cwd=self.output_dir, capture_output=True
    )
    if g4_check.returncode == 0:
      log("Running g4 sync in company docs directory...")
      res = subprocess.run(
          ["g4", "sync"], cwd=self.output_dir, capture_output=True, text=True
      )
      if res.returncode != 0:
        log(f"Warning: g4 sync failed: {res.stderr}")
      return

    # Check if inside hg client
    hg_check = subprocess.run(
        ["hg", "status"], cwd=self.output_dir, capture_output=True
    )
    if hg_check.returncode == 0:
      log("Running hg sync in company docs directory...")
      subprocess.run(
          ["hg", "sync"], cwd=self.output_dir, capture_output=True, text=True
      )

  def scan_local_files(
      self, staging_dir: str
  ) -> Dict[str, Dict[str, Any]]:
    """Scans local notes directory and prepares content for publishing."""
    local_files: Dict[str, Dict[str, Any]] = {}
    if not os.path.exists(self.input_dir):
      return local_files

    for root, dirs, files in os.walk(self.input_dir):
      # Filter out excluded directories in-place so os.walk does not traverse them
      dirs[:] = [
          d
          for d in dirs
          if not self.ignore_rules.is_ignored_dir(d)
      ]

      # Find all existing markdown/content files first
      existing_mds = {
          os.path.splitext(f)[0]
          for f in files
          if not self.ignore_rules.is_ignored_file(f) and f.endswith('.md')
      }

      for f in sorted(files):
        if self.ignore_rules.is_ignored_file(f):
          continue
        full_path = os.path.join(root, f)
        if os.path.islink(full_path) and not os.path.exists(full_path):
          continue
        rel_path = os.path.relpath(full_path, self.input_dir)

        if self.ignore_rules.is_path_ignored(rel_path):
          continue

        base_name, ext = os.path.splitext(f)
        if f != 'METADATA' and ext not in ALLOWED_EXTENSIONS:
          continue

        if is_staging_only(rel_path):
          continue

        if ext == '.org':
          # If .md already exists in the same folder, prefer the .md file
          if base_name in existing_mds:
            continue
          rel_dir = os.path.dirname(rel_path)
          rel_md = os.path.normpath(os.path.join(rel_dir, f'{base_name}.md'))
          if self.ignore_rules.is_path_ignored(rel_md):
            continue
          staged_md = os.path.join(staging_dir, rel_md)
          os.makedirs(os.path.dirname(staged_md), exist_ok=True)
          if convert_org_to_md(full_path, staged_md):
            local_files[rel_md] = {
                'src_path': full_path,
                'is_org': True,
                'content_path': staged_md,
                'hash': sha256_file(staged_md),
            }
        else:
          local_files[rel_path] = {
              'src_path': full_path,
              'is_org': False,
              'content_path': full_path,
              'hash': sha256_file(full_path),
          }

    return local_files

  def scan_remote_files(self) -> Dict[str, Dict[str, Any]]:
    """Scans remote company docs directory."""
    remote_files: Dict[str, Dict[str, Any]] = {}
    if not os.path.exists(self.output_dir):
      return remote_files

    for root, dirs, files in os.walk(self.output_dir):
      dirs[:] = [
          d
          for d in dirs
          if d not in {'.git', '.hg', '.citc', '.snapshot'}
          and not self.ignore_rules.is_ignored_dir(d)
      ]

      for f in sorted(files):
        if self.ignore_rules.is_ignored_file(f):
          continue
        full_path = os.path.join(root, f)
        if os.path.islink(full_path) and not os.path.exists(full_path):
          continue
        rel_path = os.path.relpath(full_path, self.output_dir)
        if self.ignore_rules.is_path_ignored(rel_path) or is_staging_only(rel_path):
          continue
        base_name, ext = os.path.splitext(f)
        if f != 'METADATA' and ext not in ALLOWED_EXTENSIONS:
          continue
        remote_files[rel_path] = {
            'path': full_path,
            'hash': sha256_file(full_path),
        }

    return remote_files

  def three_way_merge(
      self, local_path: str, base_path: str, remote_path: str, out_path: str
  ) -> Tuple[bool, str]:
    """Runs git merge-file 3-way merge on local, base, and remote files."""
    temp_target = tempfile.NamedTemporaryFile(delete=False)
    temp_target.close()
    shutil.copy2(local_path, temp_target.name)

    cmd = [
        'git',
        'merge-file',
        '-L',
        'local_notes',
        '-L',
        'last_sync_base',
        '-L',
        'company_docs',
        temp_target.name,
        base_path,
        remote_path,
    ]
    res = subprocess.run(cmd, capture_output=True, text=True)
    with open(temp_target.name, 'r', encoding='utf-8', errors='replace') as f:
      merged_content = f.read()

    os.remove(temp_target.name)
    clean = res.returncode == 0
    return clean, merged_content

  def sync(self) -> Dict[str, int]:
    """Executes bidirectional 3-way synchronization."""
    self.vcs_sync()

    stats = {
        'local_updated': 0,
        'remote_updated': 0,
        'merged_clean': 0,
        'conflicts': 0,
        'remote_deleted': 0,
        'local_deleted': 0,
        'unchanged': 0,
    }

    with tempfile.TemporaryDirectory(prefix='notes_sync_') as staging_dir:
      local_files = self.scan_local_files(staging_dir)
      remote_files = self.scan_remote_files()

      all_rel_paths = (
          set(local_files.keys())
          | set(remote_files.keys())
          | set(self.state.keys())
      )

      for rel_path in sorted(all_rel_paths):
        if self.ignore_rules.is_path_ignored(rel_path):
          if rel_path in self.state and not self.dry_run:
            self._remove_from_base(rel_path)
          continue
        loc_info = local_files.get(rel_path)
        rem_info = remote_files.get(rel_path)
        base_info = self.state.get(rel_path)

        loc_hash = loc_info["hash"] if loc_info else None
        rem_hash = rem_info["hash"] if rem_info else None
        base_hash = base_info.get("hash") if base_info else None
        base_cached_file = os.path.join(self.base_cache_dir, rel_path)
        has_base_file = os.path.exists(base_cached_file)

        # Full target paths
        target_local = (
            loc_info["src_path"]
            if loc_info and not loc_info.get("is_org")
            else os.path.join(self.input_dir, rel_path)
        )
        target_remote = (
            rem_info["path"]
            if rem_info
            else os.path.join(self.output_dir, rel_path)
        )

        # Case 1: Exists on both sides, base exists
        if loc_hash and rem_hash and base_hash and has_base_file:
          if loc_hash == base_hash and rem_hash == base_hash:
            stats["unchanged"] += 1
          elif loc_hash != base_hash and rem_hash == base_hash:
            log(f"UPDATE COMPANY DOCS: {rel_path} (local modified)")
            if not self.dry_run:
              os.makedirs(os.path.dirname(target_remote), exist_ok=True)
              shutil.copy2(loc_info["content_path"], target_remote)
              self._update_base_cache(rel_path, loc_info["content_path"])
            stats["remote_updated"] += 1
          elif loc_hash == base_hash and rem_hash != base_hash:
            log(f"UPDATE LOCAL NOTES: {rel_path} (company docs newer)")
            if not self.dry_run:
              os.makedirs(os.path.dirname(target_local), exist_ok=True)
              shutil.copy2(target_remote, target_local)
              self._update_base_cache(rel_path, target_remote)
            stats["local_updated"] += 1
          else:
            # Both modified
            if loc_hash == rem_hash:
              # Modified to identical content
              if not self.dry_run:
                self._update_base_cache(rel_path, target_remote)
              stats["unchanged"] += 1
            else:
              _, ext = os.path.splitext(rel_path)
              if ext.lower() in IMAGE_EXTENSIONS:
                log(f"WARNING: BINARY CONFLICT in {rel_path}")
                stats["conflicts"] += 1
              else:
                log(f"CONCURRENT MODIFICATION: {rel_path} -> 3-way merge")
                clean, merged_content = self.three_way_merge(
                    loc_info["content_path"],
                    base_cached_file,
                    target_remote,
                    target_local,
                )
                if clean:
                  log(f"MERGED CLEAN: {rel_path}")
                  stats["merged_clean"] += 1
                else:
                  log(f"WARNING: MERGE CONFLICT in {rel_path}")
                  stats["conflicts"] += 1

                if not self.dry_run:
                  os.makedirs(os.path.dirname(target_local), exist_ok=True)
                  os.makedirs(os.path.dirname(target_remote), exist_ok=True)
                  with open(target_local, "w", encoding="utf-8") as f:
                    f.write(merged_content)
                  with open(target_remote, "w", encoding="utf-8") as f:
                    f.write(merged_content)
                  self._update_base_cache(rel_path, target_local)

        # Case 2: New in Remote only (not in local, not in base)
        elif rem_hash and not loc_hash and not base_hash:
          log(f"NEW FROM COMPANY DOCS: {rel_path} -> downloading locally")
          if not self.dry_run:
            os.makedirs(os.path.dirname(target_local), exist_ok=True)
            shutil.copy2(target_remote, target_local)
            self._update_base_cache(rel_path, target_remote)
          stats["local_updated"] += 1

        # Case 3: New in Local only (not in remote, not in base)
        elif loc_hash and not rem_hash and not base_hash:
          log(f"NEW LOCAL NOTE: {rel_path} -> publishing to company docs")
          if not self.dry_run:
            os.makedirs(os.path.dirname(target_remote), exist_ok=True)
            shutil.copy2(loc_info["content_path"], target_remote)
            self._update_base_cache(rel_path, loc_info["content_path"])
          stats["remote_updated"] += 1

        # Case 4: Exists on both sides, but no base recorded yet (initial sync)
        elif loc_hash and rem_hash and not base_hash:
          if loc_hash == rem_hash:
            if not self.dry_run:
              self._update_base_cache(rel_path, target_remote)
            stats["unchanged"] += 1
          else:
            log(f"INITIAL BASE ESTABLISHED: {rel_path} (recording local notes)")
            if not self.dry_run:
              os.makedirs(os.path.dirname(target_remote), exist_ok=True)
              shutil.copy2(loc_info["content_path"], target_remote)
              self._update_base_cache(rel_path, loc_info["content_path"])
            stats["remote_updated"] += 1

        # Case 5: Deleted locally (in base and remote, not in local)
        elif base_hash and rem_hash and not loc_hash:
          if rem_hash == base_hash:
            log(f"DELETED LOCALLY: {rel_path} -> removing from company docs")
            if not self.dry_run:
              if os.path.exists(target_remote):
                os.remove(target_remote)
              self._remove_from_base(rel_path)
            stats["remote_deleted"] += 1
          else:
            log(
                f"CONFLICT: {rel_path} deleted locally but modified in company"
                " docs -> recreating locally"
            )
            if not self.dry_run:
              os.makedirs(os.path.dirname(target_local), exist_ok=True)
              shutil.copy2(target_remote, target_local)
              self._update_base_cache(rel_path, target_remote)
            stats["local_updated"] += 1

        # Case 6: Deleted in company docs (in base and local, not in remote)
        elif base_hash and loc_hash and not rem_hash:
          if loc_hash == base_hash:
            log(f"DELETED IN COMPANY DOCS: {rel_path} -> removing locally")
            if not self.dry_run:
              if os.path.exists(target_local):
                os.remove(target_local)
              self._remove_from_base(rel_path)
            stats["local_deleted"] += 1
          else:
            log(
                f"CONFLICT: {rel_path} deleted in company docs but modified"
                " locally -> re-publishing"
            )
            if not self.dry_run:
              os.makedirs(os.path.dirname(target_remote), exist_ok=True)
              shutil.copy2(loc_info["content_path"], target_remote)
              self._update_base_cache(rel_path, loc_info["content_path"])
            stats["remote_updated"] += 1

        # Case 7: In base only (already deleted on both sides)
        elif base_hash and not loc_hash and not rem_hash:
          if not self.dry_run:
            self._remove_from_base(rel_path)

      self._save_state()

    log(
        f"Sync Summary: {stats['local_updated']} local updated,"
        f" {stats['remote_updated']} remote updated, {stats['merged_clean']}"
        f" merged, {stats['conflicts']} conflicts, {stats['unchanged']}"
        " unchanged"
    )

    if not self.skip_vcs and not self.dry_run:
      self.vcs_commit_and_upload()

    return stats

  def _update_base_cache(self, rel_path: str, src_path: str) -> None:
    """Updates base cache file and state entry."""
    cache_path = os.path.join(self.base_cache_dir, rel_path)
    os.makedirs(os.path.dirname(cache_path), exist_ok=True)
    shutil.copy2(src_path, cache_path)
    self.state[rel_path] = {
        "hash": sha256_file(cache_path),
    }

  def _remove_from_base(self, rel_path: str) -> None:
    """Removes file from base cache and state."""
    cache_path = os.path.join(self.base_cache_dir, rel_path)
    if os.path.exists(cache_path):
      os.remove(cache_path)
    self.state.pop(rel_path, None)

  def vcs_commit_and_upload(self) -> None:
    """Reconciles Piper files, creates CL description, uploads, and submits."""
    if not os.path.exists(self.output_dir):
      return

    # Check if g4 client
    g4_check = subprocess.run(
        ["g4", "info"], cwd=self.output_dir, capture_output=True
    )
    if g4_check.returncode == 0:
      log("Reconciling Piper files in company docs directory...")
      subprocess.run(["g4", "fix"], cwd=self.output_dir, capture_output=True)

      # Check if any files modified / added / deleted
      g4_status = subprocess.run(
          ["g4", "status"],
          cwd=self.output_dir,
          capture_output=True,
          text=True,
      )
      if (
          g4_status.returncode == 0
          and not g4_status.stdout.strip()
      ):
        log("No changes to upload in company docs.")
        return

      gen_desc = os.path.expanduser("~/scripts/gen_notes_cl_description.sh")
      env = os.environ.copy()
      if os.path.exists(gen_desc):
        env["EDITOR"] = gen_desc

      if self.skip_submit:
        env["SKIP_SUBMIT"] = "1"

      log("Creating/updating Piper change description...")
      subprocess.run(["g4", "change"], cwd=self.output_dir, env=env)
      subprocess.run(["g4", "fix"], cwd=self.output_dir)
      log("Uploading CL to Critique...")
      subprocess.run(["g4", "upload"], cwd=self.output_dir)

      if not self.skip_submit and "SKIP_SUBMIT" not in os.environ:
        log("Submitting CL...")
        subprocess.run(["g4", "submit"], cwd=self.output_dir)


def main():
  parser = argparse.ArgumentParser(
      description="Bidirectional 3-way sync engine for notes and company docs."
  )
  parser.add_argument(
      "-i",
      "--input",
      default=os.environ.get("INPUT_DIRECTORY", DEFAULT_INPUT_DIR),
      help="Local notes directory",
  )
  parser.add_argument(
      "-o",
      "--output",
      default=os.environ.get("OUTPUT_DIRECTORY", DEFAULT_OUTPUT_DIR),
      help="Company docs output directory",
  )
  parser.add_argument(
      "-s",
      "--state-dir",
      default=os.environ.get("SYNC_STATE_DIR", DEFAULT_STATE_DIR),
      help="Directory to store sync state cache",
  )
  parser.add_argument(
      "-n",
      "--dry-run",
      action="store_true",
      help="Perform dry run without modifying files or VCS",
  )
  parser.add_argument(
      "--skip-vcs",
      action="store_true",
      help="Skip Piper/Hg VCS operations (sync, change, upload, submit)",
  )
  parser.add_argument(
      "--skip-submit",
      action="store_true",
      help="Upload CL to Critique without running g4 submit",
  )
  parser.add_argument(
      "--ignore-file",
      default=None,
      help="Path to custom .syncnotesignore file",
  )

  args = parser.parse_args()

  engine = SyncEngine(
      input_dir=args.input,
      output_dir=args.output,
      state_dir=args.state_dir,
      dry_run=args.dry_run,
      skip_vcs=args.skip_vcs,
      skip_submit=args.skip_submit,
      ignore_file=args.ignore_file,
  )

  stats = engine.sync()
  if stats["conflicts"] > 0:
    sys.exit(1)


if __name__ == "__main__":
  main()
