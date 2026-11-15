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
    "/google/src/cloud/marcelvaldez/personal_notes/company/users/marcelvaldez/notes"
)
DEFAULT_NOTES_URL_PREFIX = "/company/users/marcelvaldez/notes"
DEFAULT_STATE_DIR = os.path.expanduser("~/.config/sync_notes_to_company")

SITE_ROOT_EXCLUDED_FILES: frozenset[str] = frozenset({
    "METADATA",
    "index.md",
    "sitemap.md",
    "navbar.md",
    "jetski_cli_notify.md",
})

SITE_ROOT_EXCLUDED_DIRS: frozenset[str] = frozenset({
    "asrf",
    "archive",
    "tools",
})

_UPPERCASE_WORDS: frozenset[str] = frozenset({
    "AI",
    "AP27",
    "APP",
    "ASCI",
    "ASRF",
    "AVID",
    "CLI",
    "GDM",
    "IBA",
    "NUMA",
    "TDP",
    "UDWS",
})

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
      anchored_files: Optional[Set[str]] = None,
      anchored_dirs: Optional[Set[str]] = None,
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
    self.anchored_files: Set[str] = set(anchored_files) if anchored_files is not None else set()
    self.anchored_dirs: Set[str] = set(anchored_dirs) if anchored_dirs is not None else set()
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
    anchored_files: Set[str] = set()
    anchored_dirs: Set[str] = set()

    with open(filepath, "r", encoding="utf-8", errors="ignore") as f:
      for raw_line in f:
        line = raw_line.strip()
        if not line or line.startswith("#"):
          continue

        # Root-anchored rule (starts with '/')
        if line.startswith("/"):
          trimmed = line.lstrip("/")
          if not trimmed:
            continue
          if trimmed.endswith("/"):
            anchored_dirs.add(trimmed.rstrip("/"))
          else:
            anchored_files.add(trimmed)
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
        anchored_files=anchored_files,
        anchored_dirs=anchored_dirs,
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
    posix_path = norm_path.replace(os.sep, "/")
    if posix_path in self.anchored_files:
      return True
    for ad in self.anchored_dirs:
      if posix_path == ad or posix_path.startswith(f"{ad}/"):
        return True
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


G3MARK_OPTIN_COMMENT = "<!-- go/g3mark-in-g3doc -->"
G3MARK_OPTIN_PATTERN = re.compile(
    r"^(?:---\n(?:(?:-{0,2}(?:[^-\n][^\n]*)?|---[^\n]+)\n)+---\n)?"
    r"\s*<!--\s*(?:go/g3mark-in-g3doc|lint-g3mark)\s*-->"
)
FRONTMATTER_PATTERN = re.compile(
    r"^(---\n(?:(?:-{0,2}(?:[^-\n][^\n]*)?|---[^\n]+)\n)+---\n)"
)


def has_g3mark_optin(content: str) -> bool:
  """Checks whether markdown content satisfies the g3mark opt-in presubmit check.

  Args:
    content: Raw markdown string to validate.

  Returns:
    True if the content contains a compliant g3mark opt-in directive at the
    beginning or immediately after an optional YAML frontmatter block.

  Usage Example:
    >>> has_g3mark_optin("<!-- go/g3mark-in-g3doc -->\n# Title")
    True
  """
  return bool(G3MARK_OPTIN_PATTERN.match(content))


def ensure_g3mark_optin(content: str) -> str:
  """Idempotently ensures markdown content includes the g3mark opt-in directive.

  If the content already contains the directive in a compliant position, it is
  returned unchanged. Otherwise, the directive is inserted immediately after
  the closing YAML frontmatter delimiter or prepended to the top of the file.

  Args:
    content: Markdown string to format.

  Returns:
    Compliant markdown string with the g3mark opt-in directive.

  Usage Example:
    >>> ensure_g3mark_optin("# Plan\nDetails")
    '<!-- go/g3mark-in-g3doc -->\n\n# Plan\nDetails'
  """
  if has_g3mark_optin(content):
    return content

  match = FRONTMATTER_PATTERN.match(content)
  if match:
    fm_block = match.group(1)
    rest = content[match.end():]
    cleaned_rest = rest.lstrip("\r\n")
    if cleaned_rest:
      return f"{fm_block}\n<!-- go/g3mark-in-g3doc -->\n\n{cleaned_rest}"
    return f"{fm_block}\n<!-- go/g3mark-in-g3doc -->\n"

  cleaned = content.lstrip("\r\n")
  if cleaned:
    return f"<!-- go/g3mark-in-g3doc -->\n\n{cleaned}"
  return "<!-- go/g3mark-in-g3doc -->\n"


def convert_org_to_md(org_path: str, md_out_path: str) -> bool:
  """Converts an org file to markdown using pandoc and formats for g3mark."""
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
    with open(md_out_path, "r", encoding="utf-8") as f:
      raw_content = f.read()
    formatted = ensure_g3mark_optin(raw_content)
    with open(md_out_path, "w", encoding="utf-8") as f:
      f.write(formatted)
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
      no_g3mark_ok: Optional[str] = None,
      site_root_dir: Optional[str] = None,
      sitemap_file: Optional[str] = None,
      notes_url_prefix: Optional[str] = None,
      allow_uninitialized_notes_dir: bool = False,
  ):
    self.input_dir = os.path.abspath(input_dir)
    self.output_dir = os.path.abspath(output_dir)
    norm_out = os.path.normpath(self.output_dir)
    if site_root_dir is not None:
      self.site_root_dir = os.path.abspath(site_root_dir)
    elif os.path.basename(norm_out) == "notes":
      self.site_root_dir = os.path.dirname(norm_out)
    else:
      self.site_root_dir = self.output_dir
    self.is_notes_subdir_mode = self.site_root_dir != self.output_dir
    if sitemap_file is not None:
      self.sitemap_file = os.path.abspath(sitemap_file) if sitemap_file else ""
    elif self.is_notes_subdir_mode:
      self.sitemap_file = os.path.join(self.site_root_dir, "sitemap.md")
    else:
      self.sitemap_file = ""
    self.notes_url_prefix = (
        notes_url_prefix.rstrip("/")
        if notes_url_prefix
        else self._infer_notes_url_prefix()
    )
    self.allow_uninitialized_notes_dir = allow_uninitialized_notes_dir
    self.state_dir = os.path.abspath(state_dir)
    self.base_cache_dir = os.path.join(self.state_dir, "base")
    self.state_file = os.path.join(self.state_dir, "state.json")
    self.dry_run = dry_run
    self.skip_vcs = skip_vcs
    self.skip_submit = skip_submit
    self.no_g3mark_ok = no_g3mark_ok
    self.ignore_rules = load_ignore_rules(
        input_dir=self.input_dir,
        state_dir=self.state_dir,
        custom_path=ignore_file,
    )

    os.makedirs(self.base_cache_dir, exist_ok=True)
    self.state: Dict[str, Dict[str, str]] = self._load_state()

  def _infer_notes_url_prefix(self) -> str:
    """Infers the g3doc root-relative URL prefix for output_dir."""
    norm = self.output_dir.replace(os.sep, "/")
    if "/company/" in norm:
      return "/company/" + norm.split("/company/", 1)[1].strip("/")
    return DEFAULT_NOTES_URL_PREFIX

  def _is_notes_mode_excluded(self, rel_path: str) -> bool:
    """Checks whether rel_path belongs to site-root config or top-level portals."""
    if not self.is_notes_subdir_mode:
      return False
    posix = os.path.normpath(rel_path).replace(os.sep, "/")
    if posix in SITE_ROOT_EXCLUDED_FILES:
      return True
    top_dir = posix.split("/", 1)[0]
    return "/" in posix and top_dir in SITE_ROOT_EXCLUDED_DIRS

  def _is_path_ignored(self, rel_path: str) -> bool:
    """Checks ignore rules plus notes-subdirectory site-root protections."""
    return self.ignore_rules.is_path_ignored(rel_path) or self._is_notes_mode_excluded(rel_path)

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
    """Pulls latest changes in site_root_dir if inside Piper or Hg."""
    if self.skip_vcs or self.dry_run:
      return
    vcs_dir = self.site_root_dir if os.path.exists(self.site_root_dir) else self.output_dir
    if not os.path.exists(vcs_dir):
      log(f"Output directory does not exist: {vcs_dir}")
      return

    # Check if inside g4 client
    g4_check = subprocess.run(
        ["g4", "info"], cwd=vcs_dir, capture_output=True
    )
    if g4_check.returncode == 0:
      log("Running g4 sync in company docs directory...")
      res = subprocess.run(
          ["g4", "sync"], cwd=vcs_dir, capture_output=True, text=True
      )
      if res.returncode != 0:
        log(f"Warning: g4 sync failed: {res.stderr}")
      return

    # Check if inside hg client
    hg_check = subprocess.run(
        ["hg", "status"], cwd=vcs_dir, capture_output=True
    )
    if hg_check.returncode == 0:
      log("Running hg sync in company docs directory...")
      subprocess.run(
          ["hg", "sync"], cwd=vcs_dir, capture_output=True, text=True
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
      is_root = os.path.abspath(root) == self.input_dir
      dirs[:] = [
          d
          for d in dirs
          if not self.ignore_rules.is_ignored_dir(d)
          and not (self.is_notes_subdir_mode and is_root and d in SITE_ROOT_EXCLUDED_DIRS)
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

        if self._is_path_ignored(rel_path):
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
          if self._is_path_ignored(rel_md):
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
      is_root = os.path.abspath(root) == self.output_dir
      dirs[:] = [
          d
          for d in dirs
          if d not in {'.git', '.hg', '.citc', '.snapshot'}
          and not self.ignore_rules.is_ignored_dir(d)
          and not (self.is_notes_subdir_mode and is_root and d in SITE_ROOT_EXCLUDED_DIRS)
      ]

      for f in sorted(files):
        if self.ignore_rules.is_ignored_file(f):
          continue
        full_path = os.path.join(root, f)
        if os.path.islink(full_path) and not os.path.exists(full_path):
          continue
        rel_path = os.path.relpath(full_path, self.output_dir)
        if self._is_path_ignored(rel_path) or is_staging_only(rel_path):
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

    temp_base = tempfile.NamedTemporaryFile(delete=False)
    temp_base.close()
    shutil.copy2(base_path, temp_base.name)

    temp_remote = tempfile.NamedTemporaryFile(delete=False)
    temp_remote.close()
    shutil.copy2(remote_path, temp_remote.name)

    if local_path.endswith('.md') or remote_path.endswith('.md'):
      for path in (temp_target.name, temp_base.name, temp_remote.name):
        with open(path, 'r', encoding='utf-8', errors='replace') as f:
          c = f.read()
        if not has_g3mark_optin(c):
          with open(path, 'w', encoding='utf-8') as f:
            f.write(ensure_g3mark_optin(c))

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
        temp_base.name,
        temp_remote.name,
    ]
    res = subprocess.run(cmd, capture_output=True, text=True)
    with open(temp_target.name, 'r', encoding='utf-8', errors='replace') as f:
      merged_content = f.read()

    os.remove(temp_target.name)
    os.remove(temp_base.name)
    os.remove(temp_remote.name)
    clean = res.returncode == 0
    return clean, merged_content

  def sync(self) -> Dict[str, int | bool]:
    """Executes bidirectional 3-way synchronization."""
    self.vcs_sync()

    stats: Dict[str, int | bool] = {
        'local_updated': 0,
        'remote_updated': 0,
        'merged_clean': 0,
        'conflicts': 0,
        'remote_deleted': 0,
        'local_deleted': 0,
        'unchanged': 0,
    }

    if (
        self.is_notes_subdir_mode
        and not self.allow_uninitialized_notes_dir
        and os.path.isdir(self.site_root_dir)
        and not os.path.isdir(self.output_dir)
    ):
      log(
          "ERROR: Notes target directory does not exist yet at "
          f"{self.output_dir}. Submit the CompanyDocs reorganization CL first "
          "or pass --allow-uninitialized-notes-dir. Aborting safely."
      )
      stats["aborted"] = True
      return stats

    with tempfile.TemporaryDirectory(prefix='notes_sync_') as staging_dir:
      local_files = self.scan_local_files(staging_dir)
      remote_files = self.scan_remote_files()

      all_rel_paths = (
          set(local_files.keys())
          | set(remote_files.keys())
          | set(self.state.keys())
      )

      for rel_path in sorted(all_rel_paths):
        if self._is_path_ignored(rel_path):
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
              if rel_path.endswith('.md'):
                self._publish_markdown(loc_info["content_path"], target_remote, target_local)
              else:
                shutil.copy2(loc_info["content_path"], target_remote)
              self._update_base_cache(rel_path, target_remote)
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
                  if rel_path.endswith('.md'):
                    merged_content = ensure_g3mark_optin(merged_content)
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
            if rel_path.endswith('.md'):
              self._publish_markdown(loc_info["content_path"], target_remote, target_local)
            else:
              shutil.copy2(loc_info["content_path"], target_remote)
            self._update_base_cache(rel_path, target_remote)
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
              if rel_path.endswith('.md'):
                self._publish_markdown(loc_info["content_path"], target_remote, target_local)
              else:
                shutil.copy2(loc_info["content_path"], target_remote)
              self._update_base_cache(rel_path, target_remote)
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
              if rel_path.endswith('.md'):
                self._publish_markdown(loc_info["content_path"], target_remote, target_local)
              else:
                shutil.copy2(loc_info["content_path"], target_remote)
              self._update_base_cache(rel_path, target_remote)
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

    if self.is_notes_subdir_mode and not self.dry_run:
      self.update_notes_index_and_sitemap()

    if not self.skip_vcs and not self.dry_run:
      self.vcs_commit_and_upload()

    return stats

  def _humanize_name(self, name: str) -> str:
    """Formats a filename or directory slug into a readable title."""
    tokens = re.split(r"[_\-\s]+", name.strip())
    formatted = []
    for tok in tokens:
      if not tok:
        continue
      upper = tok.upper()
      if upper in _UPPERCASE_WORDS:
        formatted.append("AViD" if upper == "AVID" else upper)
      else:
        formatted.append(tok.capitalize())
    return " ".join(formatted) if formatted else name

  def _extract_markdown_title(self, filepath: str, rel_path: str) -> str:
    """Extracts the primary heading title from a markdown note."""
    try:
      with open(filepath, "r", encoding="utf-8", errors="replace") as f:
        for raw_line in f:
          line = raw_line.strip()
          if line.startswith("# "):
            title = re.sub(r"\s*\{#[^}]+\}\s*$", "", line[2:].strip())
            title = re.sub(r"`([^`]+)`", r"\1", title)
            if title and title.lower() not in {"goal", "overview", "readme"}:
              return title
            break
    except OSError:
      pass
    return self._humanize_name(os.path.splitext(os.path.basename(rel_path))[0])

  def _extract_existing_note_links(self, text: str) -> Dict[str, str]:
    """Extracts {url: normalized_title} note links from markdown or sitemap."""
    if not text:
      return {}
    pattern = re.compile(
        rf"\[([^\]]+)\]\(({re.escape(self.notes_url_prefix)}/[^)\s]+\.md)\)"
    )
    links: Dict[str, str] = {}
    index_url = f"{self.notes_url_prefix}/index.md"
    for match in pattern.finditer(text):
      title = re.sub(r"\s+", " ", match.group(1)).strip()
      url = match.group(2).strip()
      if url != index_url:
        links[url] = title
    return links

  def _collect_notes_tree(
      self,
      existing_titles: Optional[Dict[str, str]] = None,
  ) -> Tuple[List[Tuple[str, str]], Dict[str, List[Tuple[str, str]]]]:
    """Collects sorted root notes and subfolder notes under output_dir."""
    root_notes: List[Tuple[str, str]] = []
    folder_notes: Dict[str, List[Tuple[str, str]]] = {}
    if not os.path.isdir(self.output_dir):
      return root_notes, folder_notes

    title_overrides = existing_titles or {}
    for dirpath, dirnames, filenames in os.walk(self.output_dir):
      dirnames[:] = sorted(
          d for d in dirnames
          if d not in {".git", ".hg", ".citc", ".snapshot"}
          and not self.ignore_rules.is_ignored_dir(d)
      )
      for fn in sorted(filenames):
        if not fn.endswith(".md") or self.ignore_rules.is_ignored_file(fn):
          continue
        full_path = os.path.join(dirpath, fn)
        rel_path = os.path.relpath(full_path, self.output_dir).replace(os.sep, "/")
        if rel_path == "index.md" or self._is_path_ignored(rel_path):
          continue
        url = f"{self.notes_url_prefix}/{rel_path}"
        title = title_overrides.get(url) or self._extract_markdown_title(full_path, rel_path)
        if "/" not in rel_path:
          root_notes.append((title, url))
        else:
          top_folder = rel_path.split("/", 1)[0]
          folder_notes.setdefault(top_folder, []).append((title, url))

    root_notes.sort(key=lambda item: (item[0].lower(), item[1]))
    for folder in folder_notes:
      folder_notes[folder].sort(key=lambda item: (item[0].lower(), item[1]))

    return root_notes, folder_notes

  def update_notes_index_and_sitemap(self) -> None:
    """Regenerates notes/index.md and the Notes subtree in site_root/sitemap.md.

    Preserves curated titles and formatting whenever the set of indexed note
    URLs is unchanged.

    Args:
      None.

    Returns:
      None.

    Usage Example:
      >>> engine.update_notes_index_and_sitemap()
    """
    if not self.is_notes_subdir_mode or self.dry_run:
      return

    notes_index_path = os.path.join(self.output_dir, "index.md")
    existing_index = ""
    if os.path.exists(notes_index_path):
      with open(notes_index_path, "r", encoding="utf-8", errors="replace") as f:
        existing_index = f.read()

    existing_sitemap = ""
    has_sitemap = bool(self.sitemap_file and os.path.isfile(self.sitemap_file))
    if has_sitemap:
      with open(self.sitemap_file, "r", encoding="utf-8", errors="replace") as f:
        existing_sitemap = f.read()

    existing_index_links = self._extract_existing_note_links(existing_index)
    existing_sitemap_links = self._extract_existing_note_links(existing_sitemap)
    curated_titles = {**existing_index_links, **existing_sitemap_links}

    root_notes, folder_notes = self._collect_notes_tree(existing_titles=curated_titles)
    target_urls: Set[str] = {url for _, url in root_notes} | {
        url for items in folder_notes.values() for _, url in items
    }

    # 1. Regenerate notes/index.md only when absent or the indexed note URL set changed
    if not os.path.exists(notes_index_path) or set(existing_index_links.keys()) != target_urls:
      index_lines: List[str] = [
          "<!-- go/g3mark-in-g3doc -->",
          "",
          "# Engineering Notes",
          "",
          "<!--* freshness: { owner: 'marcelvaldez' reviewed: '2026-11-14' } *-->",
          "",
          "Working notes, project research logs, and troubleshooting guides synced from",
          "`~/notes`. Historical pre-2025 notes remain in the",
          "[Archive](/company/users/marcelvaldez/archive/index.md), and active ASRF project",
          "documentation lives in the [ASRF Project Portal](/company/users/marcelvaldez/asrf/index.md).",
          "",
          "[TOC]",
          "",
          "## General Notes {#general-notes}",
          "",
      ]
      for title, url in root_notes:
        index_lines.append(f"*   [{title}]({url})")
      index_lines.extend(["", "## Topic & Project Folders {#note-folders}", ""])
      for folder in sorted(folder_notes):
        index_lines.append(f"*   **{self._humanize_name(folder)}**")
        for title, url in folder_notes[folder]:
          index_lines.append(f"    *   [{title}]({url})")
      index_content = "\n".join(index_lines) + "\n"

      if existing_index != index_content:
        os.makedirs(self.output_dir, exist_ok=True)
        with open(notes_index_path, "w", encoding="utf-8") as f:
          f.write(index_content)

    # 2. Update the *   [Notes](...) subtree in site_root/sitemap.md when URLs change
    if not has_sitemap:
      return

    sitemap_lines = existing_sitemap.splitlines()
    notes_header_re = re.compile(r"^\*\s+(?:\[Notes\]\([^)]+\)|Notes)(?:\s|$)")
    has_notes_section = any(notes_header_re.match(line) for line in sitemap_lines)
    if has_notes_section and set(existing_sitemap_links.keys()) == target_urls:
      return

    notes_children: List[str] = []
    for title, url in root_notes:
      notes_children.append(f"    *   [{title}]({url})")
    for folder in sorted(folder_notes):
      notes_children.append(f"    *   {self._humanize_name(folder)}")
      for title, url in folder_notes[folder]:
        notes_children.append(f"        *   [{title}]({url})")

    new_lines: List[str] = []
    i = 0
    replaced = False
    while i < len(sitemap_lines):
      line = sitemap_lines[i]
      if not replaced and notes_header_re.match(line):
        new_lines.append(f"*   [Notes]({self.notes_url_prefix}/index.md)")
        new_lines.extend(notes_children)
        i += 1
        while i < len(sitemap_lines) and not sitemap_lines[i].startswith("*"):
          i += 1
        replaced = True
        continue
      new_lines.append(line)
      i += 1

    if not replaced:
      new_lines.append(f"*   [Notes]({self.notes_url_prefix}/index.md)")
      new_lines.extend(notes_children)

    updated_sitemap = "\n".join(new_lines) + "\n"
    if updated_sitemap != existing_sitemap:
      with open(self.sitemap_file, "w", encoding="utf-8") as f:
        f.write(updated_sitemap)

  def _publish_markdown(
      self, src_content_path: str, target_remote: str, target_local: str
  ) -> None:
    """Publishes markdown content to company docs ensuring g3mark compliance.

    Args:
      src_content_path: Path to source or staged markdown file.
      target_remote: Destination path in company docs.
      target_local: Corresponding path in local notes directory.

    Usage Example:
      >>> engine._publish_markdown("/tmp/n.md", "/c/n.md", "~/notes/n.md")
    """
    with open(src_content_path, "r", encoding="utf-8") as f:
      raw = f.read()

    formatted = ensure_g3mark_optin(raw)
    with open(target_remote, "w", encoding="utf-8") as f:
      f.write(formatted)

    if (
        os.path.exists(target_local)
        and target_local.endswith(".md")
        and formatted != raw
    ):
      with open(target_local, "w", encoding="utf-8") as f:
        f.write(formatted)

  def _extract_opened_rel_path(self, depot_path: str) -> Optional[str]:
    """Maps a Piper depot path from g4 status to a relative path under output_dir."""
    norm_depot = depot_path.strip()
    norm_out = self.output_dir.replace(os.sep, "/").rstrip("/")
    if "company/users/" in norm_depot and "company/users/" in norm_out:
      depot_after = norm_depot.split("company/users/", 1)[1]
      out_after = norm_out.split("company/users/", 1)[1].strip("/") + "/"
      if depot_after.startswith(out_after):
        return depot_after[len(out_after):]
      return None
    if "company/users/" in norm_depot:
      parts = norm_depot.split("company/users/", 1)[1].split("/", 1)
      rel_from_user = parts[1] if len(parts) > 1 else os.path.basename(norm_depot)
      if self.is_notes_subdir_mode:
        subdir_name = os.path.basename(norm_out) + "/"
        if rel_from_user.startswith(subdir_name):
          return rel_from_user[len(subdir_name):]
        return None
      return rel_from_user
    return os.path.basename(norm_depot)

  def reconcile_opened_markdown_files(self) -> None:
    """Reconciles opened files in Piper client to guarantee g3mark compliance.

    Inspects `g4 status` in site_root_dir, identifies opened markdown files
    under output_dir that are missing the required g3mark opt-in directive,
    formats them in-place, and updates the local note and base cache to maintain
    hash parity.
    """
    if not os.path.exists(self.output_dir):
      return

    vcs_dir = self.site_root_dir if os.path.exists(self.site_root_dir) else self.output_dir
    g4_status = subprocess.run(
        ["g4", "status"],
        cwd=vcs_dir,
        capture_output=True,
        text=True,
    )
    if g4_status.returncode != 0 or not g4_status.stdout.strip():
      return

    for status_line in g4_status.stdout.splitlines():
      line = status_line.strip()
      if not ("- add" in line or "- edit" in line):
        continue
      depot_path = line.split("#")[0].strip()
      if not depot_path.endswith(".md"):
        continue
      if os.path.basename(depot_path) in {
          "sitemap.md",
          "navbar.md",
          "README.md",
          "SKILL.md",
          "AGENTS.md",
      }:
        continue

      rel_path = self._extract_opened_rel_path(depot_path)
      if not rel_path:
        continue
      if self.is_notes_subdir_mode and rel_path == "index.md":
        continue

      target_remote = os.path.join(self.output_dir, rel_path)
      if not os.path.exists(target_remote):
        continue

      with open(target_remote, "r", encoding="utf-8") as f:
        raw = f.read()

      if not has_g3mark_optin(raw):
        log(f"Reconciling g3mark opt-in for opened file: {rel_path}")
        formatted = ensure_g3mark_optin(raw)
        with open(target_remote, "w", encoding="utf-8") as f:
          f.write(formatted)

        target_local = os.path.join(self.input_dir, rel_path)
        if os.path.exists(target_local):
          with open(target_local, "w", encoding="utf-8") as f:
            f.write(formatted)

        self._update_base_cache(rel_path, target_remote)

    self._save_state()

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
    vcs_dir = self.site_root_dir if os.path.exists(self.site_root_dir) else self.output_dir
    if not os.path.exists(vcs_dir):
      return

    # Check if g4 client
    g4_check = subprocess.run(
        ["g4", "info"], cwd=vcs_dir, capture_output=True
    )
    if g4_check.returncode == 0:
      log("Reconciling Piper files in company docs directory...")
      self.reconcile_opened_markdown_files()
      subprocess.run(["g4", "fix"], cwd=vcs_dir, capture_output=True)

      # Check if any files modified / added / deleted
      g4_status = subprocess.run(
          ["g4", "status"],
          cwd=vcs_dir,
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
      if self.no_g3mark_ok:
        env["NO_G3MARK_OK"] = self.no_g3mark_ok

      log("Creating/updating Piper change description...")
      subprocess.run(["g4", "change"], cwd=vcs_dir, env=env)
      subprocess.run(["g4", "fix"], cwd=vcs_dir)
      log("Uploading CL to Critique...")
      subprocess.run(["g4", "upload"], cwd=vcs_dir)

      if not self.skip_submit and "SKIP_SUBMIT" not in os.environ:
        log("Submitting CL...")
        subprocess.run(["g4", "submit"], cwd=vcs_dir)


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
  parser.add_argument(
      "--no-g3mark-ok",
      default=os.environ.get("NO_G3MARK_OK", None),
      help="Optional reason tag for NO_G3MARK_OK to disable g3mark presubmit check",
  )
  parser.add_argument(
      "--allow-uninitialized-notes-dir",
      action="store_true",
      help="Allow creating output_dir under site_root even if notes/ does not yet exist",
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
      no_g3mark_ok=args.no_g3mark_ok,
      allow_uninitialized_notes_dir=args.allow_uninitialized_notes_dir,
  )

  stats = engine.sync()
  if stats["conflicts"] > 0:
    sys.exit(1)


if __name__ == "__main__":
  main()
