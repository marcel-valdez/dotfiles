"""CommonMark Mermaid fence extraction, ANSI width metrics, and line mapping.

Extracts top-level fenced Mermaid blocks into sentinel tokens before Markdown
rendering and aligns rendered terminal viewport lines back to 1-based raw
Markdown source line numbers for line-anchored review comments.

Usage Example:
  extracted = extract_mermaid_fences("# Plan\\n\\n```mermaid\\ngraph TD\\nA-->B\\n```")
  mapped = map_rendered_lines_to_source("# Plan\\n", ["Plan"])
"""

from __future__ import annotations

from collections.abc import Sequence
import dataclasses
import re
import unicodedata

import mdview_constants


_FENCE_OPEN_RE = re.compile(r"^([ \t]{0,8})(`{3,}|~{3,})([^\r\n]*)$")
_ANSI_ESCAPE_RE = re.compile(
    r"\x1bP.*?\x1b\\"
    r"|\x1b_.*?\x1b\\"
    r"|\x1b\].*?(?:\x07|\x1b\\)"
    r"|\x1b\[[0-9;:?]*[ -/]*[@-~]"
    r"|\x1b[@-_]",
    re.DOTALL,
)
_WORD_RE = re.compile(r"[A-Za-z0-9]+")

_MIN_DP_SCORE: int = -(1 << 29)
_SCORE_BOTH_BLANK: int = 10
_SCORE_MISMATCH_BLANK: int = -2
_SCORE_WORD_MATCH: int = 3
_SCORE_SUBSTRING_MATCH: int = 3
_SCORE_NO_MATCH: int = -1
_SCORE_EXACT_MATCH: int = 25
_SCORE_DIAGONAL_BONUS: int = 10


@dataclasses.dataclass(frozen=True)
class MermaidFenceBlock:
  """Metadata and raw content of a top-level fenced Mermaid diagram block.

  Attributes:
    index: Zero-based ordinal index of the Mermaid block in the document.
    source_text: Raw Mermaid diagram syntax inside the fences.
    start_line: One-based raw line number of the opening fence.
    end_line: One-based raw line number of the closing fence.
    sentinel: Unique alphanumeric placeholder substituted into Markdown.
  """

  index: int
  source_text: str
  start_line: int
  end_line: int
  sentinel: str


@dataclasses.dataclass(frozen=True)
class ExtractedDocument:
  """Result of scanning a Markdown document for fenced Mermaid blocks.

  Attributes:
    substituted_markdown: Markdown text with Mermaid blocks replaced by sentinels.
    blocks: Tuple of extracted Mermaid blocks in document order.
    raw_lines: Tuple of original raw Markdown lines.
  """

  substituted_markdown: str
  blocks: tuple[MermaidFenceBlock, ...]
  raw_lines: tuple[str, ...]


@dataclasses.dataclass(frozen=True)
class CollapseSpan:
  """Viewport row span corresponding to a single originating raw source block.

  Attributes:
    start_row: Zero-based inclusive starting viewport row index.
    end_row: Zero-based exclusive ending viewport row index.
    source_line: One-based raw Markdown line number assigned to the span.
    end_source_line: Optional 1-based closing line of the block; subsequent
      viewport rows are constrained to map at or after this line.
  """

  start_row: int
  end_row: int
  source_line: int
  end_source_line: int = 0


@dataclasses.dataclass(frozen=True)
class _SourceLineFeature:
  """Precomputed lexical matching features for a single raw Markdown line.

  Attributes:
    clean: Trimmed raw source line text.
    words: Lowercase alphanumeric tokens present on the line.
    is_blank: Whether the line contains no non-whitespace characters.
    is_code: Whether the line lies inside a non-Mermaid fenced code block.
  """

  clean: str
  words: frozenset[str]
  is_blank: bool
  is_code: bool


@dataclasses.dataclass(frozen=True)
class _StyledCluster:
  """Single visible character cluster with preceding escapes and active styles.

  Attributes:
    prefix_escapes: Escape sequences appearing immediately before `text`.
    text: Base character plus any attached zero-width combining marks.
    width: Display cell width (`1` or `2`).
    sgr_state: Tuple of active SGR sequences after rendering `text`.
    osc8_open: Active OSC-8 hyperlink open sequence, or empty string.
  """

  prefix_escapes: str
  text: str
  width: int
  sgr_state: tuple[str, ...]
  osc8_open: str


def make_sentinel(index: int) -> str:
  """Builds a CommonMark-inert placeholder token for diagram `index`.

  Args:
    index: Zero-based diagram index.

  Returns:
    Sentinel string containing zero Markdown emphasis delimiters.

  Raises:
    ValueError: If `index` is negative.

  Usage Example:
    token = make_sentinel(0)  # "@@MDVIEWDIAG0000@@"
  """
  if index < 0:
    raise ValueError(f"Diagram index must be non-negative, got {index}")
  return (
      f"{mdview_constants.SENTINEL_PREFIX}"
      f"{index:04d}"
      f"{mdview_constants.SENTINEL_SUFFIX}"
  )


def extract_mermaid_fences(markdown_text: str) -> ExtractedDocument:
  """Extracts top-level `mermaid` fenced blocks and substitutes sentinels.

  Respects outer CommonMark backtick and tilde fence lengths so nested example
  fences inside 4-backtick blocks remain untouched.

  Args:
    markdown_text: Raw Markdown document text.

  Returns:
    `ExtractedDocument` holding sentinel-substituted Markdown and block spans.

  Usage Example:
    doc = extract_mermaid_fences("# Title\\n```mermaid\\nflowchart TD\\nA-->B\\n```")
  """
  raw_lines = tuple(markdown_text.splitlines())
  output_lines: list[str] = []
  blocks: list[MermaidFenceBlock] = []

  in_fence = False
  fence_char = ""
  fence_len = 0
  fence_is_mermaid = False
  fence_start_line = 0
  fence_indent = ""
  fence_buffer: list[str] = []

  for zero_idx, line in enumerate(raw_lines):
    line_num = zero_idx + 1
    if not in_fence:
      if match := _FENCE_OPEN_RE.match(line):
        indent, marker, info = match.groups()
        # In CommonMark, backtick info strings cannot contain backticks.
        if marker[0] == "`" and "`" in info:
          output_lines.append(line)
          continue
        first_token = info.strip().split()[0].lower() if info.strip() else ""
        in_fence = True
        fence_char = marker[0]
        fence_len = len(marker)
        fence_is_mermaid = first_token == "mermaid"
        fence_start_line = line_num
        fence_indent = indent
        fence_buffer = [line] if not fence_is_mermaid else []
      else:
        output_lines.append(line)
      continue

    stripped = line.strip()
    is_closing = (
        len(stripped) >= fence_len
        and stripped == fence_char * len(stripped)
    )
    if is_closing:
      if fence_is_mermaid:
        block_index = len(blocks)
        sentinel = make_sentinel(block_index)
        blocks.append(
            MermaidFenceBlock(
                index=block_index,
                source_text="\n".join(fence_buffer),
                start_line=fence_start_line,
                end_line=line_num,
                sentinel=sentinel,
            )
        )
        output_lines.append("")
        output_lines.append(f"{fence_indent}{sentinel}")
        output_lines.append("")
      else:
        fence_buffer.append(line)
        output_lines.extend(fence_buffer)
      in_fence = False
      fence_char = ""
      fence_len = 0
      fence_is_mermaid = False
      fence_buffer = []
    else:
      fence_buffer.append(line)

  if in_fence:
    # Unclosed fence at EOF: preserve verbatim rather than dropping user text.
    if fence_is_mermaid:
      output_lines.append(raw_lines[fence_start_line - 1])
    output_lines.extend(fence_buffer)

  return ExtractedDocument(
      substituted_markdown="\n".join(output_lines) + "\n",
      blocks=tuple(blocks),
      raw_lines=raw_lines,
  )


def strip_terminal_escapes(line: str) -> str:
  """Removes CSI, OSC-8, DCS, and APC terminal control sequences from `line`.

  Args:
    line: Terminal-formatted string containing ANSI or Kitty escape sequences.

  Returns:
    Plain visible character string without escape codes.

  Usage Example:
    plain = strip_terminal_escapes("\\x1b[1mTitle\\x1b[0m")  # "Title"
  """
  return _ANSI_ESCAPE_RE.sub("", line).replace("\r", "")


def visible_cell_width(line: str) -> int:
  """Computes terminal display column width of `line` after stripping escapes.

  Zero-width combining diacritics (such as Kitty `U+10EEEE` row/column marks)
  contribute 0 columns, `U+10EEEE` contributes 1 column, and wide CJK/emoji
  codepoints contribute 2 columns.

  Args:
    line: Single rendered terminal row (without trailing newline).

  Returns:
    Non-negative integer column width in monospace terminal cells.

  Usage Example:
    cols = visible_cell_width("\\x1b[32mHello\\x1b[0m")  # 5
  """
  plain = strip_terminal_escapes(line)
  width = 0
  for ch in plain:
    if ch == mdview_constants.UNICODE_PLACEHOLDER_CHAR:
      width += 1
      continue
    if unicodedata.combining(ch):
      continue
    category = unicodedata.category(ch)
    if category in ("Mn", "Me", "Cf", "Cc"):
      continue
    east_asian = unicodedata.east_asian_width(ch)
    width += 2 if east_asian in ("W", "F") else 1
  return width


def wrap_ansi_line_to_width(line: str, max_width: int) -> tuple[str, ...]:
  """Wraps an ANSI/OSC-8 styled terminal line to fit within `max_width` cells.

  Preserves active SGR colors and OSC-8 hyperlinks across wrapped continuation
  lines while leaving Kitty `U+10EEEE` image placeholder rows untouched.

  Args:
    line: Single rendered terminal row (without trailing newline).
    max_width: Maximum visible cell width per output row.

  Returns:
    Tuple of one or more ANSI-safe rows each fitting within `max_width` cells.

  Usage Example:
    rows = wrap_ansi_line_to_width("\\x1b[1mLong heading text\\x1b[0m", 12)
  """
  if (
      max_width < 1
      or mdview_constants.UNICODE_PLACEHOLDER_CHAR in line
      or visible_cell_width(line) <= max_width
  ):
    return (line,)

  cleaned_line = line.replace("\r", "")
  segments: list[tuple[bool, str]] = []
  pos = 0
  for match in _ANSI_ESCAPE_RE.finditer(cleaned_line):
    start, end = match.span()
    if start > pos:
      segments.append((False, cleaned_line[pos:start]))
    segments.append((True, match.group(0)))
    pos = end
  if pos < len(cleaned_line):
    segments.append((False, cleaned_line[pos:]))

  clusters: list[_StyledCluster] = []
  pending_escapes: list[str] = []
  active_sgr: list[str] = []
  active_osc8 = ""

  cur_prefix = ""
  cur_chars: list[str] = []
  cur_width = 0
  cur_sgr: tuple[str, ...] = ()
  cur_osc8 = ""

  for is_escape, chunk in segments:
    if is_escape:
      if cur_chars:
        clusters.append(
            _StyledCluster(
                prefix_escapes=cur_prefix,
                text="".join(cur_chars),
                width=cur_width,
                sgr_state=cur_sgr,
                osc8_open=cur_osc8,
            )
        )
        cur_chars = []
      pending_escapes.append(chunk)
      if chunk.startswith("\x1b[") and chunk.endswith("m"):
        if chunk in ("\x1b[0m", "\x1b[m"):
          active_sgr.clear()
        else:
          active_sgr.append(chunk)
      elif chunk.startswith("\x1b]8;"):
        if chunk in ("\x1b]8;;\x1b\\", "\x1b]8;;\x07"):
          active_osc8 = ""
        else:
          active_osc8 = chunk
      continue

    for ch in chunk:
      is_zero_width = bool(unicodedata.combining(ch)) or unicodedata.category(
          ch
      ) in ("Mn", "Me", "Cf", "Cc")
      if is_zero_width:
        if cur_chars:
          cur_chars.append(ch)
        continue
      if cur_chars:
        clusters.append(
            _StyledCluster(
                prefix_escapes=cur_prefix,
                text="".join(cur_chars),
                width=cur_width,
                sgr_state=cur_sgr,
                osc8_open=cur_osc8,
            )
        )
      east_asian = unicodedata.east_asian_width(ch)
      cur_prefix = "".join(pending_escapes)
      pending_escapes.clear()
      cur_chars = [ch]
      cur_width = 2 if east_asian in ("W", "F") else 1
      cur_sgr = tuple(active_sgr)
      cur_osc8 = active_osc8

  if cur_chars:
    clusters.append(
        _StyledCluster(
            prefix_escapes=cur_prefix,
            text="".join(cur_chars),
            width=cur_width,
            sgr_state=cur_sgr,
            osc8_open=cur_osc8,
        )
    )
  trailing_escapes = "".join(pending_escapes)
  if not clusters:
    return (line,)

  wrapped: list[str] = []
  idx = 0
  total = len(clusters)
  while idx < total:
    if wrapped:
      while idx < total and clusters[idx].text == " ":
        idx += 1
      if idx >= total:
        break

    reopen = (
        clusters[idx - 1].osc8_open + "".join(clusters[idx - 1].sgr_state)
        if idx > 0
        else ""
    )
    end = idx
    col = 0
    last_space = -1
    while end < total and col + clusters[end].width <= max_width:
      if clusters[end].text == " ":
        last_space = end
      col += clusters[end].width
      end += 1

    if end >= total:
      body = "".join(c.prefix_escapes + c.text for c in clusters[idx:total])
      close_seq = ""
      if active_osc8:
        close_seq += "\x1b]8;;\x1b\\"
      if active_sgr:
        close_seq += "\x1b[0m"
      wrapped.append(f"{reopen}{body}{trailing_escapes}{close_seq}")
      break

    if last_space > idx:
      split_end = last_space
      next_idx = last_space + 1
    else:
      split_end = max(idx + 1, end)
      next_idx = split_end

    row_slice = clusters[idx:split_end]
    body = "".join(c.prefix_escapes + c.text for c in row_slice)
    last_cluster = row_slice[-1]
    close_seq = ""
    if last_cluster.osc8_open:
      close_seq += "\x1b]8;;\x1b\\"
    if last_cluster.sgr_state:
      close_seq += "\x1b[0m"
    wrapped.append(f"{reopen}{body}{close_seq}")
    idx = next_idx

  return tuple(wrapped) if wrapped else ("",)


def _identify_non_mermaid_code_lines(raw_lines: Sequence[str]) -> tuple[bool, ...]:
  """Marks raw lines belonging to non-Mermaid fenced code blocks."""
  flags = [False] * len(raw_lines)
  in_fence = False
  fence_char = ""
  fence_len = 0
  is_mermaid = False
  start_idx = 0

  for idx, line in enumerate(raw_lines):
    if not in_fence:
      if match := _FENCE_OPEN_RE.match(line):
        _, marker, info = match.groups()
        if marker[0] == "`" and "`" in info:
          continue
        token = info.strip().split()[0].lower() if info.strip() else ""
        in_fence = True
        fence_char = marker[0]
        fence_len = len(marker)
        is_mermaid = token == "mermaid"
        start_idx = idx
    else:
      stripped = line.strip()
      if len(stripped) >= fence_len and stripped == fence_char * len(stripped):
        if not is_mermaid:
          for code_idx in range(start_idx, idx + 1):
            flags[code_idx] = True
        in_fence = False
  return tuple(flags)


def _line_match_score(
    view_clean: str,
    view_words: Sequence[str],
    view_blank: bool,
    src_feat: _SourceLineFeature,
) -> int:
  """Scores lexical similarity between one rendered row and one source line."""
  if view_blank and src_feat.is_blank:
    return _SCORE_BOTH_BLANK
  if view_blank or src_feat.is_blank:
    return _SCORE_MISMATCH_BLANK

  if view_words:
    matched = sum(1 for word in view_words if word in src_feat.words)
    if matched == 0:
      return _SCORE_NO_MATCH
    score = matched * _SCORE_WORD_MATCH
  else:
    has_char = any(ch in src_feat.clean for ch in view_clean)
    score = _SCORE_SUBSTRING_MATCH if has_char else _SCORE_NO_MATCH

  if view_clean and view_clean == src_feat.clean:
    score += _SCORE_EXACT_MATCH
  return score


def map_rendered_lines_to_source(
    raw_markdown: str,
    rendered_lines: Sequence[str],
    spans: Sequence[CollapseSpan] = (),
) -> tuple[int, ...]:
  """Maps each rendered viewport row to its 1-based raw Markdown line number.

  Uses monotonic dynamic programming alignment across prose, headings, lists,
  and wrapped tables, followed by authoritative `CollapseSpan` pinning for
  injected Mermaid graphics or Unicode diagram blocks.

  Args:
    raw_markdown: Original unrendered Markdown source text.
    rendered_lines: Sequence of rendered terminal rows.
    spans: Injected diagram row ranges pinned to their opening fence line.

  Returns:
    Tuple of 1-based source line numbers with length `len(rendered_lines)`.

  Usage Example:
    lines = map_rendered_lines_to_source("# H1\\n\\nBody", ["H1", "", "Body"])
  """
  num_view = len(rendered_lines)
  if num_view == 0:
    return ()

  raw_lines = raw_markdown.splitlines()
  num_src = len(raw_lines)
  if num_src == 0:
    return tuple(1 for _ in range(num_view))

  code_flags = _identify_non_mermaid_code_lines(raw_lines)
  src_features: list[_SourceLineFeature] = []
  for idx, raw_line in enumerate(raw_lines):
    cleaned = raw_line.strip()
    words = frozenset(w.lower() for w in _WORD_RE.findall(raw_line))
    src_features.append(
        _SourceLineFeature(
            clean=cleaned,
            words=words,
            is_blank=not cleaned,
            is_code=code_flags[idx],
        )
    )

  min_src = [0] * num_view
  max_src = [num_src - 1] * num_view
  for span in spans:
    s_lo = max(0, min(num_src - 1, span.source_line - 1))
    end_one_based = (
        span.end_source_line if span.end_source_line > 0 else span.source_line
    )
    s_hi = max(s_lo, min(num_src - 1, end_one_based - 1))
    start_r = max(0, min(num_view, span.start_row))
    end_r = max(start_r, min(num_view, span.end_row))
    for r_idx in range(0, start_r):
      max_src[r_idx] = min(max_src[r_idx], s_lo)
    for r_idx in range(start_r, end_r):
      min_src[r_idx] = s_lo
      max_src[r_idx] = s_lo
    for r_idx in range(end_r, num_view):
      min_src[r_idx] = max(min_src[r_idx], s_hi)
  for r_idx in range(num_view):
    if min_src[r_idx] > max_src[r_idx]:
      max_src[r_idx] = min_src[r_idx]

  dp = [[_MIN_DP_SCORE] * num_src for _ in range(num_view)]
  parent = [[0] * num_src for _ in range(num_view)]

  for v_idx, rendered_line in enumerate(rendered_lines):
    v_clean = strip_terminal_escapes(rendered_line).strip()
    v_words = [w.lower() for w in _WORD_RE.findall(v_clean)]
    v_blank = not v_clean

    prev_best_score = _MIN_DP_SCORE
    prev_best_src = min_src[v_idx]
    if v_idx > 0:
      for prior_s in range(0, min_src[v_idx]):
        if dp[v_idx - 1][prior_s] > prev_best_score:
          prev_best_score = dp[v_idx - 1][prior_s]
          prev_best_src = prior_s

    for s_idx in range(min_src[v_idx], max_src[v_idx] + 1):
      feat = src_features[s_idx]
      match_score = _line_match_score(v_clean, v_words, v_blank, feat)
      if v_idx == 0:
        dp[0][s_idx] = match_score
        parent[0][s_idx] = 0
        continue

      if feat.is_code:
        candidate_score = prev_best_score
        candidate_src = prev_best_src
        if prev_best_src == s_idx - 1:
          candidate_score += _SCORE_DIAGONAL_BONUS
        if (
            len(feat.clean) > 40
            and v_clean
            and v_clean in feat.clean
            and dp[v_idx - 1][s_idx] > candidate_score
        ):
          candidate_score = dp[v_idx - 1][s_idx]
          candidate_src = s_idx
      else:
        if dp[v_idx - 1][s_idx] > prev_best_score:
          prev_best_score = dp[v_idx - 1][s_idx]
          prev_best_src = s_idx
        candidate_score = prev_best_score
        candidate_src = prev_best_src

      dp[v_idx][s_idx] = match_score + candidate_score
      parent[v_idx][s_idx] = candidate_src

      if dp[v_idx - 1][s_idx] > prev_best_score:
        prev_best_score = dp[v_idx - 1][s_idx]
        prev_best_src = s_idx

  best_final_src = min_src[num_view - 1]
  best_final_score = _MIN_DP_SCORE
  for s_idx in range(min_src[num_view - 1], max_src[num_view - 1] + 1):
    if dp[num_view - 1][s_idx] > best_final_score:
      best_final_score = dp[num_view - 1][s_idx]
      best_final_src = s_idx

  mapped_one_based = [1] * num_view
  curr_src = best_final_src
  for v_idx in range(num_view - 1, -1, -1):
    mapped_one_based[v_idx] = curr_src + 1
    if v_idx > 0:
      curr_src = parent[v_idx][curr_src]

  return tuple(mapped_one_based)
