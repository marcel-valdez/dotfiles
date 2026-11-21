"""Deterministic width-bounded Unicode Mermaid renderer with Dracula SGR colors.

Renders Mermaid `flowchart` / `graph` (`TD`, `TB`, `BT`, `LR`, `RL`),
`stateDiagram` / `stateDiagram-v2`, and `sequenceDiagram` blocks into rounded
single-width Unicode box-drawing diagrams strictly bounded by `max_width`
columns, automatically transposing wide `LR` layouts to `TD`.

Usage Example:
  rows = render_mermaid_unicode('flowchart LR\\n  A["Start"] --> B["End"]', 80)
"""

from __future__ import annotations

from collections.abc import Sequence
import dataclasses
import html
import re
import textwrap

import mdview_constants


_HEADER_RE = re.compile(
    r"^(flowchart|graph)\s*(TD|TB|BT|LR|RL)?\b", re.IGNORECASE
)
_SEQ_MSG_RE = re.compile(
    r"^([A-Za-z0-9_.-]+|\".*?\")\s*"
    r"(-->>|->>|-->|->|--x|-x|--\)|-\))\s*[+-]?\s*"
    r"([A-Za-z0-9_.-]+|\".*?\")\s*:\s*(.*)$"
)
_SEQ_PARTICIPANT_RE = re.compile(
    r"^(?:participant|actor)\s+([A-Za-z0-9_.-]+|\".*?\")"
    r"(?:\s+as\s+(.+))?$",
    re.IGNORECASE,
)
_STATE_ARROW_RE = re.compile(
    r"^(\[\*\]|[A-Za-z0-9_.-]+)\s*-->\s*(\[\*\]|[A-Za-z0-9_.-]+)"
    r"(?:\s*:\s*(.+))?$"
)


@dataclasses.dataclass(frozen=True)
class FlowNode:
  """Parsed flowchart or state-diagram node.

  Attributes:
    node_id: Canonical identifier of the node.
    label: Cleaned display label text (may contain explicit newlines).
  """

  node_id: str
  label: str


@dataclasses.dataclass(frozen=True)
class FlowEdge:
  """Directed edge between two `FlowNode` identifiers.

  Attributes:
    src: Source `node_id`.
    dst: Destination `node_id`.
    label: Optional edge annotation text.
    dotted: Whether the edge uses a dotted line style (`-.->`).
  """

  src: str
  dst: str
  label: str = ""
  dotted: bool = False


@dataclasses.dataclass(frozen=True)
class SequenceMessage:
  """Single message arrow in a `sequenceDiagram`.

  Attributes:
    sender: Participant ID of sender.
    receiver: Participant ID of receiver.
    text: Message label text.
    dashed: Whether the arrow is dashed (`-->>` / `-->`).
  """

  sender: str
  receiver: str
  text: str
  dashed: bool = False


def _clean_label(raw: str) -> str:
  """Normalizes quotes, `<br/>` line breaks, and HTML entities in a label."""
  text = raw.strip()
  if len(text) >= 2 and text[0] == text[-1] and text[0] in ('"', "'"):
    text = text[1:-1]
  text = re.sub(r"(?i)<br\s*/?>", "\n", text)
  text = re.sub(r"(?i)</?[a-z0-9]+[^>]*>", "", text)
  return html.unescape(text).strip()


def _split_statements_respecting_quotes(code: str) -> list[str]:
  """Splits Mermaid body on newlines and unquoted/unbracketed semicolons."""
  statements: list[str] = []
  buf: list[str] = []
  quote_ch = ""
  bracket_depth = 0

  for ch in code:
    if quote_ch:
      buf.append(ch)
      if ch == quote_ch:
        quote_ch = ""
      continue
    if ch in ('"', "'"):
      quote_ch = ch
      buf.append(ch)
      continue
    if ch in ("[", "(", "{"):
      bracket_depth += 1
      buf.append(ch)
      continue
    if ch in ("]", ")", "}"):
      bracket_depth = max(0, bracket_depth - 1)
      buf.append(ch)
      continue
    if ch == "\n" or (ch == ";" and bracket_depth == 0):
      stmt = "".join(buf).strip()
      if stmt and not stmt.startswith("%%"):
        statements.append(stmt)
      buf = []
      continue
    buf.append(ch)

  trailing = "".join(buf).strip()
  if trailing and not trailing.startswith("%%"):
    statements.append(trailing)
  return statements


def _match_arrow_at(text: str) -> tuple[str, bool, int] | None:
  """Matches a Mermaid edge operator at `text[0:]`, returning `(lbl, dot, len)`."""
  # Inline labeled arrows: `-- label -->` or `-. label .->` or `== label ==>`.
  for pattern, is_dotted in (
      (r"^--\s+([^->\n]+?)\s+-->", False),
      (r"^-\.\s+([^.>\n]+?)\s+\.->", True),
      (r"^==\s+([^=>\n]+?)\s+==>", False),
  ):
    if m := re.match(pattern, text):
      return _clean_label(m.group(1)), is_dotted, m.end()

  # Standard + pipe-labeled arrows: `-->|label|`, `-.->`, `==>`, `---`, etc.
  if m := re.match(
      r"^(-\.->|==>|-->|---|-.-|===|--x|--o)(?:\|([^|]*)\|)?", text
  ):
    op = m.group(1)
    pipe_lbl = _clean_label(m.group(2) or "")
    return pipe_lbl, ("." in op), m.end()
  return None


def _split_edge_chain(
    stmt: str,
) -> tuple[list[str], list[tuple[str, bool]]]:
  """Splits `A[...] --> B[...] --> C` into node tokens and `(label, dotted)` ops."""
  segments: list[str] = []
  ops: list[tuple[str, bool]] = []
  quote_ch = ""
  depth = 0
  seg_start = 0
  idx = 0
  length = len(stmt)

  while idx < length:
    ch = stmt[idx]
    if quote_ch:
      if ch == quote_ch:
        quote_ch = ""
      idx += 1
      continue
    if ch in ('"', "'"):
      quote_ch = ch
      idx += 1
      continue
    if ch in ("[", "(", "{"):
      depth += 1
      idx += 1
      continue
    if ch in ("]", ")", "}"):
      depth = max(0, depth - 1)
      idx += 1
      continue

    if depth == 0:
      if matched := _match_arrow_at(stmt[idx:]):
        edge_lbl, is_dotted, consumed = matched
        segments.append(stmt[seg_start:idx].strip())
        ops.append((edge_lbl, is_dotted))
        idx += consumed
        seg_start = idx
        continue
    idx += 1

  segments.append(stmt[seg_start:].strip())
  return segments, ops


def _parse_node_token(token: str) -> tuple[str, str | None]:
  """Extracts `(node_id, optional_label)` from a single flowchart node ref."""
  cleaned = token.strip()
  if ":::" in cleaned:
    cleaned = cleaned.split(":::", 1)[0].strip()
  if not cleaned:
    return "", None
  if cleaned == "[*]":
    return "__start_end__", "●"

  m = re.match(r"^([A-Za-z0-9_.-]+)\s*(.*)$", cleaned, flags=re.DOTALL)
  if not m:
    lbl = _clean_label(cleaned)
    return lbl, lbl

  ident = m.group(1)
  rest = m.group(2).strip()
  if not rest:
    return ident, None

  for open_d, close_d in (
      ("(((", ")))"),
      ("([", "])"),
      ("[(", ")]"),
      ("[[", "]]"),
      ("((", "))"),
      ("{{", "}}"),
      ("[/", "/]"),
      ("[", "]"),
      ("{", "}"),
      ("(", ")"),
  ):
    if (
        rest.startswith(open_d)
        and rest.endswith(close_d)
        and len(rest) >= len(open_d) + len(close_d)
    ):
      inner = rest[len(open_d) : len(rest) - len(close_d)]
      return ident, _clean_label(inner)
  return ident, None


def _wrap_text_lines(text: str, width: int) -> list[str]:
  """Wraps `text` so every returned line has length `<= width`."""
  bound = max(6, width)
  wrapped: list[str] = []
  for paragraph in text.split("\n"):
    para = paragraph.strip()
    if not para:
      continue
    chunks = textwrap.wrap(
        para,
        width=bound,
        break_long_words=True,
        break_on_hyphens=False,
    )
    wrapped.extend(chunks if chunks else [""])
  return wrapped or [""]


def _render_plain_box(label: str, max_box_width: int) -> list[str]:
  """Builds uncolored Unicode rounded box lines (`╭─╮`) within `max_box_width`."""
  inner_limit = max(6, max_box_width - 4)
  lines = _wrap_text_lines(label, inner_limit)
  content_w = max(len(line) for line in lines)
  top = "╭" + ("─" * (content_w + 2)) + "╮"
  bot = "╰" + ("─" * (content_w + 2)) + "╯"
  body = [f"│ {line.center(content_w)} │" for line in lines]
  return [top, *body, bot]


def _colorize_unicode_row(plain_row: str) -> str:
  """Applies 24-bit Dracula SGR colors to box borders, arrows, and labels."""
  sgr = mdview_constants.DRACULA_SGR
  border_chars = frozenset("╭╮╰╯─│├┤┬┴┼")
  arrow_chars = frozenset("▼▲▶◀╴╶╷╵┆┈")

  out: list[str] = []
  cur_color = ""
  for ch in plain_row:
    if ch in border_chars:
      target = sgr["purple"]
    elif ch in arrow_chars:
      target = sgr["cyan"]
    elif ch == " ":
      target = ""
    else:
      target = sgr["fg"]

    if target and target != cur_color:
      out.append(target)
      cur_color = target
    out.append(ch)
  if cur_color:
    out.append(sgr["reset"])
  return "".join(out)


def _assign_ranks(
    nodes: Sequence[FlowNode],
    edges: Sequence[FlowEdge],
) -> list[list[FlowNode]]:
  """Assigns cycle-safe longest-path topological ranks to `nodes`."""
  node_map = {n.node_id: n for n in nodes}
  order_idx = {n.node_id: idx for idx, n in enumerate(nodes)}
  adj: dict[str, list[str]] = {n.node_id: [] for n in nodes}
  indeg: dict[str, int] = {n.node_id: 0 for n in nodes}

  # Ignore self-loops and back-edges pointing to earlier-declared ancestors.
  reachable_cache: dict[str, set[str]] = {n.node_id: set() for n in nodes}

  def _can_reach(start: str, goal: str) -> bool:
    stack = [start]
    visited: set[str] = set()
    while stack:
      cur = stack.pop()
      if cur == goal:
        return True
      for nxt in adj.get(cur, ()):
        if nxt not in visited:
          visited.add(nxt)
          stack.append(nxt)
    return False

  del reachable_cache
  for edge in edges:
    if edge.src == edge.dst or edge.src not in node_map or edge.dst not in node_map:
      continue
    if _can_reach(edge.dst, edge.src):
      continue
    adj[edge.src].append(edge.dst)
    indeg[edge.dst] += 1

  ranks: dict[str, int] = {n.node_id: 0 for n in nodes}
  queue = [n.node_id for n in nodes if indeg[n.node_id] == 0]
  if not queue and nodes:
    queue = [nodes[0].node_id]

  processed: set[str] = set()
  while queue:
    queue.sort(key=lambda nid: order_idx[nid])
    cur = queue.pop(0)
    if cur in processed:
      continue
    processed.add(cur)
    for nxt in adj[cur]:
      ranks[nxt] = max(ranks[nxt], ranks[cur] + 1)
      indeg[nxt] -= 1
      if indeg[nxt] <= 0:
        queue.append(nxt)

  for n in nodes:
    if n.node_id not in processed:
      ranks[n.node_id] = max(ranks.values(), default=0) + 1

  max_rank = max(ranks.values(), default=0)
  grouped: list[list[FlowNode]] = [[] for _ in range(max_rank + 1)]
  for n in nodes:
    grouped[ranks[n.node_id]].append(n)
  return [grp for grp in grouped if grp]


def _try_render_lr_horizontal(
    ranks: Sequence[Sequence[FlowNode]],
    edges: Sequence[FlowEdge],
    max_width: int,
) -> list[str] | None:
  """Renders simple single-node-per-rank `LR` chains horizontally if they fit."""
  if not ranks or any(len(r) != 1 for r in ranks):
    return None
  arrow_gap = 5  # " ──▶ "
  num_cols = len(ranks)
  avail_for_boxes = max_width - arrow_gap * (num_cols - 1)
  if avail_for_boxes < num_cols * 12:
    return None

  # Measure natural single-line width first; allow modest wrapping only if wide.
  boxes: list[list[str]] = []
  per_box_cap = max(12, avail_for_boxes // num_cols)
  for rank in ranks:
    node = rank[0]
    longest_word = max((len(w) for w in node.label.split()), default=1)
    if longest_word + 4 > per_box_cap or len(node.label) > per_box_cap * 2:
      return None
    boxes.append(_render_plain_box(node.label, per_box_cap))

  total_w = sum(len(b[0]) for b in boxes) + arrow_gap * (num_cols - 1)
  if total_w > max_width:
    return None

  del edges
  max_h = max(len(b) for b in boxes)
  mid_row = max_h // 2
  canvas: list[str] = []
  for r_idx in range(max_h):
    row_parts: list[str] = []
    for c_idx, box in enumerate(boxes):
      box_w = len(box[0])
      top_pad = (max_h - len(box)) // 2
      local_r = r_idx - top_pad
      cell_str = box[local_r] if 0 <= local_r < len(box) else (" " * box_w)
      row_parts.append(cell_str)
      if c_idx < num_cols - 1:
        row_parts.append(" ──▶ " if r_idx == mid_row else "     ")
    canvas.append("".join(row_parts).rstrip())
  return canvas


def _render_td_flowchart(
    ranks: Sequence[Sequence[FlowNode]],
    edges: Sequence[FlowEdge],
    max_width: int,
) -> list[str]:
  """Renders `ranks` top-down, packing sibling nodes into width-safe sub-rows."""
  subrows: list[list[FlowNode]] = []
  col_gap = 2
  min_side_by_side_box = 22

  for rank in ranks:
    max_per_row = max(
        1, (max_width + col_gap) // (min_side_by_side_box + col_gap)
    )
    for start in range(0, len(rank), max_per_row):
      subrows.append(list(rank[start : start + max_per_row]))

  edge_labels_by_dst: dict[str, list[str]] = {}
  for edge in edges:
    if edge.label:
      edge_labels_by_dst.setdefault(edge.dst, []).append(edge.label)

  canvas: list[str] = []
  prev_centers: list[int] = []

  for s_idx, group in enumerate(subrows):
    k = len(group)
    per_box_max = (max_width - col_gap * (k - 1)) // k
    boxes = [_render_plain_box(node.label, per_box_max) for node in group]
    widths = [len(b[0]) for b in boxes]
    row_total_w = sum(widths) + col_gap * (k - 1)
    left_margin = max(0, (max_width - row_total_w) // 2)

    cur_centers: list[int] = []
    cursor_x = left_margin
    for w in widths:
      cur_centers.append(cursor_x + w // 2)
      cursor_x += w + col_gap

    if s_idx > 0 and prev_centers and cur_centers:
      # Draw clean connector wires between previous subrow and current subrow.
      if len(prev_centers) == 1 and len(cur_centers) == 1 and prev_centers[0] == cur_centers[0]:
        cx = cur_centers[0]
        canvas.append((" " * cx) + "│")
        if lbls := edge_labels_by_dst.get(group[0].node_id):
          lbl = ", ".join(lbls)[: max(8, max_width - 4)]
          lx = max(0, min(max_width - len(lbl), cx - len(lbl) // 2))
          canvas.append((" " * lx) + lbl)
        canvas.append((" " * cx) + "▼")
      else:
        all_x = prev_centers + cur_centers
        lo_x, hi_x = min(all_x), max(all_x)
        wire = [" "] * max_width
        for x in range(lo_x, min(max_width, hi_x + 1)):
          wire[x] = "─"
        for px in prev_centers:
          if 0 <= px < max_width:
            if px == lo_x and px not in cur_centers:
              wire[px] = "╰"
            elif px == hi_x and px not in cur_centers:
              wire[px] = "╯"
            elif px in cur_centers:
              wire[px] = "┼"
            else:
              wire[px] = "┴"
        for cx in cur_centers:
          if 0 <= cx < max_width and wire[cx] == "─":
            if cx == lo_x:
              wire[cx] = "╭"
            elif cx == hi_x:
              wire[cx] = "╮"
            else:
              wire[cx] = "┬"
        canvas.append("".join(wire).rstrip())
        arrows = [" "] * max_width
        for cx in cur_centers:
          if 0 <= cx < max_width:
            arrows[cx] = "▼"
        canvas.append("".join(arrows).rstrip())

    max_h = max(len(b) for b in boxes)
    for r_i in range(max_h):
      line_parts = [" " * left_margin]
      for b_i, box in enumerate(boxes):
        w = widths[b_i]
        line_parts.append(box[r_i] if r_i < len(box) else (" " * w))
        if b_i < k - 1:
          line_parts.append(" " * col_gap)
      canvas.append("".join(line_parts).rstrip())

    prev_centers = cur_centers

  return canvas


def _parse_and_render_flowchart(
    statements: Sequence[str],
    direction: str,
    max_width: int,
) -> list[str] | None:
  """Parses flowchart/state statements into nodes & edges and lays them out."""
  nodes_by_id: dict[str, FlowNode] = {}
  edges: list[FlowEdge] = []

  def _upsert_node(nid: str, lbl: str | None) -> None:
    if not nid:
      return
    existing = nodes_by_id.get(nid)
    if existing is None:
      nodes_by_id[nid] = FlowNode(node_id=nid, label=lbl if lbl else nid)
    elif lbl and existing.label == nid:
      nodes_by_id[nid] = FlowNode(node_id=nid, label=lbl)

  for stmt in statements:
    low = stmt.lower()
    if low.startswith((
        "subgraph ",
        "end",
        "style ",
        "classdef ",
        "class ",
        "click ",
        "linkstyle ",
        "direction ",
    )):
      if low == "end" or not low.startswith("subgraph "):
        continue
      continue

    if m_state := _STATE_ARROW_RE.match(stmt):
      s_raw, d_raw, e_lbl = m_state.groups()
      s_id, s_lbl = _parse_node_token(s_raw)
      d_id, d_lbl = _parse_node_token(d_raw)
      _upsert_node(s_id, s_lbl)
      _upsert_node(d_id, d_lbl)
      edges.append(FlowEdge(src=s_id, dst=d_id, label=_clean_label(e_lbl or "")))
      continue

    segments, ops = _split_edge_chain(stmt)
    if not ops:
      nid, nlbl = _parse_node_token(segments[0])
      if nid and nlbl is not None:
        _upsert_node(nid, nlbl)
      continue

    parsed_groups: list[list[str]] = []
    for seg in segments:
      group_ids: list[str] = []
      for sub_tok in seg.split("&"):
        nid, nlbl = _parse_node_token(sub_tok)
        if nid:
          _upsert_node(nid, nlbl)
          group_ids.append(nid)
      parsed_groups.append(group_ids)

    for idx, (edge_lbl, is_dotted) in enumerate(ops):
      for s_id in parsed_groups[idx]:
        for d_id in parsed_groups[idx + 1]:
          edges.append(
              FlowEdge(src=s_id, dst=d_id, label=edge_lbl, dotted=is_dotted)
          )

  if not nodes_by_id:
    return None

  node_list = list(nodes_by_id.values())
  ranks = _assign_ranks(node_list, edges)
  if direction in ("LR", "RL"):
    if lr_rows := _try_render_lr_horizontal(ranks, edges, max_width):
      return lr_rows
  return _render_td_flowchart(ranks, edges, max_width)


def _render_sequence_diagram(
    statements: Sequence[str],
    max_width: int,
) -> list[str] | None:
  """Renders a `sequenceDiagram` strictly within `max_width` columns."""
  participants: dict[str, str] = {}
  messages: list[SequenceMessage] = []

  def _ensure_actor(raw_id: str, alias: str | None = None) -> str:
    pid = _clean_label(raw_id)
    if pid not in participants:
      participants[pid] = _clean_label(alias) if alias else pid
    elif alias:
      participants[pid] = _clean_label(alias)
    return pid

  for stmt in statements:
    if m_part := _SEQ_PARTICIPANT_RE.match(stmt):
      _ensure_actor(m_part.group(1), m_part.group(2))
      continue
    if m_msg := _SEQ_MSG_RE.match(stmt):
      s_raw, arrow, r_raw, msg_txt = m_msg.groups()
      s_id = _ensure_actor(s_raw)
      r_id = _ensure_actor(r_raw)
      messages.append(
          SequenceMessage(
              sender=s_id,
              receiver=r_id,
              text=_clean_label(msg_txt),
              dashed=arrow.startswith("--"),
          )
      )

  if not participants:
    return None

  actor_ids = list(participants.keys())
  n_actors = len(actor_ids)
  col_gap = 2
  per_actor_w = (max_width - col_gap * (n_actors - 1)) // n_actors

  # If too many actors to fit lifelines side-by-side, render compact stacked steps.
  if per_actor_w < 12:
    stacked: list[str] = []
    for msg in messages:
      line_txt = f"{participants[msg.sender]} -> {participants[msg.receiver]}: {msg.text}"
      stacked.extend(_render_plain_box(line_txt, max_width))
    return stacked

  header_boxes = [
      _render_plain_box(participants[aid], per_actor_w) for aid in actor_ids
  ]
  widths = [len(b[0]) for b in header_boxes]
  centers: list[int] = []
  cx = 0
  for w in widths:
    centers.append(cx + w // 2)
    cx += w + col_gap
  actor_col = {aid: centers[i] for i, aid in enumerate(actor_ids)}
  total_w = max(1, cx - col_gap)

  max_hdr_h = max(len(b) for b in header_boxes)
  canvas: list[str] = []
  for r_i in range(max_hdr_h):
    parts: list[str] = []
    for a_i, box in enumerate(header_boxes):
      w = widths[a_i]
      parts.append(box[r_i] if r_i < len(box) else (" " * w))
      if a_i < n_actors - 1:
        parts.append(" " * col_gap)
    canvas.append("".join(parts).rstrip())

  def _lifeline_row() -> list[str]:
    row = [" "] * total_w
    for c_pos in centers:
      if 0 <= c_pos < total_w:
        row[c_pos] = "│"
    return row

  for msg in messages:
    canvas.append("".join(_lifeline_row()).rstrip())
    sx = actor_col[msg.sender]
    rx = actor_col[msg.receiver]
    if sx == rx:
      lbl_lines = _wrap_text_lines(msg.text, max(8, max_width - sx - 6))
      for l_txt in lbl_lines:
        row = _lifeline_row()
        text_start = min(max_width - len(l_txt), sx + 2)
        line_str = "".join(row[:text_start]).ljust(text_start) + l_txt
        canvas.append(line_str[:max_width].rstrip())
      continue

    lo, hi = min(sx, rx), max(sx, rx)
    span_w = max(8, hi - lo - 3)
    for l_txt in _wrap_text_lines(msg.text, span_w):
      row = _lifeline_row()
      start_x = lo + max(1, (hi - lo - len(l_txt)) // 2)
      for idx, ch in enumerate(l_txt):
        pos = start_x + idx
        if 0 <= pos < len(row):
          row[pos] = ch
      canvas.append("".join(row).rstrip())

    arrow_row = _lifeline_row()
    shaft = "┈" if msg.dashed else "─"
    for x in range(lo + 1, hi):
      if 0 <= x < len(arrow_row):
        arrow_row[x] = shaft
    if sx < rx and hi - 1 < len(arrow_row):
      arrow_row[hi - 1] = "▶"
    elif rx < sx and lo + 1 < len(arrow_row):
      arrow_row[lo + 1] = "◀"
    canvas.append("".join(arrow_row).rstrip())

  canvas.append("".join(_lifeline_row()).rstrip())
  return canvas


def render_mermaid_unicode(
    mermaid_code: str,
    max_width: int = 80,
) -> tuple[str, ...]:
  """Renders `mermaid_code` into Dracula-colored Unicode lines `<= max_width`.

  Args:
    mermaid_code: Raw Mermaid diagram text inside fences.
    max_width: Strict maximum visible terminal cell width per row.

  Returns:
    Tuple of ANSI-colored Unicode box-drawing rows, each `<= max_width` cells.

  Usage Example:
    lines = render_mermaid_unicode("flowchart TD\\n  A --> B", max_width=80)
  """
  bounded_w = max(20, max_width)
  cleaned = re.sub(r"%%\{.*?\}%%", "", mermaid_code, flags=re.DOTALL).strip()
  statements = _split_statements_respecting_quotes(cleaned)
  if not statements:
    return ()

  first = statements[0].strip()
  plain_rows: list[str] | None = None

  if m_hdr := _HEADER_RE.match(first):
    direction = (m_hdr.group(2) or "TD").upper()
    plain_rows = _parse_and_render_flowchart(
        statements[1:], direction=direction, max_width=bounded_w
    )
  elif first.lower().startswith("statediagram"):
    plain_rows = _parse_and_render_flowchart(
        statements[1:], direction="TD", max_width=bounded_w
    )
  elif first.lower().startswith("sequencediagram"):
    plain_rows = _render_sequence_diagram(statements[1:], max_width=bounded_w)

  if not plain_rows:
    # Graceful bounded fallback box for unsupported syntax (pie, xychart, etc.).
    plain_rows = _render_plain_box(cleaned, bounded_w)

  clipped = [row[:bounded_w] for row in plain_rows]
  return tuple(_colorize_unicode_row(row) for row in clipped)
