"""Immutable configuration constants and enumerations for mdview."""

from __future__ import annotations

import enum
import types
from typing import Mapping


class DiagramMode(enum.StrEnum):
  """Rendering mode for fenced Mermaid diagrams."""

  AUTO = "auto"
  KITTY = "kitty"
  UNICODE = "unicode"
  RAW = "raw"


class TransportKind(enum.StrEnum):
  """Detected terminal transport between user display and mdview process."""

  DIRECT_KITTY = "direct_kitty"
  TMUX_KITTY_SSH = "tmux_kitty_ssh"
  MOSH = "mosh"
  GENERIC_TTY = "generic_tty"
  NON_TTY = "non_tty"


SENTINEL_PREFIX: str = "@@MDVIEWDIAG"
SENTINEL_SUFFIX: str = "@@"

DEFAULT_MAX_COLUMNS: int = 120
MIN_COLUMNS: int = 24
GUTTER_WIDTH: int = 7

DEFAULT_MDCAT_THEME: str = "dracula"
DEFAULT_BAT_THEME: str = "Dracula"
DRACULA_BG_HEX: str = "#282a36"

MERMAID_RENDER_VERSION: str = "v1_dracula_hidpi_2x"
DEFAULT_CELL_PX_WIDTH: int = 10
DEFAULT_CELL_PX_HEIGHT: int = 20
DEFAULT_VIEWPORT_ROWS: int = 45

CACHE_REL_DIR: str = ".cache/mdview"
REVIEWS_REL_DIR: str = ".local/share/mdview/reviews"
LAST_REVIEW_REL_PATH: str = ".local/share/mdview/last_review.txt"
QUEUE_REL_PATH: str = ".cache/mdview/queued_target.json"
QUEUE_TTL_SECONDS: float = 60.0

UNICODE_PLACEHOLDER_CHAR: str = "\U0010eeee"

DRACULA_SGR: Mapping[str, str] = types.MappingProxyType({
    "reset": "\x1b[0m",
    "fg": "\x1b[38;2;248;248;242m",
    "bold_fg": "\x1b[1;38;2;248;248;242m",
    "comment": "\x1b[38;2;98;114;164m",
    "cyan": "\x1b[38;2;139;233;253m",
    "green": "\x1b[38;2;80;250;123m",
    "orange": "\x1b[38;2;255;184;108m",
    "pink": "\x1b[38;2;255;121;198m",
    "purple": "\x1b[38;2;189;147;249m",
    "bold_purple": "\x1b[1;38;2;189;147;249m",
    "yellow": "\x1b[38;2;241;250;140m",
    "red": "\x1b[38;2;255;85;85m",
    "caret_bg": "\x1b[48;2;52;55;70m",
    "bar_bg": "\x1b[48;2;33;34;44m",
    "prompt_bg": "\x1b[48;2;40;42;54m",
})

SEARCH_EXCLUDED_DIRS: frozenset[str] = frozenset({
    ".git",
    ".hg",
    ".jj",
    ".citc",
    "node_modules",
    "__pycache__",
    ".cache",
    "elpa",
    "eln-cache",
    "backup",
    "target",
})
