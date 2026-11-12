# Setup fzf
# ---------
# Ensure ~/.fzf/bin is at the end of PATH so /usr/bin/fzf and
# /usr/bin/fzf-tmux (0.73.1) take precedence on gLinux even if ~/.bashrc
# prepended ~/.fzf/bin earlier, while keeping ~/.fzf/bin as a fallback on gMac.
if [[ -d "${HOME}/.fzf/bin" ]]; then
  _fzf_target=":${HOME}/.fzf/bin:"
  _fzf_path=":${PATH-}:"
  while [[ "${_fzf_path}" == *"${_fzf_target}"* ]]; do
    _fzf_path="${_fzf_path%%"${_fzf_target}"*}:${_fzf_path#*"${_fzf_target}"}"
  done
  _fzf_path="${_fzf_path#:}"
  _fzf_path="${_fzf_path%:}"
  export PATH="${_fzf_path:+${_fzf_path}:}${HOME}/.fzf/bin"
  unset _fzf_target _fzf_path
fi

# Auto-completion & Key bindings
# ------------------------------
unset _fzf_completion_loader
_fzf_legacy_mode=0
if declare -F ble/function#push >/dev/null 2>&1; then
  ble/function#push bind :
fi
if [[ -f /usr/share/doc/fzf/examples/key-bindings.bash ]]; then
  if ! command grep -q 'toggle-raw' /usr/share/doc/fzf/examples/key-bindings.bash 2>/dev/null; then
    _fzf_legacy_mode=1
  fi
  if [[ $- == *i* && -f /usr/share/doc/fzf/examples/completion.bash ]]; then
    source /usr/share/doc/fzf/examples/completion.bash || true
  fi
  source /usr/share/doc/fzf/examples/key-bindings.bash || true
else
  _fzf_bash_init="$(command fzf --bash 2>/dev/null || true)"
  if [[ -n "${_fzf_bash_init}" ]]; then
    if [[ "${_fzf_bash_init}" != *toggle-raw* ]]; then
      _fzf_legacy_mode=1
    fi
    eval "${_fzf_bash_init}" || true
  else
    _fzf_legacy_mode=1
    if [[ $- == *i* && -f "${HOME}/.fzf/shell/completion.bash" ]]; then
      source "${HOME}/.fzf/shell/completion.bash" 2>/dev/null || true
    fi
    if [[ -f "${HOME}/.fzf/shell/key-bindings.bash" ]]; then
      source "${HOME}/.fzf/shell/key-bindings.bash" || true
    fi
  fi
  unset _fzf_bash_init
fi
if declare -F ble/function#pop >/dev/null 2>&1; then
  ble/function#pop bind
fi

# Guard `builtin printf '\e[5n'` (Device Status Report 5) in fzf >= 0.53
# completion helpers (`_fzf_complete` and `__fzf_generic_path_completion`).
# Upstream fzf switched from `printf '\e[5n'` to `builtin printf '\e[5n'`,
# bypassing ble.sh's `ble/function#push printf` wrapper in
# `contrib/integration/fzf.common.bash` while ble.sh still suppresses
# `bind '"\e[0n": redraw-current-line'`. When ble.sh is attached, the terminal's
# `\e[0n` (`dsr0`) reply both aborts candidate insertion in
# `ble/complete/check-cancel` and triggers the `unbound keyseq: dsr0` bell.
for _fzf_dsr_fn in _fzf_complete __fzf_generic_path_completion; do
  if declare -F "${_fzf_dsr_fn}" >/dev/null 2>&1; then
    _fzf_fn_def="$(declare -f "${_fzf_dsr_fn}")"
    if [[ "${_fzf_fn_def}" == *'\e[5n'* && "${_fzf_fn_def}" != *'_ble_attached'* ]]; then
      eval "$(builtin printf '%s\n' "${_fzf_fn_def}" | command sed 's/builtin printf '\''\\e\[5n'\''/[[ -n ${_ble_attached-} ]] || &/g')"
    fi
  fi
done
unset _fzf_dsr_fn _fzf_fn_def

# Unconditional set -e-safe __fzf_defaults() and non-interactive/legacy helpers
__fzf_defaults() {
  if [[ "${_fzf_legacy_mode:-0}" == "1" ]]; then
    builtin printf '%s\n' "--height ${FZF_TMUX_HEIGHT:-40%} --bind=ctrl-z:ignore $1"
  else
    builtin printf '%s\n' "--height ${FZF_TMUX_HEIGHT:-40%} --min-height 20+ --bind=ctrl-z:ignore $1"
  fi
  if [[ -n ${FZF_DEFAULT_OPTS_FILE-} && -r ${FZF_DEFAULT_OPTS_FILE} ]]; then
    command cat "${FZF_DEFAULT_OPTS_FILE}"
  fi
  builtin printf '%s\n' "${FZF_DEFAULT_OPTS-} $2"
}

if ! declare -F __fzfcmd >/dev/null 2>&1; then
  __fzfcmd() {
    [[ -n "${TMUX_PANE-}" ]] && { [[ "${FZF_TMUX:-0}" != 0 ]] || [[ -n "${FZF_TMUX_OPTS-}" ]]; } &&
      echo "fzf-tmux ${FZF_TMUX_OPTS:--d${FZF_TMUX_HEIGHT:-40%}} -- " || echo "fzf"
  }
fi

if ! declare -F __fzf_history_delete >/dev/null 2>&1; then
  __fzf_history_delete() {
    [[ -s ${1-} ]] || return 0
    local offsets offset
    offsets=$(command sort -rn "$1" | command uniq)
    for offset in $offsets; do
      builtin history -d "$offset" 2>/dev/null || true
    done
    if [[ -n "$offsets" ]] && shopt -q histappend; then
      builtin history -w 2>/dev/null || true
    fi
  }
fi

# Ensure __fzf_history__ works cleanly on macOS /bin/bash 3.2 (where
# last_hist=$(builtin history 1) inside a pipe subshell returns empty, causing
# negative history indices like "-42\t<cmd>" that stock fzf's s/^\d*\t// regex
# fails to strip) and under set -euo pipefail.
if command -v perl >/dev/null 2>&1; then
  __fzf_history__() {
    local output script deletefile last_hist_num=0 hist_opts
    deletefile=$(mktemp)
    if [[ $(HISTTIMEFORMAT='' builtin history 1 2>/dev/null) =~ [[:digit:]]+ ]]; then
      last_hist_num="${BASH_REMATCH[0]}"
    fi
    script='BEGIN { getc; $/ = "\n\t"; $HISTCOUNT = $ENV{last_hist} + 1 } s/^[ *]//; s/\n/\n\t/gm; my $idx = $HISTCOUNT - $.; $idx = $. if $idx <= 0; print $idx . "\t$_" if !$seen{$_}++'
    if [[ "${_fzf_legacy_mode:-0}" == "1" ]]; then
      hist_opts="-n2..,.. --scheme=history --bind=ctrl-r:toggle-sort --multi ${FZF_CTRL_R_OPTS-} --read0"
    else
      hist_opts="-n2..,.. --scheme=history --bind=ctrl-r:toggle-sort,alt-r:toggle-raw --wrap-sign '"$'\t'"↳ ' --highlight-line --bind 'shift-delete:execute-silent(cat {+f1} >> \"$deletefile\")+exclude-multi' --multi ${FZF_CTRL_R_OPTS-} --read0"
    fi
    output=$(
      set +o pipefail
      builtin fc -lnr -2147483648 2>/dev/null |
        last_hist="${last_hist_num}" command perl -n -l0 -e "$script" |
        FZF_DEFAULT_OPTS=$(__fzf_defaults "" "${hist_opts}") \
        FZF_DEFAULT_OPTS_FILE='' $(__fzfcmd) --query "${READLINE_LINE-}"
    ) || true
    if [[ -s "$deletefile" ]] && declare -F __fzf_history_delete >/dev/null 2>&1; then
      __fzf_history_delete "$deletefile" || true
    fi
    command rm -f "$deletefile"
    [[ -n $output ]] || return 0
    READLINE_LINE=$(command perl -pe 's/^[^\t]*\t//gm' <<< "$output")
    if [[ -z ${READLINE_POINT-} ]]; then
      echo "$READLINE_LINE"
    else
      READLINE_POINT=0x7fffffff
    fi
  }
fi

# Re-apply ble.sh fzf integration advice wrappers and widget bindings when
# ~/.fzf.bash is re-sourced inside an already-initialized live ble.sh session.
if declare -F ble/util/import/is-loaded >/dev/null 2>&1; then
  for _ble_mod in integration/fzf-completion integration/fzf-key-bindings; do
    if ble/util/import/is-loaded "${_ble_mod}"; then
      if [[ "${_ble_mod}" == "integration/fzf-completion" ]]; then
        _ble_contrib_fzf_adjust_dynamic_completion=
      fi
      ret=""
      if ble/util/import/search "${_ble_mod}" &&
        ble/util/import/encode-filename "${ret}"; then
        builtin unset -f "ble/util/import/guard:${ret}" 2>/dev/null || true
        ble-import "${_ble_mod}" || true
      fi
    fi
  done
  unset _ble_mod ret
fi

export FZF_DEFAULT_OPTS="-m
--bind \"ctrl-y:execute-silent(echo -n '{}' | tmux load-buffer -w -)+execute-silent(tmux display-message 'Copied to clipboard')\"
--bind 'ctrl-space:select-all'
"

export FZF_CTRL_R_OPTS="-m
  --preview 'echo {}' --preview-window up:3:hidden:wrap
  --bind 'ctrl-/:toggle-preview'
  --bind \"ctrl-y:execute-silent(echo -n '{2..}' | tmux load-buffer -w -)+execute-silent(tmux display-message 'Copied to clipboard')\"
  --color header:italic
  --header 'CTRL-Y:Copy to clipboard, CTRL-/:Toggle preview'"

export FZF_ALT_C_OPTS="--preview 'tree -C {}'
  --preview-window hidden
  --bind \"ctrl-y:execute-silent(echo -n '{}' | tmux load-buffer -w -)+execute-silent(tmux display-message 'Copied to clipboard')\"
  --bind 'ctrl-/:toggle-preview'
  --header 'CTRL-Y:Copy to clipboard, CTRL-/:Toggle preview'
"
