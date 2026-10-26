# Setup fzf
# ---------
if [[ ! "$PATH" == *"${HOME}/.fzf/bin"* ]]; then
  export PATH="${PATH:+${PATH}:}${HOME}/.fzf/bin"
fi

# Auto-completion
# ---------------
if [[ $- == *i* ]]; then
  if [[ -f "${HOME}/.fzf/shell/completion.bash" ]]; then
    source "${HOME}/.fzf/shell/completion.bash" 2> /dev/null
  elif [[ -f /usr/share/doc/fzf/examples/completion.bash ]]; then
    source /usr/share/doc/fzf/examples/completion.bash
  fi
fi

# Key bindings
# ------------
if [[ -f "${HOME}/.fzf/shell/key-bindings.bash" ]]; then
  source "${HOME}/.fzf/shell/key-bindings.bash"
elif [[ -f /usr/share/doc/fzf/examples/key-bindings.bash ]]; then
  source /usr/share/doc/fzf/examples/key-bindings.bash
fi

# Ensure __fzf_history__ works cleanly on macOS /bin/bash 3.2 (where
# last_hist=$(builtin history 1) inside a pipe subshell returns empty, causing
# negative history indices like "-42\t<cmd>" that stock fzf's s/^\d*\t// regex
# fails to strip).
if declare -F __fzf_history__ >/dev/null 2>&1 && command -v perl >/dev/null 2>&1; then
  __fzf_history__() {
    local output script deletefile last_hist_line
    deletefile=$(mktemp)
    last_hist_line=$(HISTTIMEFORMAT='' builtin history 1 2>/dev/null)
    script='BEGIN { getc; $/ = "\n\t"; $HISTCOUNT = $ENV{last_hist} + 1 } s/^[ *]//; s/\n/\n\t/gm; my $idx = $HISTCOUNT - $.; $idx = $. if $idx <= 0; print $idx . "\t$_" if !$seen{$_}++'
    output=$(
      set +o pipefail
      builtin fc -lnr -2147483648 2>/dev/null |
        last_hist="${last_hist_line}" command perl -n -l0 -e "$script" |
        FZF_DEFAULT_OPTS=$(__fzf_defaults "" "-n2..,.. --scheme=history --bind=ctrl-r:toggle-sort,alt-r:toggle-raw --wrap-sign '"$'\t'"↳ ' --highlight-line --bind 'shift-delete:execute-silent(cat {+f1} >> \"$deletefile\")+exclude-multi' --multi ${FZF_CTRL_R_OPTS-} --read0") \
        FZF_DEFAULT_OPTS_FILE='' $(__fzfcmd) --query "$READLINE_LINE"
    )
    if declare -F __fzf_history_delete >/dev/null 2>&1; then
      __fzf_history_delete "$deletefile"
    fi
    command rm -f "$deletefile"
    [[ -n $output ]] || return
    READLINE_LINE=$(command perl -pe 's/^[^\t]*\t//gm' <<< "$output")
    if [[ -z $READLINE_POINT ]]; then
      echo "$READLINE_LINE"
    else
      READLINE_POINT=0x7fffffff
    fi
  }
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
