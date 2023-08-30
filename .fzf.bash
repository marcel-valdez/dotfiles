# Setup fzf
# ---------
if [[ ! "$PATH" == *"${HOME}/.fzf/bin"* ]]; then
  export PATH="${PATH:+${PATH}:}/home/marcelvaldez/.fzf/bin"
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

export FZF_DEFAULT_OPTS='-m '"
--bind 'ctrl-y:execute-silent(echo -n {} | xclip -sel clip)+abort'
"

if [[ -n "${TMUX}" ]]; then
    export FZF_DEFAULT_OPTS="${FZF_DEFAULT_OPTS}"' --height 80%'
fi

export FZF_CTRL_R_OPTS="-m
  --preview 'echo {}' --preview-window up:3:hidden:wrap
  --bind 'ctrl-/:toggle-preview'
  --bind 'ctrl-y:execute-silent(echo -n {2..} | xclip -sel clip)+abort'
  --color header:italic
  --header 'CTRL-Y:Copy to clipboard, CTRL-/:Toggle preview'"

export FZF_ALT_C_OPTS="--preview 'tree -C {}'
--preview-window hidden
--bind 'ctrl-/:toggle-preview'
--header 'CTRL-/: Toggle preview'
"
