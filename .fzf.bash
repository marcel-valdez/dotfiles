# Setup fzf
# ---------
if [[ ! "$PATH" == */home/marcel/modules/fzf/bin* ]]; then
  export PATH="/home/marcel/modules/fzf/bin:${PATH:+${PATH}:}"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/home/marcel/modules/fzf/shell/completion.bash" 2> /dev/null

# Key bindings
# ------------
source "/home/marcel/modules/fzf/shell/key-bindings.bash"

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
  --header 'Press CTRL-Y to copy command into clipboard'"

export FZF_ALT_C_OPTS="--preview 'tree -C {}'
--preview-window hidden
--bind 'ctrl-/:toggle-preview'
"
