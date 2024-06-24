#!/usr/bin/env bash

# lists sessions and previews the current *pane*
# lists the windows + main pane
# pop-up will be 90%
preview_script=$(cat<<EOF
terminal_height=$(tmux display-message -p "#{client_height}");\
popup_height=\$((terminal_height * 90 / 100));\
preview_height=\$((popup_height-4));\
windows=\$(tmux display-message -t{1} -p '#{W:1}' | wc -m);\
height=\$((preview_height-windows));\
tmux display-message -t{1} -p \
  '#{W:#{window_index}:#{window_name} ,#{window_index}:<#{window_name}> }' |\
  sed -e 's/[ ]+/ /g' | tr ' ' '\n';\
printf '%.s─' \$(seq 1 \${COLUMNS});\
tmux capture-pane -pet {1} |\
  tail "-\${height}"
EOF
              )
header="TMUX Sessions
ID name"
tmux list-sessions -F '#{session_id} #{session_name}' | \
  fzf --reverse --header "${header}" \
  --bind='enter:become:tmux switch-client -t {1}'  \
  --scroll-off=0 \
  --preview="${preview_script}" \
  --preview-window=80%,follow
