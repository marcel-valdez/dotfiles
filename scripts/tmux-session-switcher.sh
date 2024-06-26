#!/usr/bin/env bash

if ! type fzf 2>/dev/null; then
  echo "fzf not found, cannot continue." >&2
  echo "To install: sudo apt install fzf" >&2
  exit 1
fi

preview_index_file=$(mktemp)
echo -n '0' > "${preview_index_file}"

# lists sessions and previews the current *pane*
# lists the windows + main pane
# pop-up will be 90%
preview_script=$(cat<<EOF
terminal_height=$(tmux display-message -p "#{client_height}");\
popup_height=\$((terminal_height * 90 / 100));\
preview_height=\$((popup_height-4));\
windows=\$(tmux display-message -t{1} -p '#{W:1}' | wc -m);\
height=\$((preview_height-windows-2));\
window_selection_state=\$(cat "${preview_index_file}");\
window_selection_delta=\$(printf '%+d' "\${window_selection_state}");\
preview_window_index=\$(tmux display-message -t {1}:\${window_selection_delta} -p "#{window_index}");\
tmux display-message -t{1} -p \
  "#{W:#{window_index}. #{window_name} [#{window_panes} panes] #{?#{==:#{window_index},\${preview_window_index}},<-,} !,#{window_index}. <#{window_name}> [#{window_panes} panes] #{?#{==:#{window_index},\${preview_window_index}},<-,} !}" |\
sed -e 's/[ ]+/ /g' | tr '!' '\n';\
printf '%.s─' \$(seq 1 \${COLUMNS});\
tmux capture-pane -pet {1}:\${preview_window_index} |\
  tail "-\${height}"
EOF
)
# DEBUG: echo "window_selection_delta=\${window_selection_delta},preview_window_index=\${preview_window_index},preview_index_file=${preview_index_file};\$(cat ${preview_index_file})";

shift_up_script=$(cat<<EOF
prev_value=\$(cat "${preview_index_file}");\
echo -n "\$((prev_value-1))" > "${preview_index_file}"
EOF
)
shift_down_script=$(cat<<EOF
prev_value=\$(cat "${preview_index_file}");\
echo -n "\$((prev_value+1))" > "${preview_index_file}"
EOF
)
header="TMUX Sessions
ID name"
tmux list-sessions -F '#{session_id} #{session_name}' | \
  fzf --cycle --reverse --header "${header}" \
  --bind='enter:become:tmux switch-client -t {1}' \
  --bind="shift-up:execute<${shift_up_script}>+preview<${preview_script}>" \
  --bind="shift-down:execute<${shift_down_script}>+preview<${preview_script}>" \
  --bind="up:execute(echo -n '0' >${preview_index_file})+up" \
  --bind="down:execute(echo -n '0' >${preview_index_file})+down" \
  --scroll-off=0 \
  --preview="${preview_script}" \
  --preview-window=80%,follow \
  --preview-label="Session Windows" \
  --preview-label-pos=5
