#!/usr/bin/env bash

logs_file="$1"

popup_cmd=$(cat<<EOF
tmux display-popup -h 90% -w 90% -E "cat ${logs_file} | batcat --highlight-line={1} --color=always | less -R +{1}"
EOF
         )

preview_cmd=$(cat<<EOF
awk '{print NR, \$0}' ${logs_file} | rg --color=always --context=20 {}
EOF
           )

awk '{print NR, $0}' "${logs_file}" | fzf --bind="ctrl-/:execute(${popup_cmd})"  --preview="${preview_cmd}" --preview-window=up
