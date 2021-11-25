#!/usr/bin/env bash

pane="$1"
tmux send-key -t "${pane}" '\r'
tmux send-key -t "${pane}" 'Enter'
