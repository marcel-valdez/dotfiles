#!/usr/bin/env bash

command="$@"

ctrl_c_pressed=

function ctrl_c_handler {
  ctrl_c_pressed=1
  echo "[ctrl+c] pressed, exiting." >&2
  exit 0
}

trap ctrl_c_handler SIGINT

while true; do
  "$@"
  echo "Command '$*' finished with exit code: $?" >&2
  echo  "Press ctrl+c to exit, any other key to retry: '$*'" >&2
  read -n 1 -s -r >&2
done
