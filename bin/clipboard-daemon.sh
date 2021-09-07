#!/usr/bin/env bash
HOST=127.0.0.1
PORT=3333

NUM=$(netstat -tlpn 2>/dev/null | grep -c " ${HOST}:${PORT} ")
if [ ${NUM} -gt 0 ]; then
  exit
fi

displays_array=("0")
if [[ -e /tmp/.X11-unix/ ]]; then
  displays=$(ls /tmp/.X11-unix/ | sed 's/X//g')
  readarray -t displays_array <<< ${displays}
fi

while [ true ]; do
  paste_buffer=$(nc -l "${HOST}" "${PORT}") # wait for next paste to clipboard
  for display in ${displays_array[@]} # paste to all available displays
  do
    echo ${paste_buffer} | xclip -selection clipboard -display ":${display}"
    if [[ $? -gt 0 ]]; then
      echo "$(today) $(date '+%H:%M:%S'): Failed to copy contents to \
remote SSH clipboard at display :${display}" &>> /tmp/clipboard-daemon.log
    fi
  done
  echo "$(today) $(date '+%H:%M:%S'): Copied remote SSH clipboard." &>> /tmp/clipboard-daemon.log
done