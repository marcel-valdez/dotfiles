#!/usr/bin/env bash
HOST=127.0.0.1
PORT=3333

NUM=$(netstat -tlpn 2>/dev/null | grep -c " ${HOST}:${PORT} ")
if [ ${NUM} -gt 0 ]; then
  exit
fi

while [ true ]; do
  displays=$(ls /tmp/.X11-unix/ | sed 's/X//g')
  readarray -t displays_array <<< ${displays}
  paste_buffer=$(nc -l "${HOST}" "${PORT}") # wait for next paste to clipboard
  for display in ${displays_array[@]} # paste to all available displays
  do
    xclip -selection clipboard -display ":${display}" <<EOF
${paste_buffer}
EOF
    if [[ $? -gt 0 ]]; then
      echo "$(today) $(date '+%H:%M:%S'): Failed to copy contents to \
remote SSH clipboard at display :${display}" &>> /tmp/clipboard-daemon.log
    fi
  done
  echo "$(today) $(date '+%H:%M:%S'): Copied remote SSH clipboard." &>> /tmp/clipboard-daemon.log
done
