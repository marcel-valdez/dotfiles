#!/usr/bin/env bash
HOST=127.0.0.1
PORT=3333

NUM=$(netstat -tlpn 2>/dev/null | grep -c " ${HOST}:${PORT} ")
if [ ${NUM} -gt 0 ]; then
  exit
fi

while [ true ]; do
  nc -l ${HOST} ${PORT} | xclip -selection clipboard
  echo "$(today) $(date '+%H:%M:%S'): Copied remote SSH clipboard." &>> /tmp/clipboard-daemon.log
done
