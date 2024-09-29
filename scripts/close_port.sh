#!/usr/bin/env bash

PORT="$1"

if [[ -z "${PORT}" ]]; then
  echo "A port must be provided as the single argument." >&2
  exit 1
fi

OWNER_PID=$(sudo lsof -t -i:"${PORT}")

if [[ -z "${OWNER_PID}" ]]; then
  echo "Port ${PORT} isn't open or not owned by any process." >&2
  exit 0
fi

echo "Killing process ${OWNER_PID} which has port ${PORT} open" >&2
kill "${OWNER_PID}"

sleep "0.25s"

if kill -0 "${OWNER_PID}" &>/dev/null; then
  echo "Process ${OWNER_PID} did not die with normal kill command." >&2
  echo "we will proceed to kill with -9 signal" >&2
  kill -9 "${OWNER_PID}"
fi
