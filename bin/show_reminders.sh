#!/usr/bin/env bash

REMINDERS_FILE="${HOME}/.reminders.txt"

function show_reminders {
  local contents=
  if [[ -f "${REMINDERS_FILE}" ]]; then
    contents="$(cat "${REMINDERS_FILE}")"
  fi

  if [[ -z "${contents}" ]]; then
    tmux display-message -F '#[fg=#009900]No reminders. :)'
  else
    tmux display-popup "mg '${REMINDERS_FILE}'"
  fi
}


show_reminders
