#!/usr/bin/env bash

source "${HOME}/.bash_functions"
source "${HOME}/.googlerc.d/.googlerc"

GCLOUD_FOLDERS=("notes")
GCLOUD_HOST="${USER}.c.googlers.com"
OFFICE_HOST="${USER}.mtv.corp.google.com"
LAPTOP_HOST="${USER}-glaptop"

REVERSE_TUNNEL_PORT=3333
CLIPBOARD_DAEMON_BIN="${HOME}/bin/clipboard-daemon.sh"

function remote_cmd() {
  local host="$1"
  shift
  ssh -ASnone -o LogLevel=QUIET -t "${USER}@${host}" "$@"
}

function is_folder_mounted {
  local _folder="$1"
  mountpoint -q "${HOME}/${_folder}"
}

function mount_remote_gcloud_folders {
  echo "Re-mounting (if necessary) folders from host ${USER}.c.googlers.com"
  for folder in "${GCLOUD_FOLDERS[@]}"; do
    if ! is_folder_mounted "${folder}"; then
      if ! [[ -e "${HOME}/${folder}" ]]; then
        mkdir "${HOME}/${folder}"
      fi
      local remote_folder="/usr/local/google/home/${USER}/${folder}"
      echo "Mounting ${GCLOUD_HOST}:${remote_folder} on ${HOME}/${folder}"
      sshfs -o reconnect "${USER}@${GCLOUD_HOST}:${remote_folder}" "${HOME}/${folder}"
    fi
    # attempt to list directory contents in order to force reconnect of the SSHFS mount
    ls "${HOME}/${folder}" &>/dev/null
  done
}

function check_gcert_ssh {
  gcertstatus -check_ssh=true -check_loas2=false "$@"
}

function get_gcert_ssh_hours_remaining {
  check_gcert_ssh | grep -oP "[0-9]+(?=h\s[0-9]+m)"
}

function refresh_gcert {
  if ! check_gcert_ssh -quiet=true; then
    echo "Refreshing gcert, as it is invalid now."
    gcert
  else
    local retries=3
    local remaining_hrs=
    while [[ ${retries} -gt 0 ]] && [[ -z "${remaining_hrs}" ]]; do
      remaining_hrs=$(get_gcert_ssh_hours_remaining)
      retries=$((retries-1))
    done

    if [[ "${remaining_hrs}" -lt 8 ]]; then
      echo "Less than 8 hr remaining (${remaining_hrs} hr left) in gcert. Refreshing now."
      gcert
    fi
  fi
}

function is_gcloud_host {
  [[ "${HOSTNAME}" == "${GCLOUD_HOST}" ]]
}

function is_office_host {
  [[ "${HOSTNAME}" == "${OFFICE_HOST}" ]]
}

function is_laptop_host {
  [[ "${HOSTNAME}" == "${LAPTOP_HOST}" ]]
}

function is_reverse_tunnel_setup() {
  cloud_ssh_cmd netstat -tulpen | grep ":${REVERSE_TUNNEL_PORT} " &>/dev/null
}

function is_clipboard_daemon_running() {
  pgrep -af "${CLIPBOARD_DAEMON_BIN}" &>/dev/null
}

function setup_reverse_tunnel {
  # Assumes that the gcloud_tunnel parameters are specified in ~/.ssh/config,
  # otherwise here we'd specify:
  # ssh -f -N -R ${REVERSE_TUNNEL_PORT}:localhost:${REVERSE_TUNNEL_PORT} ${USER}@${GCLOUD_HOST}
  # See: go/effective-ssh
  ssh -f -N gcloud_tunnel
}

function exit_remote_reverse_tunnel {
  if is_laptop_host; then
    if remote_cmd "${USER}@${OFFICE_HOST}" ssh gcloud_tunnel -O check &>/dev/null; then
      echo "Shutting down reverse tunnel for ${OFFICE_HOST}"
      remote_cmd "${USER}@${OFFICE_HOST}" ssh gcloud_tunnel -O exit
    fi
  fi
  # We have no way of exiting the reverse tunnel of the laptop than to grep for
  # SSH processes on the cloud instance and then kill the process corresponding
  # to the laptop's SSH session.
}

function restart_master_session {
  # Stop the master session if it already exists
  if ssh "${GCLOUD_HOST}" -O check &>/dev/null; then
     echo "Stopping the previous master SSH session."
     ssh "${GCLOUD_HOST}" -O exit
  fi
  # Initiate an SSH session in order to start the Master SSH Session
  # assumes there is a master session configuration.
  echo "Starting a new master SSH session."
  ssh -N "${USER}@${CLOUD_HOST}"
}

function main {
  refresh_gcert
  if ! is_gcloud_host; then
    restart_master_session
    mount_remote_gcloud_folders
    if ! is_clipboard_daemon_running; then
      echo "Clipboard Daemon is not running yet, starting it in the background."
      "${CLIPBOARD_DAEMON_BIN}" &>/dev/null &
    fi
    exit_remote_reverse_tunnel
    if ! is_reverse_tunnel_setup; then
      echo "Reverse tunnel is not setup yet, setting it up in the background."
      setup_reverse_tunnel
    fi
  fi
}

main
