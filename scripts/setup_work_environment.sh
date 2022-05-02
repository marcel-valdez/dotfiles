#!/usr/bin/env bash

[[ -f "${HOME}/.bash_functions" ]] && source "${HOME}/.bash_functions"
[[ -f "${HOME}/.googlerc.d/.googlerc" ]] && source "${HOME}/.googlerc.d/.googlerc"

GCLOUD_FOLDERS=("notes")
GCLOUD_HOST="${USER}.c.googlers.com"
OFFICE_HOST="${USER}.mtv.corp.google.com"
LAPTOP_HOST="${USER}-glaptop"
REVERSE_TUNNEL_PORT=3333
CLIPBOARD_DAEMON_BIN="${HOME}/bin/clipboard-daemon.sh"


function is_gcloud_host {
  [[ "${HOSTNAME}" == "${GCLOUD_HOST}" ]]
}

function is_office_host {
  [[ "${HOSTNAME}" == "${OFFICE_HOST}" ]]
}

function is_laptop_host {
  [[ "${HOSTNAME}" == "${LAPTOP_HOST}" ]]
}

function remote_ssh_cmd() {
  local remote_host="$1"
  shift
  ssh -o LogLevel=QUIET -t "${USER}@${remote_host}" "$@"
}

function office_ssh_cmd() {
  remote_ssh_cmd "${OFFICE_HOST}" "$@"
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

function check_cloud_gcert_loas {
  cloud_ssh_cmd /usr/bin/gcertstatus -check_ssh=false \
    -check_loas2=true "$@"
}

function get_remote_gcert_loas_hours_remaining {
  check_remote_gcert_loas | grep -oP "[0-9]+(?=h\s[0-9]+m)"
}

function remote_gcert {
  remote_cmd "${GCLOUD_HOST}" gcert
}

function refresh_gcert {
  local environment="$1"
  local check_gcert=check_gcert_ssh
  local get_gcert_hours_remaining=get_gcert_ssh_hours_remaining
  local _refresh_gcert=gcert
  local gcert_msg="gcert"
  if [[ "${environment}" == "remote" ]]; then
    check_gcert=check_remote_gcert_loas
    get_gcert_hours_remaining=get_remote_gcert_loas_hours_remaining
    _refresh_gcert=remote_gcert
    gcert_msg="remote gcert"
  fi

  if ! "${check_gcert}" -quiet=true; then
    echo "Refreshing ${gcert_msg}, as it is invalid now."
    "${_refresh_gcert}"
  else
    local retries=3
    local remaining_hrs=
    while [[ ${retries} -gt 0 ]] && [[ -z "${remaining_hrs}" ]]; do
      remaining_hrs=$(${get_gcert_hours_remaining})
      retries=$((retries-1))
    done

    if [[ "${remaining_hrs}" -lt 8 ]]; then
      echo "Less than 8 hr remaining (${remaining_hrs} hr left) in ${gcert_msg}. Refreshing now."
      "${_refresh_gcert}"
    fi
  fi
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
    if office_ssh_cmd ssh gcloud_tunnel -O check &>/dev/null; then
      echo "Shutting down reverse tunnel for ${OFFICE_HOST}"
      office_ssh_cmd ssh gcloud_tunnel -O exit
    fi
  else
    # We have no way of issuing a command on the laptop because we can't SSH
    # into it, therefore instead we tell the cloud instance to close the reverse
    # tunnel port. Do note that this may end up killing the SSH session that
    # opened the reverse tunnel.
    echo "Closing the reverse tunnel port ${REVERSE_TUNNEL_PORT} directly on the cloud host ${GCLOUD_HOST}"
    cloud_ssh_cmd "/usr/local/google/home/${USER}/scripts/close_port.sh" "${REVERSE_TUNNEL_PORT}"
  fi
}

function restart_reverse_tunnel {
  exit_remote_reverse_tunnel
  if ! is_reverse_tunnel_setup; then
    echo "Setting up reverse tunnel to host ${GCLOUD_HOST} at port ${REVERSE_TUNNEL_PORT}"
    setup_reverse_tunnel
  else
    echo "We were unable to close the pre-existing reverse tunnel to the cloud instance ${GCLOUD_HOST} at port ${REVERSE_TUNNEL_PORT}" >&2
    echo "Therefore we can't re-create a new one for this host." >&2
  fi
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
  ssh -N "${USER}@${GCLOUD_HOST}"
}

function main {
  if [[ "$#" -gt 0 ]]; then
    "$@"
  else
    refresh_gcert
    if ! is_gcloud_host; then
      restart_master_session
      mount_remote_gcloud_folders
      if ! is_clipboard_daemon_running; then
        echo "Clipboard Daemon is not running yet, starting it in the background."
        "${CLIPBOARD_DAEMON_BIN}" &>/dev/null &
      fi
      sleep "0.125s"
      if is_clipboard_daemon_running; then
        restart_reverse_tunnel
      else
        echo "We were unable to start the clipboard daemon, therefore we won't open the reverse tunnel for clipboard capturing." >&2
      fi
      # refresh gcert on the cloud host
      cloud_gcert
    fi
  fi
}

if ! (return 0 2>/dev/null); then
  main "$@"
fi
