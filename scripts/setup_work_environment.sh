#!/usr/bin/env bash

[[ -f "${HOME}/.bash_functions" ]] && source "${HOME}/.bash_functions"
[[ -f "${HOME}/.googlerc.d/.googlerc" ]] && source "${HOME}/.googlerc.d/.googlerc"

GCLOUD_FOLDERS=("notes" "gtd")
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
  # TODO: sshpass is doing something weird here where it doesn't allow gnubby
  # to work when it executes like this. Could it be because we're in a master session
  # already?
  sshpass -p "$(get_secret)" ssh -o LogLevel=QUIET -t "${USER}@${remote_host}" "$@"
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
      sshpass -p "$(get_secret)" sshfs -o reconnect "${USER}@${GCLOUD_HOST}:${remote_folder}" "${HOME}/${folder}"
      if [[ $? -ne 0 ]]; then
        echo "Unable to mount folder ${HOME}/${folder}. Unmounting and remounting once." >&2
        umount "${HOME}/${folder}"
        sshpass -p "$(get_secret)" sshfs -o reconnect "${USER}@${GCLOUD_HOST}:${remote_folder}" "${HOME}/${folder}"
      fi
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

function get_cloud_gcert_loas_hours_remaining {
  check_cloud_gcert_loas | grep -oP "[0-9]+(?=h\s[0-9]+m)"
}

function cloud_gcert {
  # gcert doesn't play well with sshpass for some reason.
  remote_ssh_cmd "${GCLOUD_HOST}" gcert
}

function retry_cmd {
  local cmd="$@"
  local retries=3
  local exit_status=1
  while [[ ${retries} -gt 0 ]] && [[ ${exit_status} -ne 0 ]]; do
    "${cmd[@]}"
    exit_status=$?
    retries=$((retries-1))
  done
  return ${exit_status}
}

function refresh_gcert {
  local environment="$1"
  local check_gcert=check_gcert_ssh
  local get_gcert_hours_remaining=get_gcert_ssh_hours_remaining
  local do_refresh_gcert=gcert
  local gcert_msg="gcert"
  if [[ "${environment}" == "cloud" ]]; then
    check_gcert=check_cloud_gcert_loas
    get_gcert_hours_remaining=get_cloud_gcert_loas_hours_remaining
    do_refresh_gcert=cloud_gcert
    gcert_msg="remote gcert"
  fi

  if ! "${check_gcert}" -quiet=true; then
    echo "Refreshing ${gcert_msg}, as it is invalid now."
    retry_cmd "${do_refresh_gcert}"
    return $?
  else
    local retries=3
    local remaining_hrs=
    while [[ ${retries} -gt 0 ]] && [[ -z "${remaining_hrs}" ]]; do
      remaining_hrs=$(${get_gcert_hours_remaining})
      retries=$((retries-1))
    done

    if [[ "${remaining_hrs}" -lt 8 ]]; then
      echo "Less than 8 hr remaining (${remaining_hrs} hr left) in ${gcert_msg}. Refreshing now."
      retry_cmd "${do_refresh_gcert}"
      return $?
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
  __debug "Setting up reverse tunnel."
  sshpass -p "$(get_secret)" ssh -f -N gcloud_tunnel
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
    # TODO: We need to conditionally do this. Only restart if the previous owner is not this workstation.
    echo "Closing the reverse tunnel port ${REVERSE_TUNNEL_PORT} directly on the cloud host ${GCLOUD_HOST}"
    cloud_ssh_cmd "/usr/local/google/home/${USER}/scripts/close_port.sh" "${REVERSE_TUNNEL_PORT}"
  fi

  if is_reverse_tunnel_setup; then
    echo "We were unable to close the pre-existing reverse tunnel to the cloud instance ${GCLOUD_HOST} at port ${REVERSE_TUNNEL_PORT}" >&2
    return 1
  fi

  return 0
}

function restart_reverse_tunnel {
  __debug "Restarting reverse tunnel"
  if exit_remote_reverse_tunnel; then
    setup_reverse_tunnel
    return 0
  fi

  return 1
}

function start_master_session {
  sshpass -p "$(get_secret)" ssh -N "${USER}@${GCLOUD_HOST}"
}

function kill_master_session {
  sshpass -p "$(get_secret)" ssh "${GCLOUD_HOST}" -O exit
}

function is_master_session_active {
  sshpass -p "$(get_secret)" ssh "${GCLOUD_HOST}" -O check
}

function restart_master_session {
  # Stop the master session if it already exists
  if is_master_session_active &>/dev/null; then
     echo "Stopping the previous master SSH session."
     kill_master_session
  fi
  # Initiate an SSH session in order to start the Master SSH Session
  # assumes there is a master session configuration.
  echo "Starting a new master SSH session."
  start_master_session
}


function list_commands {
  grep -E '^[[:space:]]*([[:alnum:]_]+[[:space:]]*\(\)|function[[:space:]]+[[:alnum:]_]+)' "${SCRIPT}" |\
    grep -Eo '[[:space:]][a-Z_]+' |\
    grep -Ev "main|parse_args|run|usage" |\
    sort
}

# TODO: If parameters are needed for the individual commands, then use the double dash to give the user the option of specifying the required parameters. i.e.:
# setup_work_environment.sh command_name -- param1 param2 param3
SCRIPT="$(basename $0)"
function usage() {
  cat <<EOF
${SCRIPT} [--help|-h] [--skip-master-session] [--skip-remote-folders] [--skip-clipboard-daemon] [--skip-cloud-gcert] <COMMANDS...>

--help: Show this message.
--skip-master-session: Skips any master session related setup/teardown.
--skip-remote-folders: Skips any remote folder mounting setup/teardown.
--skip-clipboard-daemon: Skips any setup/teardown of the clipboard daemon.
--skip-gloud-gcert: Skips calling gcert on the cloud machine.
COMMANDS: Space separated commands to execute, when this is set none of the other options are used.
Available commands:
$(list_commands)
EOF
}

DO_MASTER_SESSION=1
DO_MOUNT_REMOTE_FOLDERS=1
DO_CLIPBOARD_DAEMON=1
DO_CLOUD_GCERT=1
COMMANDS=()
function parse_args {
  while [[ $# -gt 0 ]]; do
    case "$1" in
      --help|-h)
        usage
        exit 0
        ;;
      --skip-master-session)
        DO_MASTER_SESSION=
        shift
        ;;
      --skip-remote-folders)
        DO_MOUNT_REMOTE_FOLDERS=
        shift
        ;;
      --skip-clipboard-daemon)
        DO_CLIPBOARD_DAEMON=
        shift
        ;;
      --skip-cloud-gcert)
        DO_CLOUD_GCERT=
        shift
        ;;
      *)
        local found=
        for cmd in $(list_commands); do
          if [[ "${cmd}" == "$1" ]]; then
            found=1
          fi
        done

        if [[ -z "${found}" ]]; then
          echo "ERROR: Unknown command or parameter: $1" >&2
          exit 1
        fi
        COMMANDS+=("$1")
        shift
        ;;
    esac
  done
}

function run {
  local _command="$1"
  if [[ "${_command}" ]]; then
    "${_command}"
    return $?
  fi

  if ! refresh_gcert; then
    echo "Failed to refresh gcert. Unable to continue." >&2
    exit 1
  fi

  if ! is_gcloud_host; then
    # NOTE: Always exit the clipboard daemon first, since it may kill any existing master session if the
    # previous clipboard tunnel owner is the same computer this script runs on.
    if [[ "${DO_CLIPBOARD_DAEMON}" ]]; then
      if ! exit_remote_reverse_tunnel; then
        echo "Cancelling creation of clipboard daemon & reverse tunnel." >&2
        DO_CLIPBOARD_DAEMON=
      fi
    fi

    if [[ "${DO_MASTER_SESSION}" ]]; then
      # We do not need to restart the master session by default because it will be restarted for us automatically.
      restart_master_session
    fi

    if [[ "${DO_CLOUD_GCERT}" ]]; then
      cloud_gcert
    fi

    if [[ "${DO_MOUNT_REMOTE_FOLDERS}" ]]; then
      mount_remote_gcloud_folders
    fi

    # NOTE: Always restart the clipboard daemon last, since it may kill the master session if the previous clipboard
    # tunnel owner is the same computer this script runs on.
    if [[ "${DO_CLIPBOARD_DAEMON}" ]]; then
      if ! is_clipboard_daemon_running; then
        echo "Clipboard Daemon is not running yet, starting it in the background."
        "${CLIPBOARD_DAEMON_BIN}" &>/dev/null &
      fi
      sleep "0.125s"
      if is_clipboard_daemon_running; then
        # NOTE: This seems to kill the SSH master session strangely.
        setup_reverse_tunnel
      else
        echo "We were unable to start the clipboard daemon, therefore we won't open the reverse tunnel for clipboard capturing." >&2
      fi
    fi
  fi
}

function main {
  parse_args "$@"
  for cmd in "${COMMANDS[@]}"; do
    run "${cmd}"
  done
}

if ! (return 0 2>/dev/null); then
  SCRIPT=$0
  main "$@"
fi
