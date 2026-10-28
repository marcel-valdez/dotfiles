#!/usr/bin/env bash

if [[ -d "/opt/homebrew/bin" && ":${PATH}:" != *":/opt/homebrew/bin:"* ]]; then
  export PATH="/opt/homebrew/bin:/opt/homebrew/sbin:${PATH}"
fi
if (( BASH_VERSINFO[0] < 4 )) && [[ -x "/opt/homebrew/bin/bash" ]]; then
  exec /opt/homebrew/bin/bash "$0" "$@"
fi

[[ -f "${HOME}/lib/os_compat.sh" ]] && source "${HOME}/lib/os_compat.sh"
[[ -f "${HOME}/.bash_functions" ]] && source "${HOME}/.bash_functions"
[[ -f "${HOME}/.googlerc.d/.googlerc" ]] && source "${HOME}/.googlerc.d/.googlerc"
[[ -f "${HOME}/lib/log_lib.sh" ]] &&  source "${HOME}/lib/log_lib.sh"

GCLOUD_FOLDERS=("notes" "gtd" "tmp")

function get_local_mount_folder_name {
  case "$1" in
    tmp) echo "gcloud_tmp" ;;
    *)   echo "$1" ;;
  esac
}

# macOS (macFUSE 5, FSKit backend): about 30 s after mounting, the ROOT
# directory of an sshfs volume stops listing (ls shows nothing and new entries
# are not found), while every subdirectory keeps listing and updating normally
# (seen with macFUSE 5.4.0 + SSHFS 3.7.5 on macOS 26.7). So on macOS the remote
# home directory is mounted once on GCLOUD_HOME_MOUNT, and ~/notes, ~/gtd and
# ~/gcloud_tmp are symlinks into it (see mount_gcloud_home_and_link_folders).
GCLOUD_HOME_MOUNT="${HOME}/.gcloud_home"


GCLOUD_HOST="${USER}.c.googlers.com"
OFFICE_HOST="${USER}.mtv.corp.google.com"
LAPTOP_HOST="${USER}-glaptop"
CHROMEOS_HOST="${USER}28"
if declare -F os::is_macos >/dev/null 2>&1 && os::is_macos; then
  REVERSE_TUNNEL_PORT="${CLIPBOARD_PORT:-3335}"
else
  REVERSE_TUNNEL_PORT="${CLIPBOARD_PORT:-3333}"
fi
CLIPBOARD_DAEMON_BIN="${HOME}/bin/clipboard-daemon.sh"

function is_gcloud_host {
  [[ "${HOSTNAME}" == "${GCLOUD_HOST}" ]]
}

function is_office_host {
  [[ "${HOSTNAME}" == "${OFFICE_HOST}" ]]
}

function is_laptop_host {
  [[ "${HOSTNAME}" =~ ^${LAPTOP_HOST} ]] || \
    [[ "${HOSTNAME}" == "${CHROMEOS_HOST}" ]] || \
    [[ "${HOSTNAME}" =~ macbook|darwin ]] || \
    (declare -F os::is_macos >/dev/null 2>&1 && os::is_macos)
}

# override sshpass & get_secret for now as they are broken
function get_secret {
  echo "__BAD__"
}

function sshpass {
  shift  # -p
  shift  # secret
  "$@"
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
  if declare -F os::is_mounted >/dev/null 2>&1; then
    os::is_mounted "${HOME}/${_folder}"
  else
    mountpoint -q "${HOME}/${_folder}"
  fi
}

# Mounts one remote gcloud folder with sshfs. On macOS, macFUSE's FSKit backend
# (-o backend=fskit) cannot fork after mounting ("fuse: forking after mount is
# not supported"), so sshfs stays in the foreground for the life of the mount
# and would block the folder loop on its first mount. Start it in the
# background and wait for the mount point instead. Never SIGKILL it: a killed
# FSKit sshfs leaves a stale mount that hangs every access until force-unmounted.
function sshfs_mount_gcloud_folder {
  local remote_folder="$1"
  local mount_point="$2"
  local sshfs_opts=(-o compression=yes -o auto_cache -o reconnect)
  if ! os::is_macos; then
    sshpass -p "$(get_secret)" sshfs "${sshfs_opts[@]}" \
      "${USER}@${GCLOUD_HOST}:${remote_folder}" "${mount_point}"
    return $?
  fi

  local log_file
  log_file="${SSHFS_LOG_DIR:-/tmp}/sshfs-$(basename "${mount_point}").log"
  if ! command -v sshfs > /dev/null 2>&1; then
    log::warn "sshfs not found: install macFUSE and SSHFS from https://macfuse.github.io/"
    return 1
  fi
  # setsid: detach sshfs (and the ssh child it spawns for sftp) from the
  # caller's terminal session, so a SIGHUP from closing the terminal or tmux
  # pane cannot kill the ssh child and leave an empty, disconnected mount.
  ( sshpass -p "$(get_secret)" perl -MPOSIX -e 'POSIX::setsid(); exec @ARGV or die "exec: $!"' \
      sshfs -o backend=fskit "${sshfs_opts[@]}" \
      "${USER}@${GCLOUD_HOST}:${remote_folder}" "${mount_point}" ) \
    >> "${log_file}" 2>&1 < /dev/null &
  local sshfs_job=$!
  local _
  for _ in {1..60}; do
    if os::is_mounted "${mount_point}"; then
      return 0
    fi
    # sshfs exited without mounting (e.g. auth or remote path failure)
    kill -0 "${sshfs_job}" 2>/dev/null || break
    sleep 1
  done
  log::warn "sshfs did not mount ${mount_point} (see ${log_file})."
  # Stop a still-pending attempt gracefully (SIGTERM lets FUSE unmount cleanly).
  pkill -TERM -u "$(id -u)" -f "sshfs.*[[:space:]]${mount_point}\$" 2>/dev/null || true
  return 1
}

# Makes ~/<local folder name> a symlink to GCLOUD_HOME_MOUNT/<folder> (macOS).
# A per-folder sshfs mount left on it by older versions of this script is
# unmounted and its empty mount point directory replaced. Anything else (a
# non-empty directory, a file, or a symlink pointing elsewhere) is left alone.
function link_gcloud_folder {
  local folder="$1"
  local link
  link="${HOME}/$(get_local_mount_folder_name "${folder}")"
  local target="${GCLOUD_HOME_MOUNT}/${folder}"
  if [[ -L "${link}" ]]; then
    if [[ "$(readlink "${link}")" == "${target}" ]]; then
      return 0
    fi
    log::warn "${link} is a symlink to $(readlink "${link}"), not ${target}; leaving it alone."
    return 1
  fi
  if [[ -d "${link}" ]]; then
    if os::is_mounted "${link}"; then
      log::info "Unmounting the per-folder sshfs mount on ${link}"
      if ! umount "${link}"; then
        log::warn "Unable to unmount ${link}; not replacing it with a symlink to ${target}."
        return 1
      fi
    fi
    if ! rmdir "${link}" 2>/dev/null; then
      log::warn "${link} is a non-empty directory; not replacing it with a symlink to ${target}."
      return 1
    fi
  elif [[ -e "${link}" ]]; then
    log::warn "${link} exists and is not a directory; not replacing it with a symlink to ${target}."
    return 1
  fi
  log::info "Linking ${link} -> ${target}"
  ln -s "${target}" "${link}"
}

# macOS: mounts the remote home directory once on GCLOUD_HOME_MOUNT and links
# ~/notes, ~/gtd and ~/gcloud_tmp into it, because macFUSE FSKit volumes stop
# listing their root directory shortly after mounting (see GCLOUD_HOME_MOUNT).
function mount_gcloud_home_and_link_folders {
  local remote_home="/usr/local/google/home/${USER}"
  if ! os::is_mounted "${GCLOUD_HOME_MOUNT}"; then
    mkdir -p "${GCLOUD_HOME_MOUNT}"
    log::info "Mounting ${GCLOUD_HOST}:${remote_home} on ${GCLOUD_HOME_MOUNT}"
    if ! sshfs_mount_gcloud_folder "${remote_home}" "${GCLOUD_HOME_MOUNT}"; then
      log::warn "Unable to mount ${GCLOUD_HOME_MOUNT}. Unmounting and remounting once."
      if declare -F os::unmount_lazy >/dev/null 2>&1; then
        os::unmount_lazy "${GCLOUD_HOME_MOUNT}"
      else
        umount "${GCLOUD_HOME_MOUNT}"
      fi
      if ! sshfs_mount_gcloud_folder "${remote_home}" "${GCLOUD_HOME_MOUNT}"; then
        return 1
      fi
    fi
  fi
  local folder
  local rc=0
  for folder in "${GCLOUD_FOLDERS[@]}"; do
    link_gcloud_folder "${folder}" || rc=1
    # attempt to list directory contents in order to force reconnect of the SSHFS mount
    ls "${HOME}/$(get_local_mount_folder_name "${folder}")/" &>/dev/null
  done
  return "${rc}"
}

function mount_remote_gcloud_folders {
  log::info "Re-mounting (if necessary) folders from host ${USER}.c.googlers.com"
  if declare -F os::is_macos >/dev/null 2>&1 && os::is_macos; then
    mount_gcloud_home_and_link_folders
    return $?
  fi
  for folder in "${GCLOUD_FOLDERS[@]}"; do
    local local_folder_name
    local_folder_name="$(get_local_mount_folder_name "${folder}")"
    if ! is_folder_mounted "${local_folder_name}"; then
      if ! [[ -e "${HOME}/${local_folder_name}" ]]; then
        mkdir "${HOME}/${local_folder_name}"
      fi
      local remote_folder="/usr/local/google/home/${USER}/${folder}"
      log::info "Mounting ${GCLOUD_HOST}:${remote_folder} on ${HOME}/${local_folder_name}"
      if ! sshfs_mount_gcloud_folder "${remote_folder}" "${HOME}/${local_folder_name}"; then
        log::warn "Unable to mount folder ${HOME}/${folder}. Unmounting and remounting once."
        if declare -F os::unmount_lazy >/dev/null 2>&1; then
          os::unmount_lazy "${HOME}/${local_folder_name}"
        else
          umount "${HOME}/${local_folder_name}"
        fi
        sshfs_mount_gcloud_folder "${remote_folder}" "${HOME}/${local_folder_name}"
      fi
    fi
    # attempt to list directory contents in order to force reconnect of the SSHFS mount
    ls "${HOME}/${local_folder_name}" &>/dev/null
  done
}

function check_gcert_ssh {
  gcertstatus -check_ssh=true -check_loas2=false "$@"
}

function get_gcert_ssh_hours_remaining {
  check_gcert_ssh "$@" | sed -nE 's/.* ([0-9]+)h [0-9]+m.*/\1/p' | head -n 1
}

function check_cloud_gcert_loas {
  cloud_ssh_cmd /usr/bin/gcertstatus -check_ssh=false \
    -check_loas2=true "$@"
}

function get_cloud_gcert_loas_hours_remaining {
  check_cloud_gcert_loas "$@" | sed -nE 's/.* ([0-9]+)h [0-9]+m.*/\1/p' | head -n 1
}

function cloud_gcert {
  # gcert doesn't play well with sshpass for some reason.
  remote_ssh_cmd "${GCLOUD_HOST}" gcert "$@"
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
    log::info "Refreshing ${gcert_msg}, as it is invalid now."
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
      log::info "Less than 8 hr remaining (${remaining_hrs} hr left) in ${gcert_msg}. Refreshing now."
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

KILLED_MASTER_SESSION=
function exit_remote_reverse_tunnel {
  if is_laptop_host; then
    if office_ssh_cmd ssh gcloud -O check &>/dev/null; then
      log::info "Shutting down SSH master session for ${OFFICE_HOST}"
      office_ssh_cmd ssh gcloud -O exit
    fi
  else
    # We have no way of issuing a command on the laptop because we can't SSH
    # into it, therefore instead we tell the cloud instance to close the reverse
    # tunnel port. Do note that this may end up killing the SSH session that
    # opened the reverse tunnel.
    # If the gcloud process isn't running (meaning another process is connected).
    if ! ssh gcloud -O check &>/dev/null; then
      log::info "Closing the master session directly on the cloud host ${GCLOUD_HOST}"
      cloud_ssh_cmd "/usr/local/google/home/${USER}/scripts/close_port.sh" "${REVERSE_TUNNEL_PORT}"
    fi
  fi

  # Give the previous command 100 ms to run.
  sleep "0.1"
  if is_reverse_tunnel_setup; then
    log::error "We were unable to close the pre-existing reverse tunnel to the cloud instance ${GCLOUD_HOST} at port ${REVERSE_TUNNEL_PORT}"
    return 1
  fi

  return 0
}

function restart_reverse_tunnel {
  __debug "Restarting reverse tunnel"
  # NOTE: It is possible that issuing data on gcloud into the 3333 port
  # makes sure the port 3333 gets 'unstuck' when it refuses to die.
  # Essentially: `echo "data" >/dev/tcp/127.0.0.1/3333` would fail and
  # thereby release 3333 on gcloud.
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
  # but not if it was already killed in the script.
  if [[ -z "${KILLED_MASTER_SESSION}" ]] && is_master_session_active &>/dev/null; then
    log::info "Stopping the previous master SSH session."
    KILLED_MASTER_SESSION=1
    kill_master_session
  fi
  # Initiate an SSH session in order to start the Master SSH Session
  # assumes there is a master session configuration.
  if ! is_master_session_active &>/dev/null; then
    log::info "Starting a new master SSH session."
    start_master_session
  fi
}


function list_commands {
  grep -E '^[[:space:]]*([[:alnum:]_]+[[:space:]]*\(\)|function[[:space:]]+[[:alnum:]_]+)' "${SCRIPT}" |\
    grep -Eo '[[:space:]][[:alpha:]_][[:alnum:]_]*' |\
    grep -Ev "main|parse_args|run|usage" |\
    sort
}

SCRIPT="$(basename $0)"
function usage() {
  cat <<EOF
${SCRIPT} [--help|-h] [--skip-master-session] [--skip-remote-folders] [--skip-clipboard-daemon] [--skip-cloud-gcert] <COMMANDS...> -- <ARGS...>

--help: Show this message.
--skip-master-session: Skips any master session related setup/teardown.
--skip-remote-folders: Skips any remote folder mounting setup/teardown.
--skip-clipboard-daemon: Skips any setup/teardown of the clipboard daemon.
--skip-gloud-gcert: Skips calling gcert on the cloud machine.
COMMANDS: Space separated commands to execute, when this is set none of the other options are used.
ARGS: Sapce separated arguments to pass to all COMMANDS (normally better to specify one command).
Available commands:
$(list_commands)
EOF
}

DO_MASTER_SESSION=1
DO_MOUNT_REMOTE_FOLDERS=1
DO_CLIPBOARD_DAEMON=1
DO_CLOUD_GCERT=1
COMMANDS=()
ARGS=()
function parse_args {
  local process_args=
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
      --)
        process_args=1
        shift
        break
        ;;
      *)
        local found=
        for cmd in $(list_commands); do
          if [[ "${cmd}" == "$1" ]]; then
            found=1
          fi
        done

        if [[ -z "${found}" ]]; then
          log::error "ERROR: Unknown command or parameter: $1"
          return 1
        fi
        COMMANDS+=("$1")
        shift
        ;;
    esac
  done

  if [[ ${process_args} -gt 0 ]]; then
    while [[ $# -gt 0 ]]; do
      ARGS+=("$1")
      shift
    done
  fi
}

function run {
  local _command="$1"
  if [[ "${_command}" ]]; then
    if [[ "$2" == "--help" ]]; then
      type "$1" | tail +2
      return 127
    else
      "$@"
      return $?
    fi
  fi

  if ! is_gcloud_host; then
    # NOTE: Always exit the clipboard daemon first, since it may kill any existing master session if the
    # previous clipboard tunnel owner is the same computer this script runs on.
    if [[ "${DO_CLIPBOARD_DAEMON}" ]]; then
      if ! exit_remote_reverse_tunnel; then
        log::warn "Cancelling creation of clipboard daemon & reverse tunnel."
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
        log::info "Clipboard Daemon is not running yet, starting it in the background."
        ("${CLIPBOARD_DAEMON_BIN}" &>/dev/null) & disown
      fi
      sleep "0.125s"
      #if is_clipboard_daemon_running; then
      #  # NOTE: This seems to kill the SSH master session strangely.
      #  setup_reverse_tunnel
      #else
      #  echo "We were unable to start the clipboard daemon, therefore we won't open the reverse tunnel for clipboard capturing." >&2
      #fi
    fi
  fi
}

function check_gcert {
  if ! type gcertstatus &>/dev/null; then
    log::error "gcertstatus not available."
    return 1
  fi

  if ! gcertstatus &>/dev/null; then
    log::info "No gcert certificate available. Running gcert."

    if ! type gcert &>/dev/null; then
      log::error "gcert not available, is this a corp machine?"
      return 1
    fi

    gcert
  fi
}

function main {
  if ! parse_args "$@"; then
    log::error "Error while parsing args."
    return 1
  fi
  if ! check_gcert; then
    log::error "Unable to obtain a gcert certificate, cannot proceed."
    return 1
  fi

  if [[ ${#COMMANDS[@]} -gt 0 ]]; then
    for cmd in "${COMMANDS[@]}"; do
      if ! run "${cmd}" "${ARGS[@]}"; then
        return 1
      fi
    done
  else
    run
  fi
}

if ! (return 0 2>/dev/null); then
  SCRIPT=$0
  if ! main "$@"; then
    log::fatal "Encountered a fatal error."
  fi
fi
