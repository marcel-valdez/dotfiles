#!/usr/bin/env bash

function ::util::log_debug() {
  [ "${DEBUG_BASHRC}" != "" ] && echo "$(date +%H:%M:%S) $1"
}

# Gets a random decimal number made up of 0-N bytes specified by the user.
# So, for example, random 1 produces a decimal between 0-255
function random() {
  local num_bytes=$1
  [[ ${num_bytes} == "" ]] && local num_bytes=1
  od -A n -t d -N ${num_bytes} /dev/urandom | tr -d ' '
}

function now() {
  date "+%H:%M:%S"
}

function diff-lines() {
  local path=
  local line=
  while read -r; do
    esc=$'\033'
    if [[ "${REPLY}" =~ ---\ (a/)?.* ]]; then
      continue
    elif [[ "${REPLY}" =~ \+\+\+\ (b/)?([^[:blank:]$esc]+).* ]]; then
      path="${BASH_REMATCH[2]}"
    elif [[ "${REPLY}" =~ @@\ -[0-9]+(,[0-9]+)?\ \+([0-9]+)(,[0-9]+)?\ @@.* ]]; then
      line="${BASH_REMATCH[2]}"
    elif [[ "${REPLY}" =~ ^(${esc}\[[0-9;]+m)*([\ +-]) ]]; then
      echo "${path}:${line}:${REPLY}"
      if [[ "${BASH_REMATCH[2]}" != - ]]; then
        ((line++))
      fi
    fi
  done
}

# start git functions

function g() {
  git "$@"
}

function git-diff-lines() {
  git diff "$@" | diff-lines
}

function g-diff-lines() {
  git-diff-lines "$@"
}

function git-branch-out() {
  local new_branch_name=$1
  local treeish=$2
  git checkout -b "${new_branch_name}" "${treeish}"
}

function g-branch-out() {
  git-branch-out "$@"
}

# end git functions

# start npm functions

function node-check-use() {
  local is_silent="no"
  case "$1" in
    -s|--silent)
      is_silent="yes"
      ;;
    *)
      ;;
  esac

  if ! type nvm &>/dev/null; then
      local error_msg="node-check-use: nvm is not available." >&2
      ::util::log_debug "${error_msg}"
      if [[ "${is_silent}" == "no" ]]; then
          echo "${error_msg}" >&2
      fi
      return 1
  fi

  local node_version= && node_version=$(node --version 2>/dev/null)
  if [ "${node_version}" != "v${NODE_VERSION}" ]; then
    local node_version_installed= && \
      node_version_installed=$(nvm ls 2>/dev/null | grep "${NODE_VERSION}")
    if [ "${node_version_installed}" == "" ]; then
      echo "Node v${NODE_VERSION} is not installed, installing now."
      nvm install "v${NODE_VERSION}"
    fi

    case "$1" in
      -s|--silent)
        nvm use "${NODE_VERSION}" &>/dev/null
        ;;
      *)
        nvm use "${NODE_VERSION}"
        ;;
    esac
    # put the path to the node executable
    command -v node > ~/.node_exec
  fi
}

npm-tdd() {
  node-check-use
  npm run test:watch
}

npm-install() {
  node-check-use
  npm install
}

npm-test() {
  node-check-use
  npm run test
}

npm-start() {
  node-check-use
  npm start
}

npm-lint() {
  node-check-use
  npm run lint
}


npm-run-all() {
  local time= && time=$(date +%H:%M)
  lemonbar-show --fg "#FFFF00" "(${time}) Running npm-run-all"
  run-step "npm install" "1/5"
  if [ ! "${__step_result}" -eq 0 ]; then
    exit 1
  fi

  run-step "npm run test" "2/5"
  run-step "npm run test-tools" "3/5"
  run-step "npm run lint" "4/5"
  run-step "npm run validateLicenses" "5/5"
}

# end npm functions

mlocate-here() {
  local directory=$2
  if [ -z "${directory}" ]; then
    directory="$(pwd)/"
  fi
  mlocate -b "$1" | grep --color=never "^${directory}"
}

run-step() {
  eval "$1"
  __step_result=$?
  local time= && time=$(date +%H:%M)
  if [ "${__step_result}" == "0" ]; then
    lemonbar-show --fg "#99FF99" "(${time}) <$1> done ($2)"
  else
    lemonbar-show --fg "#C45454" "(${time}) <$1> failed ($2)"
  fi
}

function top-n() {
  history \
  | sed 's/^ \+//;s/  / /' \
  | cut -d']' -f2- \
  | awk '{ count[$0]++ } END { for (i in count) print count[i], i }' \
  | sort -rn \
  | head "-$1"
}

function top-ten() {
  top-n 10
}

function java-package-to-path() {
  get-arg-or-stdin "$@" | sed 's/\./\//g'
}

function copy-to-clip() {
  get-arg-or-stdin "$@" | perl -pe 'chomp if eof' | xclip -sel clip
}

function paste-clip() {
  xclip -o
}

function get-arg-or-stdin() {
  # if there are no arguments, then echo from stdin, otherwise echo arguments
  ([ $# -eq 0 ] && cat) || echo "$@"
}

function tmux-to-clip() {
  tmux show-buffer | copy-to-clip
}

function copy-to-tmux() {
  tmux set-buffer "$(get-arg-or-stdin $@)"
}

function history-cmd-only() {
  history | sed 's/^[^]]*\]//'
}

function cmd-exists() {
  ( type "$1" >/dev/null 2>&1 && echo "true" ) || echo "false"
}

function tmux-session-name() {
  tmux display-message -p '#S'
}

function tmux-window-set-name() {
  local short_name=
  if [[ "$2" == "-n" ]]; then
    short_name="$1"
  else
    short_name=$(echo "$1" | cut -c 1-30)
  fi
  tmux rename-window "${short_name}"
}

function tmux-window-name() {
  tmux display-message -p '#W'
}

function tmux-window-bell-on-activity() {
  local setting=$1
  [[ -z "${setting}" ]] && setting="on"
  tmux set-window-option monitor-activity "${setting}"
}

function is-ssh-session() {
  ([ -n "${SSH_CLIENT}" ] || [ -n "${SSH_TTY}" ] || [ -n "${SSH_CONNECTION}" ]) && echo "true"
}

function get-ssh-relay-ip() {
  local connection_string=
  connection_string=$( ([ -n "${SSH_CLIENT}" ] && echo "${SSH_CLIENT}") \
                         || echo "${SSH_CONNECTION}")
  echo "${connection_string}" | cut -d' ' -f1
}

function get-ssh-relay-hostname() {
  local relay_ip=$(get-ssh-relay-ip)
  if [ -n "${relay_ip}" ]; then
    nslookup "${relay_ip}" | grep -o 'name =.*'  | cut -d'=' -f2 | sed 's/ //g' | sed 's/\.$//'
  fi
}

function __process-ps-lines() {
  local lines=
  lines="$(cat)"
  local byte_limit=
  byte_limit="$(numfmt --to=none --from=si $1 2>/dev/null)"
  ::util::log_debug "__process-ps-lines: byte_limit=${byte_limit}"
  readarray -t lines_array <<<"${lines}"
  for line in "${lines_array[@]}"
  do
    local elements=(${line//:/ })
    local kb_size="${elements[0]}"
    if [[ "${kb_size}" == "SIZE"  ]]; then
      echo "${line}"
    else
      local byte_size= && byte_size=$(echo "${kb_size} * 1024" | bc)
      elements=("${elements[@]/$kb_size}")
      local human_size= && human_size="$(numfmt --to=si ${byte_size})"
      if [[ "${byte_size}" -gt "${byte_limit}"  ]] || [[ -z "${byte_limit}" ]]; then
        echo "${human_size} ${elements[@]}"
      fi
    fi
  done
}

function ps-show-memory-hogs() {
  local procs=25
  local command_format=comm
  while [[ $# -gt 0 ]]; do
    case $1 in
      -n|--number)
        procs=$2
        shift
        ;;
      -f|--full-command)
        command_format="args"
        shift
        ;;
      -s|--short-command)
        # this is the default
        ;;
      -l|--limit-size)
        limit=$2
        shift
        ;;
      *)
        echo "Ignoring unkown parameter: $1" >&2
        ;;
    esac
    shift
  done
  ::util::log_debug "procs=${procs}"
  ::util::log_debug "limit=${limit}"

  ps -A -o size -o pid -o "${command_format}" --sort=-size \
    | __process-ps-lines "${limit}" | head "-${procs}"
}

function ps-nice() {
  # TODO: move to its own script file
  local format="-o pid -o tty -o start -o args"
  local sort="--sort=-comm"
  local config="axw"

  # default configuration
  local use_headers="false"
  local full_command="false"
  local size=

  while [[ $# -gt 0 ]]; do
    case $1 in
      --use-headers)
        use_headers="true"
        ;;

      --full-command)
        full_command="true"
        ;;

      --show-size)
        size="true"
        ;;

      *)
        break
        ;;

    esac
    shift
  done

  if [ "${use_headers}" == "false" ]; then
    config="${config}h"
  fi

  if [ "${full_command}" == "true" ]; then
    config="${config}w"
  fi

  if [ ! -z "${size}" ]; then
    format="-o pid -o size -o tty -o start -o args"
  fi

  ps "${format}"\
    "${sort}"\
    "${config}"\
    "$@"
}

function emacs() {
  if [[ "${TERM}" =~ "eterm" ]]; then
    emacs-client "$@"
  else
    /usr/bin/emacs --no-window-system "$@" &>"/tmp/emacs-${USER}-${RANDOM}.log"
  fi
}

# force myself to use emacs, not nano
function nano() {
  [ "$1" == "-F" ] && shift
  emacs-client "$@"
}

# shows the shell's keyboard shortcuts
function bind-show-shortcuts() {
  bind -p | grep -v "not bound" | grep -v "self-insert"
}

# shows the shell's settings
function bind-show-settings() {
  bind -v
}

# show the shell's interactive capabilities
function bind-show-capabilities() {
  bind -l
}

function show-shell-shortcuts() {
  bind-show-shortcuts
}

function show-shell-settings() {
  bind-show-settings
}

function show-shell-capabilities() {
  bind-show-capabilities
}

function wait-for-process() {
  for pid in "$@"
  do
    # skip pid if process does not exist
    [ -z "$(pgrep "${pid}")"  ] && continue

    wait "${pid}" &>/dev/null ||\
      tail "--pid=${pid}" -f &>/dev/null ||\
      echo "Failed to wait for process ${pid}" 1>&2
  done
}

function diff-color {
  diff -burN --color=always $@
}

function less-color {
  less -r $@
}

function ls-sort-date {
  ls -lt "$@"
}

function start-notification-daemon {
  DISPLAY=:0 /usr/lib/notification-daemon/notification-daemon
}
