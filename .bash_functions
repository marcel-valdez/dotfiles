#!/usr/bin/env bash

function __debug() {
  if [[ ! -z ${DEBUG} ]]; then
    echo "$@"
  fi
}

function now() {
  date "+%H:%M:%S"
}

function diff-lines() {
  local path=
  local line=
  while read; do
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

function git-diff-lines() {
  git diff "$@" | diff-lines
}

function g-diff-lines() {
  git-diff-lines "$@"
}

function git-branch-out() {
  new_branch_name=$1
  treeish=$2
  git checkout -b ${new_branch_name} ${treeish}
}

function g-branch-out() {
  git-branch-out "$@"
}

# end git functions

# start npm functions

function node-check-use() {
  node_version=$(node --version 2>/dev/null)
  if [ "${node_version}" != "v${NODE_VERSION}" ]; then
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
    which node > ~/.node_exec
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
  time=$(date +%H:%M)
  lemonbar-show --fg "#FFFF00" "(${time}) Running npm-run-all"
  run-step "npm install" "1/5"
  if [ ! ${__result} -eq 0 ]; then
    exit 1
  fi

  run-step "npm run test" "2/5"
  run-step "npm run test-tools" "3/5"
  run-step "npm run lint" "4/5"
  run-step "npm run validateLicenses" "5/5"
}

# end npm functions

mlocate-here() {
  directory=$2
  if [ -z "${directory}" ]; then
    directory="$(pwd)/"
  fi
  mlocate -b "$1" | grep --color=never "^${directory}"
}

run-step() {
  eval "$1"
  __result=$?
  time=$(date +%H:%M)
  if [ "${__result}" == "0" ]; then
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
  | head -$1
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

function tmux-goto-session-client() {
  g4d $(tmux-session-name)
}

function tmux-window-set-name() {
  if [[ "$2" == "-n" ]]; then
    local short_name="$1"
  else
    local short_name=$(echo "$1" | cut -c 1-30)
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
  connection_string=$(([ -n "${SSH_CLIENT}" ] && echo "${SSH_CLIENT}")|| echo "${SSH_CONNECTION}")
  echo "${connection_string}" | cut -d' ' -f1
}

function get-ssh-relay-hostname() {
  relay_ip=$(get-ssh-relay-ip)
  if [ -n "${relay_ip}" ]; then
    nslookup "${relay_ip}" | grep -o 'name =.*'  | cut -d'=' -f2 | sed 's/ //g' | sed 's/\.$//'
  fi
}

function __process-ps-lines() {
  lines="$(cat)"
  byte_limit="$(numfmt --to=none --from=si $1 2>/dev/null)"
  __debug "__process-ps-lines: byte_limit=${byte_limit}"
  readarray -t lines_array <<<"${lines}"
  for line in "${lines_array[@]}"
  do
    elements=(${line//:/ })
    kb_size="${elements[0]}"
    if [[ "${kb_size}" == "SIZE"  ]]; then
      echo "${line}"
    else
      byte_size=$(echo "${kb_size} * 1024" | bc)
      elements=("${elements[@]/$kb_size}")
      human_size="$(numfmt --to=si ${byte_size})"
      if [[ "${byte_size}" -gt "${byte_limit}"  ]] || [[ -z "${byte_limit}" ]]; then
        echo "${human_size} ${elements[@]}"
      fi
    fi
  done
}

function ps-show-memory-hogs() {
  procs=25
  command_format=comm
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
  __debug "procs=${procs}"
  __debug "limit=${limit}"

  ps -A -o size -o pid -o "${command_format}" --sort=-size \
    | __process-ps-lines "${limit}" | head "-${procs}"
}

function ps-nice() {
  # TODO: move to its own script file
  format="-o pid -o tty -o start -o args"
  sort="--sort=-comm"
  config="axw"

  # default configuration
  use_headers="false"
  full_command="false"

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

  ps ${format}\
    ${sort}\
    ${config}\
    "$@"
}

function emacs() {
  if [[ "${TERM}" =~ "eterm" ]]; then
    /usr/bin/emacsclient &>/tmp/emacs-${USER}-${RANDOM}.log
  else
    /usr/bin/emacs --no-window-system "$@" &>/tmp/emacs-${USER}-${RANDOM}.log
  fi
}

function google-emacs() {
  if [[ "${TERM}" =~ "eterm" ]]; then
    /usr/bin/emacsclient &> /tmp/google-emacs-${USER}-${RANDOM}.log
  else
    /usr/bin/google-emacs --no-window-system "$@" &> /tmp/google-emacs-${USER}-${RANDOM}.log
  fi
}

# force myself to use emacs, not nano
function nano() {
  [ "$1" == "-F" ] && shift
  emacs "$@"
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

    wait ${pid} &>/dev/null ||\
      tail --pid=${pid} -f &>/dev/null ||\
      echo "Failed to wait for process ${pid}" 1>&2
  done
}
