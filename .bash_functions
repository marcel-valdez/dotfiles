#!/usr/bin/env bash

function __debug() {
  if [[ ! -z ${DEBUG} ]]; then
    echo "$@"
  fi
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
function node-lts-version() {
  local node_version_file="/tmp/${USER}_NODE_VERSION"
  local lts_node_version=
  if [[ -e "${node_version_file}" ]]; then
    lts_node_version=$(cat "${node_version_file}")
  fi

  if [[ -z "${lts_node_version}" ]]; then
    lts_node_version=$(nvm ls-remote --lts | tail -1 | grep -oP "(?<=v)[0-9a-b\.]+")
    echo "${lts_node_version}" > "${node_version_file}"
  fi

  echo "${lts_node_version}"
}


function node-check-use() {
  if ! type nvm &>/dev/null; then
    # Do not proceed if NVM is not installed
    echo "WARNING: NVM is not installed on this machine."
    return 1
  fi

  if [[ "${NODE_VERSION}" == "lts" ]] || [[ "${NODE_VERSION}" == "LTS" ]]; then
    NODE_VERSION="$(node-lts-version)"
    export NODE_VERSION
  fi

  node_version=$(node --version 2>/dev/null)
  if [[ "${node_version}" != "v${NODE_VERSION}" ]]; then
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
    emacs-client "$@"
  else
    /usr/bin/emacs --no-window-system "$@" &>/tmp/emacs-${USER}-${RANDOM}.log
  fi
}

function emacs-client() {
  local editor_cmd=(/usr/bin/emacsclient --create-frame --tty --socket-name=tty-server)
  if ! EDITOR="'""${editor_cmd[@]}""'" "${editor_cmd[@]}" "$@"; then
    if type at &>/dev/null; then
      echo "emacs --daemon=tty-server" | at NOW
    else
      (emacs --bg-daemon=tty-server &)
    fi
    # Give the server 125ms to start listening for connections.
    sleep 0.125
    EDITOR="'""${editor_cmd[@]}""'" "${editor_cmd[@]}" "$@"
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

    wait ${pid} &>/dev/null ||\
      tail --pid=${pid} -f &>/dev/null ||\
      echo "Failed to wait for process ${pid}" 1>&2
  done
}

function fixup_ssh_auth_sock()  {
  if [[ -n ${SSH_AUTH_SOCK} && ! -e ${SSH_AUTH_SOCK} ]]; then
    local new_sock=
    new_sock=$(echo /tmp/ssh-*/agent*)
    if [[ -n ${new_sock} ]]; then
      export SSH_AUTH_SOCK=${new_sock}
    fi
  fi
}

function less-color() {
  less -R "$@"
}

function imgcat-url {
  local url="$1"
  if [[ -z "${url}" ]]; then
    url=$(cat)
  fi

  if [[ -z "${url}" ]]; then
    echo "No URL provided, stopping" >&2
    return 1
  fi

  local clean_url=$(echo "${url}" | sed 's|[\r\n\t]||g')
  if [[ "${TMUX}" ]] || ! type wezterm &>/dev/null; then
    if type imgcat &>/dev/null; then
      echo "We're within a TMUX session, using normal imgcat" >&2
      wget -O /dev/stdout "${clean_url}" | imgcat
    else
      echo "You need to install imgcat" >&2
      return 1
    fi
  elif type wezterm &>/dev/null; then
    wget -O /dev/stdout "${clean_url}" | wezterm imgcat
  fi
}


function gnome-terminal-dump-conf {
  dconf dump /org/gnome/terminal/
}

function gnome-terminal-load-conf {
  local conf_file="$1"
  if [[ -z "${conf_file}" ]]; then
    conf_file="${HOME}/.gterminal.conf"
  fi

  cat "${conf_file}" | dconf load /org/gnome/terminal/legacy/profiles:/
}

function timestamp {
  date "+%s"
}

function chrono-start {
  __CHRONO_START=$(timestamp)
  echo "${__CHRONO_START}"
}

function chrono-end {
  local chrono_end=
  chrono_end=$(timestamp)
  local chrono_duration_sec=$((chrono_end-__CHRONO_START))
  echo "${chrono_duration_sec}"
}

function fzf-cmd {
  if ! type -p fzf &>/dev/null; then
    echo "fzf-find: fzf not available, can't proceed." >&2
    echo 'sudo apt install fzf' >&2
  fi

  local fzf_cmd=('fzf')
  if [[ "${TMUX}" ]]; then
    fzf_cmd=('fzf-tmux' '-p' '--height' '80%')
  fi

  "${fzf_cmd[@]}" "$@"
}

function fzf-find {
  if ! type -p rg &>/dev/null; then
    echo "fzf-find: rg not available, can't proceed." >&2
    echo 'sudo apt install ripgrep' >&2
  fi

  # 1. Search for text in files using Ripgrep
  # 2. Interactively restart Ripgrep with reload action

  local options=("$@")
  if [[ "$#" -eq 0 ]]; then
    options=('.*')
  fi
  rg --color=always --line-number --no-heading --smart-case "${options[@]}" |
  fzf-cmd --ansi \
      --color "hl:-1:underline,hl+:-1:underline:reverse" \
      --delimiter : \
      --preview 'batcat --theme=gruvbox-dark --color=always {1} --highlight-line {2}' \
      --preview-window 'up,60%,border-bottom,+{2}+3/3,~3'
}

function fzf-edit {
    rg --color=always --line-number --no-heading --smart-case "${*:-}" |
        fzf --ansi \
            --color "hl:-1:underline,hl+:-1:underline:reverse" \
            --delimiter : \
            --preview 'batcat --color=always {1} --highlight-line {2}' \
            --preview-window 'up,60%,border-bottom,+{2}+3/3,~3' \
            --bind 'enter:become(emacsclient --socket-name=tty-server --tty --create-frame {1} +{2})'
}

function fzf-edit-deep {
  if ! type -p rg &>/dev/null; then
    echo "fzf-find: rg not available, can't proceed." >&2
    echo 'sudo apt install ripgrep' >&2
  fi

  if ! type -p batcat &>/dev/null; then
    echo "fzf-find: batcat not available, can't proceed." >&2
    echo 'sudo apt install bat' >&2
  fi

  RG_PREFIX="rg --column --line-number --no-heading --color=always --smart-case "
  FILESET=("$@")
  if [[ "$#" -eq 0 ]]; then
    FILESET=(".")
  fi
  : | fzf-cmd --ansi --disabled --query "" \
    --bind "start:reload:$RG_PREFIX {q} ${FILESET[*]}" \
    --bind "change:reload:sleep 0.1; $RG_PREFIX {q} ${FILESET[*]} || true" \
    --bind "alt-enter:unbind(change,alt-enter)+change-prompt(2. fzf> )+enable-search+clear-query" \
    --color "hl:-1:underline,hl+:-1:underline:reverse" \
    --prompt '1. ripgrep> ' \
    --delimiter : \
    --preview 'batcat --color=always {1} --highlight-line {2}' \
    --preview-window 'up,60%,border-bottom,+{2}+3/3,~3' \
    --bind 'enter:become(/usr/bin/emacsclient --create-frame --tty {1} +{2})'
}

function fzf-preview {
    fzf-cmd --ansi \
            --color "hl:-1:underline,hl+:-1:underline:reverse" \
            --delimiter : \
            --preview 'batcat --color=always {1} --highlight-line {2}' \
            --preview-window 'up,60%,border-bottom,+{2}+3/3,~3'
}

function fzf-kill {
  # pipes ps -ef output to fzf and kills the process with with signal -9
  local kill_args=("$@")
  if [[ $# -eq 0 ]]; then
    kill_args=('-9')
  fi
  (date; ps -ef) |
  fzf-cmd --bind='ctrl-r:reload(date; ps -ef)' \
      --header=$'Press CTRL-R to reload\n\n' --header-lines=2 \
      --preview='echo {}' --preview-window=down,3,wrap \
      --height=80% | awk '{print $2}' | xargs kill "${kill_args[@]}"
}

function f {
    # if no arguments passed, just lauch fzf
    if [ $# -eq 0 ]
    then
        fzf-cmd | sort
        return 0
    fi

    # Store the program
    program="$1"

    # Remove first argument off the list
    shift

    # Store any option flags
    options=("$@")

    # Store the arguments from fzf
    arguments=($(IFS='
' fzf-cmd --multi))

    # If no arguments passed (e.g. if Esc pressed), return to terminal
    if [ "${#arguments[@]}" -eq 0 ]; then
        return 1
    fi

    # Sanitise the command by putting single quotes around each argument, also
    # first put an extra single quote next to any pre-existing single quotes in
    # the raw argument. Put them all on one line.
    clean_arguments=()
    for arg in "${arguments[@]}"; do
      clean_arguments+=($(echo "$arg" | IFS='' sed "s/'/''/g; s/.*/'&'/g; s/\n//g"))
    done
    # space is the default
    IFS=" "

    # If the program is on the GUI list, add a '&'
    if [[ "${program}" =~ ^(nautilus|zathura|evince|vlc|eog|kolourpaint)$ ]]; then
      clean_arguments+=("&")
    fi

    # Write the shell's active history to ~/.bash_history.
    history -w

    # Add the command with the sanitised arguments to .bash_history
    echo "${program}" "${options[@]}" "${clean_arguments[@]}" >> ~/.bash_history

    # Reload the ~/.bash_history into the shell's active history
    history -r

    # execute the last command in history
    "${program}" "${options[@]}" "${clean_arguments[@]}"
}

function fzf-cs-cd {
  local cd_file=
  cd_file="$(fzf-cs | cut -d':' -f1)"
  local directory=
  directory="$(dirname "${cd_file}")"
  cd "${directory}" || return 1
}
