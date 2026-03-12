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
    echo "WARNING: NVM is not installed on this machine." >&2
    return 1
  fi

  if [[ "${NODE_VERSION}" == "lts" ]] || [[ "${NODE_VERSION}" == "LTS" ]]; then
    NODE_VERSION="$(node-lts-version)"
    export NODE_VERSION
  fi

  node_version=$(node --version 2>/dev/null)
  __debug "Node is version: ${node_version} and env variable NODE_VERSION is ${NODE_VERSION}"
  if [[ "${node_version}" != "v${NODE_VERSION}" ]]; then
    node_version_installed=$(nvm ls 2>/dev/null | grep "${NODE_VERSION}")
    if [[ "${node_version_installed}" =~ .*N/A.* ]]; then
      echo "Node v${NODE_VERSION} is not installed, installing now." >&2
      nvm install "v${NODE_VERSION}"
    fi

    case "$1" in
      -s|--silent)
        __debug "nvm use '${NODE_VERSION}' &>/dev/null"
        nvm use "v${NODE_VERSION}" &>/dev/null
        ;;
      *)
        __debug "nvm use '${NODE_VERSION}'"
        nvm use "v${NODE_VERSION}"
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
  tmux set-buffer -w "$(get-arg-or-stdin "$@")"
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
  local editor_cmd=(/usr/bin/emacsclient --create-frame --tty --socket-name=${EMACS_TTY_SERVER})
  if ! EDITOR="'""${editor_cmd[@]}""'" "${editor_cmd[@]}" "$@"; then
    if type at &>/dev/null; then
      echo "emacs --daemon=${EMACS_TTY_SERVER}" | at NOW
    else
      (emacs --bg-daemon=${EMACS_TTY_SERVER} &)
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

function fzf-context {
  local file="$1"
  local context_lines="$2"
  if [[ -z "${context_lines}" ]]; then
    context_lines=10
  fi
  local tmpfile=
  tmpfile="$(mktemp).sh"

  local terminal_width=
  terminal_width=$(stty -a | grep -Po '(?<=columns )\d+')
  local preview_width=
  preview_width=$((terminal_width / 2))

  (cat<<EOF
hilite=\$1;
context=\$2;
start=\$((hilite - context));
if [[ \$start -lt 0 ]]; then start=0; fi;
end=\$((hilite + context));
batcat ${file} --highlight-line=\${hilite} --line-range=\${start}:\${end} --color=always --style=numbers --wrap=character --terminal-width=${preview_width} --paging=always
EOF
)>"${tmpfile}"
  cat -n "${file}" | fzf --preview "bash ${tmpfile} {1} ${context_lines}" --bind 'ctrl-/:toggle-preview'
}

function fzf-cmd {
  if ! type -p fzf &>/dev/null; then
    echo "fzf-find: fzf not available, can't proceed." >&2
    echo 'sudo apt install fzf' >&2
  fi

  local fzf_cmd=('fzf')
  if [[ "${TMUX}" ]]; then
    fzf_cmd=('fzf-tmux' '-p' '-h' '90%' '-w' '90%')
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
    --preview 'batcat --color=always {1} --highlight-line {2}' \
    --preview-window 'up,60%,border-bottom,+{2}+3/3,~3'
}

function fzf-edit {
    rg --color=always --line-number --no-heading --smart-case "${*:-}" |
      fzf --ansi \
        --color "hl:-1:underline,hl+:-1:underline:reverse" \
        --delimiter : \
        --preview 'batcat --color=always {1} --highlight-line {2} --line-range {2}:-5 --line-range {2}:+5' \
        --preview-window 'up,60%,border-bottom,+{2}+3/3,~3' \
        --bind 'enter:become(emacsclient --socket-name='"${EMACS_TTY_SERVER}"' --tty --create-frame {1} +{2})'
}

function fzf-edit-deep {
  if ! type -p rg &>/dev/null; then
    echo "fzf-edit-deep: rg not available, can't proceed." >&2
    echo 'sudo apt install ripgrep' >&2
  fi

  if ! type -p batcat &>/dev/null; then
    echo "fzf-edit-deep: batcat not available, can't proceed." >&2
    echo 'sudo apt install bat' >&2
  fi

  RG_PREFIX="rg --column --line-number --no-heading --color=always --smart-case "
  FILESET=("$@")
  if [[ "$#" -eq 0 ]]; then
    FILESET=(".")
  fi
  : | fzf --ansi --disabled --query "" \
    --bind "start:reload:$RG_PREFIX {q} ${FILESET[*]}" \
    --bind "change:reload:sleep 0.1; $RG_PREFIX {q} ${FILESET[*]} || true" \
    --header=$'ctrl-f:fzf-mode, ctrl-space:select-all'\
    --bind "ctrl-f:rebind(ctrl-r)+unbind(change,ctrl-f)+change-prompt(2. fzf> )+change-header(ctrl-r:ripgrep-mode, ctrl-space:select-all)+enable-search+clear-query" \
    --bind "ctrl-r:rebind(change,ctrl-f)+unbind(ctrl-r)+change-prompt(1. ripgrep> )+change-header(ctrl-f:fzf-mode, ctrl-space:select-all)+disable-search+clear-query" \
    --bind  "ctrl-space:select-all" \
    --color "hl:-1:underline,hl+:-1:underline:reverse" \
    --prompt '1. ripgrep> ' \
    --delimiter : \
    --preview 'batcat --color=always {1} --highlight-line {2}' \
    --preview-window 'up,60%,border-bottom,+{2}+3/3,~3' \
    --bind 'enter:become(/usr/bin/emacsclient --socket-name='"${EMACS_TTY_SERVER}"' --create-frame --tty {1} +{2})'
}

function fzf-preview {
  fzf-cmd --ansi --query '' \
    --color "hl:-1:underline,hl+:-1:underline:reverse" \
    --delimiter : \
    --preview 'batcat --color=always {1} --highlight-line {2}' \
    --preview-window 'up,66%,border-bottom,+{2}+3/3,~3'
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

function fif {
  __debug "fif: Checks."
  if ! type rg &>/dev/null; then
    echo "Ripgrep required. sudo apt install ripgrep" >&2
    return 1
  fi

  if ! type fzf &>/dev/null; then
    echo "Fzf required. sudo apt install fzf" >&2
    return 1
  fi

  if ! type sponge &>/dev/null; then
    echo "Sponge required. sudo apt install moreutils" >&2
    return 1
  fi

  __debug "fif: Start."
  local results_file=
  local results_file_history=
  local prompt_file=
  local prompt_file_history=
  local initial_query=()
  local _pwd=
  local prompt_ln="/tmp/fif-prompt.${RANDOM}"
  local results_ln="/tmp/fif-files.${RANDOM}"
  local rg_query_file="/tmp/rg-fzf-r.${RANDOM}"
  local fzf_query_file="/tmp/rg-fzf-f.${RANDOM}"
  local ctrl_space_execute=
  local ctrl_delete_execute=

  local term_width=$(tput cols)
  local half_term_width=$((term_width / 2))
  local preview_title_width=$((half_term_width - 10))
  prompt_file=$(mktemp)
  prompt_file_history=$(mktemp)
  results_file=$(mktemp)
  results_file_history=$(mktemp)

  echo "${prompt_file}" > "${prompt_file_history}"
  ln -sf "${prompt_file}" "${prompt_ln}"
  echo "${results_file}" > "${results_file_history}"
  ln -sf "${results_file}" "${results_ln}"

  while read -r filename; do
    if [[ -d "${filename}" ]]; then
      find "${filename}" -type f >> "${results_ln}"
    else
      echo "${filename}" >> "${results_ln}"
    fi
  done

  __debug "fif: Step 1."
  sort < "${results_ln}" | uniq | sponge "${results_ln}"
  if [[ "$1" ]]; then
    initial_query+=("-e" "$1")
    echo -n "$1> " > "${prompt_ln}"
  else
    initial_query+=("-e" '')
    echo -n '> ' > "${prompt_ln}"
  fi

  local rg_prefix="rg-file --files-with-matches --line-buffered --file-list-path ${results_ln}"
  local rg_header="[RG MODE] ctrl-space:filter / ctrl-delete:undo-filter
ctrl+o:open / ctrl+/: popup batcat / ctrl-f:fzf mode"
  local fzf_header="[FZF MODE] ctrl-space:filter / ctrl-delete:undo-filter
ctrl+o:open / ctrl+/: popup batcat / ctrl-r:rg mode"
  _pwd="$(pwd)"

  ctrl_space_execute=$(cat<<EOF
mktemp >> ${results_file_history};
mktemp >> ${prompt_file_history};
bash -c 'cp ${prompt_ln} \$(tail -1 ${prompt_file_history})';
bash -c 'ln -sf \$(tail -1 ${results_file_history}) ${results_ln}';
bash -c 'ln -sf \$(tail -1 ${prompt_file_history}) ${prompt_ln}';
echo {+} | sed -E 's|[ ]+|\n|g' > ${results_ln};
echo -n "{q} > " >> ${prompt_ln}
EOF
                    )
  ctrl_delete_execute=$(cat<<EOF
head -n -1 ${results_file_history} | sponge ${results_file_history};
head -n -1 ${prompt_file_history} | sponge ${prompt_file_history};
bash -c 'ln -sf \$(tail -1 ${results_file_history}) ${results_ln}';
bash -c 'ln -sf \$(tail -1 ${prompt_file_history}) ${prompt_ln}';
EOF
                     )
  __debug "fif: Step 2."
  FZF_DEFAULT_COMMAND="${rg_prefix} ${initial_query[@]}" \
    fzf \
    --sort \
    --multi \
    --preview '[[ ! -z {} ]] && rg --pretty --context 5 {q} {}' \
    --ansi --phony --query "${initial_query[1]}" \
    --bind "start:reload(cat ${results_ln})" \
    --bind "focus:transform-preview-label(echo {} | tail -c ${preview_title_width})" \
    --bind "result:transform-preview-label(echo {} | tail -c ${preview_title_width})" \
    --bind "change:reload(sleep 0.25 && ${rg_prefix} -e {q} | sort || true)" \
    --bind "ctrl-space:select-all+execute(${ctrl_space_execute})+transform-prompt(cat ${prompt_ln})+clear-query+reload(cat ${results_ln})" \
    --bind "ctrl-delete:deselect-all+execute(${ctrl_delete_execute})+transform-prompt(cat ${prompt_ln})+clear-query+reload(cat ${results_ln})" \
    --bind "ctrl-f:unbind(change,ctrl-f)+change-header(${fzf_header})+enable-search+rebind(ctrl-r)+transform-query(echo {q} > ${rg_query_file}; cat ${fzf_query_file})+change-preview(batcat --paging=never --style='numbers,changes' --color=always {})" \
    --bind "ctrl-r:unbind(ctrl-r)+change-header(${rg_header})+disable-search+reload(${rg_prefix} -e {q} || true)+rebind(change,ctrl-f)+transform-query(echo {q} > ${fzf_query_file}; cat ${rg_query_file})+change-preview([[ ! -z {} ]] && rg --pretty --context 5 {q} {})" \
    --bind 'ctrl-o:become(bash -i -c "emacs-client {+}")' \
    --prompt "$(cat "${prompt_ln}")" \
    --bind "ctrl-/:execute:tmux display-popup -w '80%' -h '80%' -d '${_pwd}' -T '{}' -E batcat --paging=always --style='numbers,changes' --color=always {}" \
    --header "${rg_header}"
}

function fzf-cs-cd {
  local cd_file=
  cd_file="$(fzf-cs | cut -d':' -f1)"
  local directory=
  directory="$(dirname "${cd_file}")"
  cd "${directory}" || return 1
}

function hg_lines_changed {
  local diffstats_local=
  local diffstats_commit=
  local added_local=
  local added_commit=0
  local deleted_local=
  local deleted_commit=0
  # The head commit is authored by this user
  if [[ "${USER}@google.com" == "$(hg log -r . -T '{user}')" ]]; then
    # The head commit is not submitted
    if [[ -z "$(hg log -r . -T '{submittedcls}')" ]]; then
      diffstats_commit="$(hg log -r . -T '{diffstat}')"
      added_commit=$(echo "${diffstats_commit}" | cut -d'+' -f2 | cut -d'/' -f1)
      deleted_commit=$(echo "${diffstats_commit}" | cut -d'-' -f2)
    fi
  fi

  # Format:
  # files_modified: +lines_added/-lines_deleted
  diffstats_local="$(hg status -T '{diffstat}')"
  added_local=$(echo "${diffstats_local}" | cut -d'+' -f2 | cut -d'/' -f1)
  deleted_local=$(echo "${diffstats_local}" | cut -d'-' -f2)

  modified_total=$((added_local+added_commit+deleted_local+deleted_commit))

  if [[ ${modified_total} -lt 49 ]]; then
    printf "\033[0;32m${modified_total}\033[0m"
  elif [[ ${modified_total} -lt 250 ]]; then
    printf "\033[1;32m${modified_total}\033[0m"
  elif [[ ${modified_total} -lt 500 ]]; then
    printf "\033[0;33m${modified_total}\033[0m"
  elif [[ ${modified_total} -lt 1000 ]]; then
    printf "\033[1;33m${modified_total}\033[0m"
  elif [[ ${modified_total} -lt 1500 ]]; then
    printf "\033[0;31m${modified_total}\033[0m"
  else
    printf "\033[1;31m${modified_total}\033[0m"
  fi
}

function fzf-navigate {
  local _pwd="$(pwd)"
  find "$1" -type f | fzf --ansi --query ''\
    --preview 'batcat --color=always {}' \
    --bind "ctrl-/:execute:tmux display-popup -w '80%' -h '80%' -d '${_pwd}' -T '{}' -E batcat paging=always --style='numbers,changes' --color=always {}" \
    --header 'ctrl-/: popup batcat'
}

function tmux-send-to-bash-panes {
  # Sends keys to all panes whose foreground command is bash.
  _TMUX_PANE_PROC="bash" tmux-send-to-session-panes "$@"
}

function tmux-send-to-session-panes {
  # Sends keys to all panes whose foreground command is _TMUX_PANE_PROC (default: bash).
  local process="${_TMUX_PANE_PROC}"
  [[ -z "${process}" ]] && process="bash"

  for arg in "$@"; do
    tmux list-panes -s -F "#{pane_id} #{pane_current_command}" | grep "${process}"'$' | cut -d' ' -f1 | xargs -Ipaneid tmux send-keys -t'paneid' "${arg}"
  done

  local current_process=
  current_process="$(tmux display-message -p '#{pane_pid}' | xargs ps -o comm= -p $(ps -o ppid= -p $(tmux display-message -p '#{pane_pid}')) | tail -1)"

  if [[ "${current_process}" == "${process}" ]]; then
    for arg in "$@"; do
      tmux send-keys "${arg}"
    done
  fi
}

function hg-update-fzf {
  hg xl --color=always | \
    fzf --ansi \
    --preview 'echo {} | grep -Eo "[ ]([0-f]{6,})[ ]" | xargs -Iccc hg log --stat -r ccc' \
    --bind 'enter:become:echo {} | grep -Eo "[ ]([0-f]{6,})[ ]" | xargs -Iccc hg update -r ccc'
}

function wayland-maximize {
  gdbus call --session --dest org.gnome.Shell --object-path /org/gnome/Shell --method org.gnome.Shell.Eval "global.display.get_focus_window().move_resize_frame(true, 0, 0, 3840, 1080);"
}

# Function to refresh the Kitty socket inside tmux
function refresh_kitty {
  if [ -n "${TMUX}" ]; then
    eval "$(tmux show-environment -s KITTY_LISTEN_ON)"
    eval "$(tmux show-environment -s KITTY_PUBLIC_KEY)"
    eval "$(tmux show-environment -s KITTY_PID)"
    eval "$(tmux show-environment -s KITTY_WINDOW_ID)"
  fi
}
