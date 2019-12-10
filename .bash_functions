#!/usr/bin/env bash

EMACS_VERSION="25.1.91"

# alert function for long running commands.  Use like so:
#   sleep 10; alert
function alert() {
  msg="Terminal"
  [ $? -eq 0 ] && msg="Error"
  prevous_cmd="$(history | tail -n2 | head -n1 | sed -e 's/  */ /g' | cut -d' ' -f3-)"
  notify-send --urgency=low\
              -i "${msg}"\
              "${previous_cmd}"
}

function open-bg() {
  filename=$(basename $1)
  logfile="/tmp/$USER-$filename.log"
  $1 >&${logfile} &
}

function compress-pdf() {

  if [ "$1" == "--help" ]; then
    echo "This command compresses a pdf to enough quality for documents with images (like passports)"
    echo "Usage:"
    echo "compress-pdf <input-file> [output-file]"
    echo "input-file: The PDF file to compress"
    echo "output-file: The name of the output file to compress, if no output-file is given"
    echo "             then a file called compressed-<input-file> is created."
  fi

  input_file=$1
  output_file=$2
  if [ "$output_file" == "" ]; then
    output_file="compressed-$(basename $input_file)"
  fi

  compression_level="/ebook"
  # /screen selects low-resolution output similar to the Acrobat Distiller "Screen Optimized" setting.
  # /ebook selects medium-resolution output similar to the Acrobat Distiller "eBook" setting.
  # /printer selects output similar to the Acrobat Distiller "Print Optimized" setting.
  # /prepress selects output similar to Acrobat Distiller "Prepress Optimized" setting.
  # /default selects output intended to be useful across a wide variety of uses, possibly at the expense of a larger output file.
  gs -dNOPAUSE -dQUIET -dBATCH \
     -sDEVICE=pdfwrite \
     -dCompatibilityLevel=1.4 \
     -dPDFSETTINGS=$compression_level \
     -sOutputFile=$output_file $input_file
  # other compression options for pdfwrite
  # -dr[resolution] -> default setting is 720dpi
  # -dDetectDuplicateImages -> defaults to true, will reuse a image used more than once
  # -dCompressPages -> defaults to true
  # -dOptimize -> defaults to false, set to true on /screen, /ebook, /printer, /prepress
  # -dCompressFonts -> defaults to true, never set to false.

}

# executes a command without any output
function succeeds() {
  $@ >&/dev/null
}

# executes a command with output
function fails() {
  $@ >&/dev/null
}

function emacs() {
  local emacs_bin="$(which emacs)"
  if [[ "${EMACS_VERSION}" != "" ]] && [[ -e "/usr/local/bin/emacs-${EMACS_VERSION}" ]]; then
    emacs_bin="/usr/local/bin/emacs-${EMACS_VERSION}"
    echo "Emacs binary /usr/local/bin/emacs-${EMACS_VERSION} not found. Using ${emacs_bin} instead." >&2
  fi
  "${emacs_bin}" -nw "$@"
}

function emacs-client() {
  local editor='emacsclient --create-frame --tty --socket-name=default'
  EDITOR="${editor}" "${editor}" "$@" || \
    (emacs --bg-daemon=default && "${editor}" "$@")
}

function nano() {
  [ "$1" == "-F" ] && shift
  emacs "$@"
}

# shows the shell's keyboard shortcuts
function bind-show-shortcuts() {
  bind -p | tail -n +1 | grep -v "not bound" | grep -v "self-insert"
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

# This function reduces the horrible lag spikes caused by the NetworkManager
# scanning the wifi network in the background when using a 802.11AC enabled
# wireless network interface.
# NOTE: if this solution does not work well enough, you can alternatively
# switch from NetworkManager to wicd (manual configuration).

function wifi-pause-background-scan() {
  sudo killall -STOP NetworkManager &>/dev/null ||\
    sudo killall -STOP wicd &>/dev/null
  if [[ "$?" != "0" ]]; then
    echo "Neither a NetworkManager nor a wicd process found to STOP." >&2
  fi
  echo "Disabled background wi-fi network scan by pausing the NetworkManager/wicd process"
}

function wifi-enable-background-scan() {
  sudo killall -CONT NetworkManager &>/dev/null ||\
    sudo killall -CONT wicd &>/dev/null
  if [[ "$?" != "0" ]]; then
    echo "Neither a NetworkManager nor a wicd process found to CONTinue." >&2
  fi
  echo "Enabled background wi-fi network scan by signaling the NetworkManager/wicd process"
}


# start npm functions
function node-check-use() {
  local node_version=$(node --version 2>/dev/null)
  if [ "${node_version}" != "v${NODE_VERSION}" ]; then
    local node_version_installed=$(nvm ls 2>/dev/null | grep ${NODE_VERSION})
    if [ "${node_version_installed}" == "" ]; then
      echo Node v$NODE_VERSION is not installed, installing now.
      nvm install "v${NODE_VERSION}"
    fi

    if [ "$1" == "--silent" ]; then
      nvm use "${NODE_VERSION}" >/dev/null 2>&1
    else
      nvm use "${NODE_VERSION}"
    fi
  fi
}

function npm-tdd() {
  node-check-use
  npm run test:watch
}

function npm-install() {
  node-check-use
  npm install
}

function npm-test() {
  node-check-use
  npm run test
}

function npm-start() {
  node-check-use
  npm start
}

function npm-lint() {
  node-check-use
  npm run lint
}

# start git functions

function diff-lines() {
  local path=
  local line=
  while read; do
    esc=$'\033'
    if [[ $REPLY =~ ---\ (a/)?.* ]]; then
      continue
    elif [[ $REPLY =~ \+\+\+\ (b/)?([^[:blank:]$esc]+).* ]]; then
      path=${BASH_REMATCH[2]}
    elif [[ $REPLY =~ @@\ -[0-9]+(,[0-9]+)?\ \+([0-9]+)(,[0-9]+)?\ @@.* ]]; then
      line=${BASH_REMATCH[2]}
    elif [[ $REPLY =~ ^($esc\[[0-9;]+m)*([\ +-]) ]]; then
      echo "$path:$line:$REPLY"
      if [[ ${BASH_REMATCH[2]} != - ]]; then
        ((line++))
      fi
    fi
  done
}

function git-diff-lines() {
  git diff $1 $2 $3 $4 | diff-lines
}

function g-diff-lines() {
  git-diff-lines $1 $2 $3 $4
}

function git-branch-out() {
  new_branch_name=$1
  treeish=$2
  git checkout -b $1 $2
}

function g-branch-out() {
  git-branch-out $1 $2
}

function git-rm-forever() {
  if [ "$1" == "--help" ]; then
    echo Removes a file completely from the repository history
    exit 1
  fi

  file="$1"
  git filter-branch --force --index-filter \
      "git rm --cached --ignore-unmatch ${file}" \
      --prune-empty --tag-name-filter cat -- --all
}

function g-rm-forever() {
  git-rm-forever $1
}

function git-push-marcel() {
  git push marcel $(git-current-branch):$(git-current-branch)
}

function git-push-f-marcel() {
  git push -f marcel $(git-current-branch):$(git-current-branch)
}

# end git functions

# java management functions
function set-env-java9() {
  export JAVA_HOME='/usr/lib/jvm/java-9-openjdk-amd64'
}

function set-env-java8() {
  export JAVA_HOME='/usr/lib/jvm/java-8-openjdk-amd64'
}

function java() {
  if [[ ! -z ${JAVA_HOME} ]]; then
    ${JAVA_HOME}/bin/java $@
  else
    /usr/bin/java $@
  fi
}

function javac() {
  if [[ ! -z ${JAVA_HOME} ]]; then
    "${JAVA_HOME}/bin/java" $@
  else
    /usr/bin/javac $@
  fi
}

function generate-dotfiles-tags() {
  type etags &>/dev/null && etags ~/.bashrc ~/.bash_functions ~/.bash_aliases\
                                  ~/.bash_ssh ~/.bash_profile ~/.bash_logout\
                                  ~/.profile ~/.xinitrc ~/.xinputrc ~/.xsession
}
