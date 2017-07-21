#!/usr/bin/env bash


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
  /usr/bin/emacs -nw $@
}

function nano() {
  [ "$1" == "-F" ] && shift
  emacs $@
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

# this function reduces the horrible lag spikes caused by the NetworkManager
# scanning the wifi network in the background when using a 802.11AC enabled
# wireless network interface.
# NOTE: if this solution does not work well enough, you can alternatively
# switch from NetworkManager to wicd (manual configuration).
function wifi-pause-background-scan() {
  sudo killall -STOP NetworkManager
  echo "Disabled background wi-fi network scan by pausing the NetworkManager process"
}

function wifi-enable-background-scan() {
  sudo killall -CONT NetworkManager
  echo "Enabled background wi-fi network scan by signaling the NetworkManager process"
}


# start npm functions
node-check-use() {
  node_version=$(node --version 2>/dev/null)
  if [ "${node_version}" != "v${NODE_VERSION}" ]; then
    node_version_installed=$(nvm ls 2>/dev/null | grep ${NODE_VERSION})
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

git-rm-forever() {
  if [ "$1" == "--help" ]; then
    echo Removes a file completely from the repository history
    exit 1
  fi

  file="$1"
  git filter-branch --force --index-filter \
      "git rm --cached --ignore-unmatch ${file}" \
      --prune-empty --tag-name-filter cat -- --all
}
