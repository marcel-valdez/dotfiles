# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

[[ -z "${DEBUG_BASHRC:-}" ]] && export DEBUG_BASHRC=
[[ -z "${DEBUG:-}" ]] && export DEBUG=
[[ -z "${EMACS_TTY_SERVER:-}" ]] && export EMACS_TTY_SERVER="tty-server"
# enable 24-bit colors
export COLORTERM=truecolor
# If not running interactively, don't do anything
case $- in
  *i*) ;;
    *) return;;
esac

function log_debug() {
  [[ -n "${DEBUG_BASHRC:-}" ]] && echo "$(date +%H:%M:%S) $1"
}

function tmux_attach_or_create_initial_session() {
  local _tmux=tmux
  type tmx2 &>/dev/null && _tmux=tmx2

  if [ -z "${TMUX_INIT_SESSION:-}" ]; then
    log_debug "tmux init: attaching to default session"
    "${_tmux}" new-session -s "default" >&/dev/null \
    || "${_tmux}" attach-session -d -t "default"
  else
    log_debug "tmux init: attaching to ${TMUX_INIT_SESSION}"
    "${_tmux}" new-session -s "${TMUX_INIT_SESSION}" >&/dev/null \
    || "${_tmux}" attach-session -d -t "${TMUX_INIT_SESSION}"
  fi
}

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
export HISTCONTROL=ignoredups:erasedups

# append to the history file, don't overwrite it
shopt -s histappend

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
export HISTSIZE=100000
export HISTFILESIZE=200000
# history -a: append this session's new history elements to the history file
# history -c: clear this session's history list
# history -r: read the history file's entries and make them the current history list
export PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND$'\n'}"

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
  debian_chroot=$(cat /etc/debian_chroot)
fi

log_debug "Loading bash-preexec"
# Because bash-preexec relies on low-level traps (DEBUG and PROMPT_COMMAND) to
# monitor command execution, make sure it is initialized before ble.sh takes
# over your line editor, allowing both tools to hook into the shell's event loop
# without interfering with your prompt rendering.
if [[ -f /usr/share/bash-preexec/bash_preexec.sh ]]; then
    source /usr/share/bash-preexec/bash_preexec.sh
elif [[ -f ~/.bash-preexec/bash_preexec.sh ]]; then
    source ~/.bash-preexec/bash_preexec.sh
fi
log_debug "Loaded bash-preexec"

# This runs right before your prompt appears
precmd() {
  return 0
}

# The preexec function runs automatically just after you hit Enter, capturing
# the exact command-line string you typed right before the shell executes it.
# It passes the command string as an argument ($1).
preexec() {
  return 0
}



log_debug "Loading bash_completion"
# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

log_debug "Loaded bash_completion"


# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

log_debug "Loading .bash* sources"
if [ -s "${HOME}/.bash_aliases" ]; then
  . "${HOME}/.bash_aliases"
fi

if [ -s "${HOME}/.bash_functions" ]; then
  . "${HOME}/.bash_functions"
fi
log_debug "Loaded .bash* sources"

log_debug "Loading .google* sources"
if [ -s "${HOME}/.googlerc.d/.googlerc" ]; then
  . "${HOME}/.googlerc.d/.googlerc"
fi
log_debug "Loaded .google* sources"

# set a fancy prompt (non-color, unless we know we "want" color)
case "${TERM:-}" in
  xterm-color) color_prompt=yes;;
  wezterm) color_prompt=yes;;
  xterm-kitty) color_prompt=yes;;
  xterm-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "${force_color_prompt:-}" ]; then
  if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
    # We have color support; assume it's compliant with Ecma-48
    # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
    # a case would tend to support setf rather than setaf.)
    color_prompt=yes
  else
    color_prompt=
  fi
fi

# custom: It is 2017, colored prompt is ALWAYS available
color_prompt=yes
# Use MTV time.
export TZ="US/Pacific"
log_debug "Setting PS1 (prompt)"
g4_functions_exist=$(type g4-client-name >&/dev/null && echo "yes")
if [ "${g4_functions_exist:-}" == "yes" ] && [[ -z "${DISABLE_G4_PS1:-}" ]]; then
  # NOTE: This requires 256 color support.
  PS1_SUFFIX='$(g4-client-ps1) \[\033[0m\[\033[38;5;237m\D{%H:%M:%S}\[\033[0;0m\n\$ '
  WORKDIR='$(g4-workdir-ps1)'
else
  PS1_SUFFIX=' \[\033[38;5;237m\D{%H:%M:%S}\[\033[0;0m\n\$ '
  WORKDIR='\w'
fi

if [[ -z "${PS1_HOST:-}" ]]; then
  export PS1_HOST="$(hostname)"
  PS1_HOST=${PS1_HOST/.mtv.*/}
fi

if [[ "${color_prompt:-}" == "yes" ]]; then
  log_debug "Using color_prompt PS1"
  PS1="\[\033[00;1m[Exit: \[\033[1;31m\]\${PIPESTATUS[@]/#0/\[\033[0;1m\]\[\033[1;32m\]0\[\033[1;31m\]}\[\033[0;1m\]] "
else
  log_debug "Using non-color prompt PS1"
  PS1="[Exit: \${PIPESTATUS[@]/#0/0}] "
fi

if [ "${color_prompt:-}" = "yes" ]; then
  PS1="${PS1}"'${debian_chroot:+($debian_chroot)}\[\033[01;32m\]@${PS1_HOST}\[\033[00m\]:\[\033[01;34m\]'${WORKDIR}'\[\033[00;1m\]'${PS1_SUFFIX}
else
  PS1="${PS1}"'${debian_chroot:+($debian_chroot)}@${PS1_HOST}:\w'${PS1_SUFFIX}
fi

unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "${TERM:-}" in
  xterm-kitty)
    # noop
  ;;
  xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}@${PS1_HOST}: \w\a\]${PS1}"
  ;;
*)
  ;;
esac
log_debug "Done setting PS1 (prompt)"

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
  test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
  alias ls='ls --color=auto'
  #alias dir='dir --color=auto'
  #alias vdir='vdir --color=auto'

  alias grep='grep --color=auto'
  alias fgrep='fgrep --color=auto'
  alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
# alias alert='notify-send --urgency=low -i "$([ ${?} = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Path additions
if [ -d "${HOME}/bin" ]; then
  export PATH="${PATH}:${HOME}/bin"
fi

if [ -d "${HOME}/.cargo/bin" ]; then
    export PATH="${PATH}:${HOME}/.cargo/bin"
fi

if [ -d "${HOME}/.local/bin" ]; then
  export PATH="${PATH}:${HOME}/.local/bin"
fi
# Add RVM to PATH for scripting
# custom environment variables
# export J2D_PIXMAPS="shared"
export NVM_DIR="${HOME}/.nvm"
export CHECK_PACKAGES_CONFIG="${HOME}/.launchpad-helper"
export NODE_VERSION="lts"
export BIN_UTILS_PASS_PHRASE_FILE="${HOME}/.bin-utils-pass-phrase"
export SUBLIME_PROJECTS_FOLDER="${HOME}/sublime_projects"

if [ "$(expr substr $(uname) 1 5)" == "Linux" ]; then
  if [[ "${TERM:-}" =~ "eterm" ]]; then
    export EDITOR="emacs-client"
    export GIT_EDITOR="${EDITOR}"
  else
    export EDITOR="emacsclient --socket-name=${EMACS_TTY_SERVER} --tty"
    export GIT_EDITOR="${EDITOR}"
  fi
  # if the terminal has not been initialized yet
  if [ -z "${TERMINAL_SESSION_INITIALIZED:-}" ]; then
    # This is slow, so we do not want to do it for every TMUX pane
    DO_NOT_ADD_KEYS_TO_AGENT=1
    log_debug "Loading SSH session"
    source "${HOME}/lib/ssh-persist-session.sh"
    log_debug "Loaded SSH session"
    export TERMINAL_SESSION_INITIALIZED="true"
  fi

  # if we are not within tmux and not within an emacs ansi-term
  # start or join a tmux session
  if [ "${TMUX:-}" == "" ] && [[ ! "${TERM:-}" =~ "eterm" ]] ; then
    # this will run once per non-eterm terminal opened
    tmux_attach_or_create_initial_session
  fi

  # this will run for every terminal opened and tmux pane
  if type verify-packages &>/dev/null; then
    log_debug "Verifying packages with cache"
    verify-packages --use-cache
    log_debug "Verified packages with cache"
  fi
fi

if ! pgrep -af '.*emacs.*'"--daemon=${EMACS_TTY_SERVER}"'.*' &>/dev/null; then
  log_debug "No emacs daemon running. Starting one."
#  if [[ -x /usr/bin/systemd-run ]]; then
#    log_debug "systemd-run, found. Using systemd to start an emacs daemon named ${EMACS_TTY_SERVER}"
#    (/usr/bin/systemd-run --user /usr/bin/emacs --daemon="${EMACS_TTY_SERVER}") &
#
#  else
#    log_debug "systemd-run NOT found. Starting an emacs daemon named ${EMACS_TTY_SERVER}"
    (nohup /usr/bin/emacs --daemon="${EMACS_TTY_SERVER}" &> "/tmp/emacs-${EMACS_TTY_SERVER}-server.log") & disown
#  fi

else
  log_debug "Emacs daemon already running, not starting another one."
fi

# This loads nvm
log_debug "Loading NVM"
[ -s "${NVM_DIR}/nvm.sh" ] && . "${NVM_DIR}/nvm.sh"
# This loads nvm bash_completion
log_debug "Loading NVM bash completion"
[ -s "${NVM_DIR}/bash_completion" ] && . "${NVM_DIR}/bash_completion"
log_debug "Loaded NVM"

# Load RVM into a shell session *as a function*
log_debug "Loading RVM"
[ -s "${HOME}/.rvm/scripts/rvm" ] && . "${HOME}/.rvm/scripts/rvm"
log_debug "Loaded RVM"
export PATH="${PATH}:${HOME}/.rvm/bin" # Add RVM to PATH for scripting

# This sets up the default node version and loads it
log_debug "Loading node.js"
node-check-use --silent
log_debug "Loaded node.js"

# Reads the pending log buffer
log-buffer --read
export http_proxy=''
export https_proxy=''
export ftp_proxy=''
export socks_proxy=''

# Enable fzf keybindings for Bash:
[ -s /usr/share/doc/fzf/examples/key-bindings.bash ] && . /usr/share/doc/fzf/examples/key-bindings.bash
# Enable fuzzy auto-completion for Bash:
if [ -s /usr/share/doc/fzf/examples/completion.bash ]; then
  . /usr/share/doc/fzf/examples/completion.bash
fi

[ -s "${HOME}/.fzf.bash" ] && . "${HOME}/.fzf.bash"

[ -s "${HOME}/.bat.conf" ] && export BAT_CONFIG_PATH="${HOME}/.bat.conf"

[ -s "${HOME}/.cargo/env" ] && . "${HOME}/.cargo/env"

if [ -s "${HOME}/.gemini.key" ]; then
  GEMINI_API_KEY="$(cat "${HOME}/.gemini.key")"
  export GEMINI_API_KEY
fi

export GEMINI_SEARCH_TOOL="rg"
# Or if the tool requires specific flags for readable output:
export GEMINI_RG_FLAGS="--column --line-number --no-heading --color=never --smart-case"

if [[ ${BLE_VERSION-} ]]; then
  if type ble-attach &>/dev/null; then
    ble-attach
  fi
else
  if [[ -f "${HOME}/.local/share/blesh/ble.sh" ]]; then
    source -- "${HOME}/.local/share/blesh/ble.sh"
  elif [[ -f "/usr/share/blesh/ble.sh" ]]; then
    source -- "/usr/share/blesh/ble.sh"
  fi
fi

## IMPORTANT: Carapace must be loaded AFTER ble.sh (or ble-attach)
if type carapace &>/dev/null; then
  # 1. Enable bridges so carapace can steal completions from other tools
  export CARAPACE_BRIDGES='zsh,fish,inshellisense'

  # 2. Initialize the carapace engine for bash
  eval "$(carapace _carapace)"
fi

#+begin_src sh [ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \ source "$EAT_SHELL_INTEGRATION_DIR/bash"
#+end_src sh
