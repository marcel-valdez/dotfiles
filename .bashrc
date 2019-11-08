# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
  *i*) ;;
    *) return;;
esac

function ::util::log_debug() {
  [ "${DEBUG_BASHRC}" != "" ] && echo "$(date +%H:%M:%S) $1"
}

function tmux_attach_initial_session() {
  local _tmux=tmux
  type tmx2 &>/dev/null && _tmux=tmx2

  if [ "${TMUX_INIT_SESSION}" == "" ]; then
    ::util::log_debug "tmux init: attaching to default session"
    "${_tmux}" new-session -s "default" >&/dev/null \
    || "${_tmux}" attach-session -d -t "default"
  else
    ::util::log_debug "tmux init: attaching to ${TMUX_INIT_SESSION}"
    "${_tmux}" new-session -s "${TMUX_INIT_SESSION}" >&/dev/null \
    || "${_tmux}" attach-session -d -t "${TMUX_INIT_SESSION}"
  fi
}

# load the facebook rc first, so we can override some values
if [ -f "${HOME}/.fb.d/.fbrc" ]; then
  source "${HOME}/.fb.d/.fbrc"
fi

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
export HISTCONTROL=ignoreboth:erasedups

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
export PROMPT_COMMAND="history -a"

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
  debian_chroot=$(cat /etc/debian_chroot)
fi


# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

::util::log_debug "Loading .bash* sources"
if [ -f "${HOME}/.bash_aliases" ]; then
  source "${HOME}/.bash_aliases"
fi

if [ -f "${HOME}/.bash_functions" ]; then
  source "${HOME}/.bash_functions"
fi
::util::log_debug "Loaded .bash* sources"

::util::log_debug "Loading .fb* sources"
if [ -f "${HOME}/.fbrc.d/.fb_aliases" ]; then
  source "${HOME}/.fbrc.d/.fb_aliases"
fi

if [ -f "${HOME}/.fbrc.d/.fb_functions" ]; then
  source "${HOME}/.fbrc.d/.fb_functions"
fi
::util::log_debug "Loaded .fb* sources"

# set a fancy prompt (non-color, unless we know we "want" color)
case "${TERM}" in
  xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "${force_color_prompt}" ]; then
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

::util::log_debug "Setting PS1 (prompt)"
[[ -f "${HOME}/lib/git-prompt.sh" ]] && source "${HOME}/lib/git-prompt.sh"

short_hostname=$(echo ${HOSTNAME} | egrep '^.{0,20}' | head -1)
if [ "${color_prompt}" = yes ]; then
  ::util::log_debug "Using color_prompt PS1"
  export PS1="[Exit: \[\033[1;31m\]\${PIPESTATUS[@]/#0/\[\033[0m\]\[\033[1;32m\]0\[\033[1;31m\]}\[\033[0m\]]"
else
  ::util::log_debug "Using non-color prompt PS1"
  export PS1="[Exit: \${PIPESTATUS[@]/#0/0}]"
fi

# Show the current bookmark or branch
if type _scm_prompt &>/dev/null; then
  ::util::log_debug "Using _scm_prompt"
  export PS1="$PS1"'$(_scm_prompt) \w\n\$ '
elif type __git_ps1 &>/dev/null; then
  export PS1="$PS1"'$(__git_ps1) \w\n\$ '
fi


::util::log_debug "Done setting PS1 (prompt)"

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
# alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

::util::log_debug "Loading bash_completion"
# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    source /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    source /etc/bash_completion
  fi
fi

::util::log_debug "Loaded bash_completion"

# Path additions
if [ -d "${HOME}/bin" ]; then
  export PATH="${PATH}:${HOME}/bin"
fi

if [ -d "${HOME}/.local/bin" ]; then
  export PATH="${PATH}:${HOME}/.local/bin"
fi
# Add RVM to PATH for scripting
# custom environment variables
# export J2D_PIXMAPS="shared"
export NVM_DIR="${HOME}/.nvm"
export CHECK_PACKAGES_CONFIG="${HOME}/.launchpad-helper"
export NODE_VERSION="9.5.0"
export BIN_UTILS_PASS_PHRASE_FILE="${HOME}/.bin-utils-pass-phrase"
export SUBLIME_PROJECTS_FOLDER="${HOME}/sublime_projects"

if [ "$(expr substr $(uname) 1 5)" == "Linux" ]; then
  if [[ "$TERM" =~ "eterm" ]]; then
    export EDITOR="emacs-client"
    export GIT_EDITOR=$EDITOR
  else
    export EDITOR="emacs --no-window-system"
    export GIT_EDITOR=$EDITOR
  fi
  # if the terminal has not been initialized yet
  if [ -z "${TERMINAL_SESSION_INITIALIZED}" ]; then
    # This is slow, so we do not want to do it for every TMUX pane
    ::util::log_debug "Loading SSH session"
    source "${HOME}/lib/ssh-persist-session.sh"
    ::util::log_debug "Loaded SSH session"
    export TERMINAL_SESSION_INITIALIZED="true"
  fi

  # if we are not within tmux and not within an emacs ansi-term
  # start or join a tmux session
  if [ "$TMUX" == "" ] && [[ ! "$TERM" =~ "eterm" ]] ; then
    # this will run once per non-eterm terminal opened
    tmux_attach_initial_session
  fi

  # this will run for every terminal opened and tmux pane
  if type verify-packages &>/dev/null; then
    ::util::log_debug "Verifying packages with cache"
    verify-packages --use-cache
    ::util::log_debug "Verified packages with cache"
  fi
fi

# This loads nvm
::util::log_debug "Loading NVM"
[ -s "${NVM_DIR}/nvm.sh" ] && source "${NVM_DIR}/nvm.sh"
# This loads nvm bash_completion
::util::log_debug "Loading NVM bash completion"
[ -s "${NVM_DIR}/bash_completion" ] && source "${NVM_DIR}/bash_completion"
::util::log_debug "Loaded NVM"

# Load RVM into a shell session *as a function*
::util::log_debug "Loading RVM"
[[ -d "/usr/share/rvm/scripts/" ]] && source "/usr/share/rvm/scripts/rvm"
[[ -d "/usr/share/rvm/bin" ]] && export PATH="${PATH}:/usr/share/rvm/bin"
[[ -s "${HOME}/.rvm/scripts/rvm" ]] && source "${HOME}/.rvm/scripts/rvm"
[[ -d "${PATH}:${HOME}/.rvm/bin" ]] && export PATH="${PATH}:${HOME}/.rvm/bin" # Add RVM to PATH for scripting
::util::log_debug "Loaded RVM"

# This sets up the default node version and loads it
::util::log_debug "Loading node.js"
node-check-use --silent
::util::log_debug "Loaded node.js"

# Reads the pending log buffer
log-buffer --read
# devserver-specific
if [[ "${HOSTNAME}" =~ .*facebook.com.* ]]; then
  export http_proxy='http://fwdproxy:8080'
  export https_proxy='http://fwdproxy:8080'
else
  export http_proxy=''
  export https_proxy=''
fi

export ftp_proxy=''
export socks_proxy=''

[ -f "${HOME}/.fzf.bash" ] && source "${HOME}/.fzf.bash"
