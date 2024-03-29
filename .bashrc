# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

if [[ "${DISPLAY}" ]]; then
    if type setxkbmap &>/dev/null; then
        setxkbmap -option terminate:ctrl_alt_bksp
    fi
fi

DEFAULT_TMUX_SESSION="default"
[[ -z "${TMUX_INIT_SESSION}" ]] && TMUX_INIT_SESSION="${DEFAULT_TMUX_SESSION}"

function log_debug() {
  [[ ! -z "${DEBUG_BASHRC}" ]] && echo "$(date +%H:%M:%S) $1"
}

function tmux_attach_session() {
  log_debug "tmux init: attaching to <${TMUX_INIT_SESSION}>"
  tmux new-session -s "${TMUX_INIT_SESSION}" >&/dev/null \
    || tmux attach-session -t "${TMUX_INIT_SESSION}"
}

function is_first_time_starting_tmux() {
  tmux has-session -t "${DEFAULT_TMUX_SESSION}"
}

function initialize_environment() {
  # Put commands that should execute once per shell session
  # normally there is one tmux session per each system restart

  # WINE settings
  # set to use wine simulating Windows 64-bit by default
  [[ -f "${HOME}/bin/wine-use-win64" ]] && source "${HOME}/bin/wine-use-win64"
  # uncomment to use wine simulating Windows 32-bit
  # source "${HOME}/bin/wine-use-win32"

  # SSH agent environment, only load it whenever we start
  # the environment for the first time
  [[ -f "${HOME}/.bash_ssh" ]] && source "${HOME}/.bash_ssh"
}

[[ -f "${HOME}/bin/functions" ]] && source "${HOME}/bin/functions"
[[ -f "${HOME}/bin/reinstall_modules" ]] && source "${HOME}/bin/reinstall_modules"
[[ -f "${HOME}/lib/git-prompt" ]] && source "${HOME}/lib/git-prompt"

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
export HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
export HISTSIZE=10000
export HISTFILESIZE=50000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

export TERM="xterm-256color"

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
# shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[[ -x /usr/bin/lesspipe ]] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [[ -z "${debian_chroot:-}" ]] && [[ -r /etc/debian_chroot ]]; then
  debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "${TERM}" in
  xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes

if [[ -n "${force_color_prompt}" ]]; then
  if [[ -x /usr/bin/tput ]] && tput setaf 1 >&/dev/null; then
    # We have color support; assume it's compliant with Ecma-48
    # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
    # a case would tend to support setf rather than setaf.)
    color_prompt=yes
  else
    color_prompt=
  fi
fi

# Remove username from prompt but shortened hostname,
# in order to avoid confusion when SSHing
short_hostname=$(echo ${HOSTNAME} | egrep '^.{0,20}' | head -1)
if [[ "${color_prompt}" = "yes" ]]; then
  log_debug "Using color_prompt PS1"
  PS1="[Exit: \[\033[1;31m\]\${PIPESTATUS[@]/#0/\[\033[0m\]\[\033[1;32m\]0\[\033[1;31m\]}\[\033[0m\]] "
else
  log_debug "Using non-color prompt PS1"
  PS1="[Exit: \${PIPESTATUS[@]/#0/0}] "
fi

# Remove username from prompt but shortened hostname,
# in order to avoid confusion when SSHing
short_hostname=$(echo "${HOSTNAME}" | grep -oE '^[a-Z]{5}')
if [[ "${color_prompt}" = yes ]]; then
  log_debug "Using color_prompt PS1"
  PS1="${PS1}"'${debian_chroot:+($debian_chroot)}\[\033[01;32m\]@'"${short_hostname}"'\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]'
else
  log_debug "Using non-color prompt PS1"
  PS1="${PS1}"'${debian_chroot:+($debian_chroot)}@'"${short_hostname}"':\w'
fi

export PS1="${PS1}"'$(__git_ps1)\n'
export PS1="${PS1}"'\$ '

unset color_prompt force_color_prompt

# enable color support of ls and also add handy aliases
if [[ -x /usr/bin/dircolors ]]; then
  test -r "${HOME}/.dircolors" && eval "$(dircolors -b ~/.dircolors)"\
      || eval "$(dircolors -b)"
  alias ls='ls --color=auto'
  # alias dir='dir --color=auto'
  # alias vdir='vdir --color=auto'

  alias grep='grep --color=auto'
  alias fgrep='fgrep --color=auto'
  alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

[[ -f "${HOME}/.bash_aliases" ]] && source "${HOME}/.bash_aliases"
[[ -f "${HOME}/.bash_functions" ]] && source "${HOME}/.bash_functions"

export PATH="${PATH}:${HOME}/bin" # add local bin folder to path

if [[ -d "${HOME}/.local/bin" ]]; then
  export PATH="${PATH}:${HOME}/.local/bin"
fi

export PATH="${PATH}:${HOME}/.rvm/bin" # Add RVM to PATH for scripting
export PATH="${PATH}:${HOME}/modules/buck/bin" # Add buck to the PATH


# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [[ -f /usr/share/bash-completion/bash_completion ]]; then
    source /usr/share/bash-completion/bash_completion
  elif [[ -f /etc/bash_completion ]]; then
    source /etc/bash_completion
  fi
fi

if [[ "$(uname)" =~ "Linux" ]]; then
  export GIT_EDITOR="${HOME}/.local/bin/emacs -nw"
  if ! [[ -x "${HOME}/.local/bin/emacs" ]]; then
      export GIT_EDITOR="/snap/bin/emacs -nw"
  fi
  if ! [[ -x "/snap/bin/emacs" ]]; then
      export GIT_EDITOR="/usr/bin/emacs -nw"
  fi
  export EDITOR="${GIT_EDITOR}"

  if [[ "${TERM}" =~ "eterm" ]]; then
    export GIT_EDITOR="emacsclient"
    export EDITOR="${GIT_EDITOR}"
  fi
  # if we are not in a tmux session
  if [[ -z "${TMUX}" ]] && [[ ! "${TERM}" =~ "eterm" ]] && \
       [[ ! "${TERM_PROGRAM}" = "vscode" ]]; then
    # initialize environment if running for the first time
    is_first_time_starting_tmux && initialize_environment
    # this will make the terminal attach to an existing tmux session or create one
    tmux_attach_session
  else
    export TERM="xterm-256color"
    # A new TMUX pane was created
    __ignore__=1 # only added so BASH does not hate us
    if [[ -e "/usr/share/doc/tmux/examples/bash_completion_tmux.sh" ]]; then
      source /usr/share/doc/tmux/examples/bash_completion_tmux.sh
    fi
    # put commands here that should execute with every opened pane
  fi
fi

if ! pgrep -af '.*emacs.*--daemon=tty-server.*' &>/dev/null; then
    echo "emacs --daemon=tty-server" | at NOW
fi

if [[ -z "${MONO_PATH}" ]]; then
  export MONO_PATH="/usr/bin/continuoustests"
else
  export MONO_PATH="${MONO_PATH}:/usr/bin/continuoustests"
fi

export NVM_DIR="${HOME}/.nvm"
[[ -s "${NVM_DIR}/nvm.sh" ]] && source "${NVM_DIR}/nvm.sh"  # This loads nvm
# This sets up the default node version and loads it
export NODE_VERSION="lts"
node-check-use --silent

# Enable fzf keybindings for Bash:
[[ -f /usr/share/doc/fzf/examples/key-bindings.bash ]] && source /usr/share/doc/fzf/examples/key-bindings.bash

# Enable fuzzy auto-completion for Bash:
[[ -f /usr/share/doc/fzf/examples/completion.bash ]] && source /usr/share/doc/fzf/examples/completion.bash
[[ -f "${HOME}/.fzf.bash" ]] && source "${HOME}/.fzf.bash"
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
