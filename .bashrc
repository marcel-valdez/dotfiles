 # ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

DEFAULT_TMUX_SESSION="default"
[ -z ${TMUX_INIT_SESSION} ] && TMUX_INIT_SESSION=${DEFAULT_TMUX_SESSION}

function log_debug() {
  [ ! -z ${DEBUG_BASHRC} ] && echo "$(date +%H:%M:%S) $1"
}

function tmux_attach_session() {
  log_debug "tmux init: attaching to <${TMUX_INIT_SESSION}>"
  tmux new-session -s ${TMUX_INIT_SESSION} >&/dev/null \
  || tmux attach-session -t ${TMUX_INIT_SESSION}
}

function is_first_time_starting_tmux() {
  tmux has-session -t ${DEFAULT_TMUX_SESSION}
}

function initialize_environment() {
  # Put commands that should execute once per shell session
  # normally there is one tmux session per each system restart

  # WINE settings
  # set to use wine simulating Windows 64-bit by default
  source ~/bin/wine-use-win64
  # uncomment to use wine simulating Windows 32-bit
  # . ~/bin/wine-use-win32

  # SSH agent environment, only load it whenever we start
  # the environment for the first time
  source ~/.bash_ssh
}

source ~/bin/functions
source ~/bin/reinstall_modules
source ~/lib/git-prompt


# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=5000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
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
short_hostname=$(echo ${HOSTNAME} | grep -oE '^[a-Z]{5}')
if [ "$color_prompt" = yes ]; then
  log_debug "Using color_prompt PS1"
  PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]@'"${short_hostname}"'\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]'
else
  log_debug "Using non-color prompt PS1"
  PS1='${debian_chroot:+($debian_chroot)}@'"${short_hostname}"':\w'
fi

PS1="$PS1"'$(__git_ps1)'
PS1="$PS1"'\n'
PS1="$PS1"'\$ '

unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir

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
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    source ~/.bash_aliases
fi

if [ -f ~/.bash_functions ]; then
  source ~/.bash_functions
fi

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

if [[ "$(uname)" =~ "Linux" ]]; then
  export GIT_EDITOR="emacs -nw"
  export EDITOR=${GIT_EDITOR}
  if [[ "${TERM}" =~ "eterm" ]]; then
      export GIT_EDITOR="emacsclient"
      export EDITOR=${GIT_EDITOR}
  fi
  # if we are not in a tmux session
  if [ -z ${TMUX} ] && [[ ! "${TERM}" =~ "eterm" ]]; then
    # initialize environment if running for the first time
    [ is_first_time_starting_tmux ] && initialize_environment
    # this will make the terminal attach to an existing tmux session or create one
    tmux_attach_session
  else
    # A new TMUX pane was created
    __ignore__=1 # only added so BASH does not hate us
    # put commands here that should execute with every opened pane
  fi
fi

export PATH="$PATH:$HOME/bin" # add local bin folder to path
export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

if [ -z ${MONO_PATH} ]; then
  export MONO_PATH="/usr/bin/continuoustests"
else
  export MONO_PATH="$MONO_PATH:/usr/bin/continuoustests"
fi

export NVM_DIR="/home/marcel/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
