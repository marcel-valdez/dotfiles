# $Id: //depot/google3/googledata/corp/puppet/goobuntu/common/modules/shell/files/bash/skel.bashrc#1 $
# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
  *i*) ;;
    *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
export HISTCONTROL=ignoreboth:erasedups

# append to the history file, don't overwrite it
shopt -s histappend

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
export HISTSIZE=30000
export HISTFILESIZE=60000
# history -a: append this session's new history elements to the history file
# history -c: clear this session's history list
# history -r: read the history file's entries and make them the current history list
export PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND$'\n'}history -a"


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
#force_color_prompt=yes

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

color_prompt=yes

if [ "$color_prompt" = yes ]; then
  #PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
  # do not display username, I am always me ;)
  PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\n\$ '
else
  #PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
  # do not display username, I am always me ;)
  PS1='${debian_chroot:+($debian_chroot)}@\h:\w\$ '
fi

unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
  #PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
  # do not display username, I am always me ;)
  PS1="\[\e]0;${debian_chroot:+($debian_chroot)}@\h: \w\a\]$PS1"
  ;;
*)
  ;;
esac

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

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f "$HOME/.bash_aliases" ]; then
  source "$HOME/.bash_aliases"
fi

if [ -f "$HOME/.bash_functions" ]; then
  source "$HOME/.bash_functions"
fi

if [ -f "$HOME/.googlerc" ]; then
  source "$HOME/.googlerc"
fi

if [ -f "$HOME/.google_aliases" ]; then
  source "$HOME/.google_aliases"
fi

if [ -f "$HOME/.google_functions" ]; then
  source "$HOME/.google_functions"
fi

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

# Path additions
if [ -d "$HOME/bin" ]; then
  export PATH=$PATH:"$HOME/bin"
fi
# Add RVM to PATH for scripting
export PATH=$PATH:"$HOME/.rvm/bin"
# custom environment variables
# export J2D_PIXMAPS="shared"
export NVM_DIR="$HOME/.nvm"
export CHECK_PACKAGES_CONFIG="$HOME/.launchpad-helper"
export NODE_VERSION="6.2.1"
export BIN_UTILS_PASS_PHRASE_FILE="$HOME/.bin-utils-pass-phrase"

if [ "$(expr substr $(uname) 1 5)" == "Linux" ]; then
  export GIT_EDITOR=nano
  export EDITOR=nano
  if [ "$TERM" == "xterm" ] || [ "$TERM" == "linux" ]; then
    # This is slow, so we do not want to do it for every TMUX pane
    source "$HOME/lib/ssh-persist-session.sh"
    # this will run for every terminal (but not for every pane)
    tmux attach
  else
    # we are already in a tmux session, this will run for every
    # tmux pane
    verify-packages --use-cache
  fi
fi

# This loads nvm
[ -s "$NVM_DIR/nvm.sh" ] && source "$NVM_DIR/nvm.sh"
# Load RVM into a shell session *as a function*
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
# This sets up the default node version and loads it
node-check-use --silent
