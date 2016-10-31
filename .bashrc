# base-files version 3.9-3

# To pick up the latest recommended .bashrc content,
# look in /etc/defaults/etc/skel/.bashrc

# Modifying /etc/skel/.bashrc directly will prevent
# setup from updating it.

# The copy in your home directory (~/.bashrc) is yours, please
# feel free to customise it to create a shell
# environment to your liking.  If you feel a change
# would be benificial to all, please feel free to send
# a patch to the cygwin mailing list.

# User dependent .bashrc file
# alias ssh='/bin/ssh.exe'

# Add git prompt and git completion
[ -r ~/bin/git-completion ] && source ~/bin/git-completion
[ -r ~/bin/git-prompt ] && source ~/bin/git-prompt

[ -r ~/.bash_aliases ] && source ~/.bash_aliases
[ -r ~/.bash_functions ] && source ~/.bash_functions

# non-printable characters must be enclosed inside \[ and \]
PS1='\[\033[0m\]'              # VT100 compat: reset all colors
# PS1="$PS1"'\u@\h '             # user@host<space>
PS1="$PS1"'\[\033[33m\]'       # change color => yellow
PS1="$PS1"'\w'                 # current working directory
PS1="$PS1"'\[\033[32m\]'   # change color => green
PS1="$PS1"'$(__git_ps1)'   # bash function for current git branch
PS1="$PS1"'\[\033[0m\]'        # change color
PS1="$PS1"'\n'                 # new line
PS1="$PS1"'$ '                 # prompt: always $

#############################################################################
# Save ssh key
#############################################################################
# Note: ~/.ssh/environment should not be used, as it
#       already has a different purpose in SSH.
env=~/.ssh/agent.env

# Note: Don't bother checking SSH_AGENT_PID. It's not used
#       by SSH itself, and it might even be incorrect
#       (for example, when using agent-forwarding over SSH).

agent_is_running() {
  if [ "$SSH_AUTH_SOCK" ]; then
      # ssh-add returns:
      #   0 = agent running, has keys
      #   1 = agent running, no keys
      #   2 = agent not running
      ssh-add -l >/dev/null 2>&1 || [ $? -eq 1 ]
  else
      false
  fi
}

agent_has_keys() {
  ssh-add -l >/dev/null 2>&1
}

agent_load_env() {
  . "$env" >/dev/null
}

agent_start() {
  (umask 077; ssh-agent >"$env")
  . "$env" >/dev/null
}

if [ "$SESSION" != "screen" ]; then

    if ! agent_is_running; then
        agent_load_env
    fi

    if ! agent_is_running; then
        agent_start
        ssh-add
    elif ! agent_has_keys; then
        ssh-add
    fi
fi

unset env

# Environment Variables
# #####################

# TMP and TEMP are defined in the Windows environment.  Leaving
# them set to the default Windows temporary directory can have
# unexpected consequences.
unset TMP
unset TEMP

# Alternatively, set them to the Cygwin temporary directory
# or to any other tmp directory of your choice
export TMP=/tmp
export TEMP=/tmp

# Or use TMPDIR instead
export TMPDIR=/tmp

# Shell Options
# #############

# See man bash for more options...

# Don't wait for job termination notification
# set -o notify

# Don't use ^D to exit
# set -o ignoreeof

# Use case-insensitive filename globbing
# shopt -s nocaseglob

# Make bash append rather than overwrite the history on disk
shopt -s histappend

# When changing directory small typos can be ignored by bash
# for example, cd /vr/lgo/apaache would find /var/log/apache
shopt -s cdspell


# Completion options
# ##################

# These completion tuning parameters change the default behavior of bash_completion:

# Define to access remotely checked-out files over passwordless ssh for CVS
# COMP_CVS_REMOTE=1

# Define to avoid stripping description in --option=description of './configure --help'
# COMP_CONFIGURE_HINTS=1

# Define to avoid flattening internal contents of tar files
# COMP_TAR_INTERNAL_PATHS=1

# If this shell is interactive, turn on programmable completion enhancements.
# Any completions you add in ~/.bash_completion are sourced last.
case $- in
    *i*) [[ -f /etc/bash_completion ]] && . /etc/bash_completion ;;
esac


# History Options
# ###############
# Don't put duplicate lines in the history.
export HISTCONTROL="ignoredups"

# Ignore some controlling instructions
# HISTIGNORE is a colon-delimited list of patterns which should be excluded.
# The '&' is a special pattern which suppresses duplicate entries.
# export HISTIGNORE=$'[ \t]*:&:[fb]g:exit'
export HISTIGNORE=$'[ \t]*:&:[fb]g:exit:ls' # Ignore the ls command as well

# Whenever displaying the prompt, write the previous line to disk
export PROMPT_COMMAND="history -a"

# Functions
# #########

# Some example functions
# function settitle() { echo -ne "\e]2;$@\a\e]1;$@\a"; }
source ~/bin/functions

# Aliases
# #######
# cd_func is sourced from ~/bin/functions
alias cd=cd_func

# Some example alias instructions
# If these are enabled they will be used instead of any instructions
# they may mask.  For example, alias rm='rm -i' will mask the rm
# application.  To override the alias instruction use a \ before, ie
# \rm will call the real rm not the alias.

# Interactive operation...
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

# Default to human readable figures
alias df='df -h'
alias du='du -h'

# Misc :)
alias less='less -r'                          # raw control characters
alias whence='type -a'                        # where, of a sort
alias grep='grep --color'                     # show differences in colour

# Some shortcuts for different directory listings
alias ls='ls -hF --color=tty'                 # classify files in colour
alias dir='ls --color=auto --format=vertical'
alias vdir='ls --color=auto --format=long'
alias ll='ls -l'                              # long list
alias la='ls -A'                              # all but . and ..
alias l='ls -CF'                              #

# Custom Aliases
alias wget_page='wget -S -qO - '

# custom git aliases
alias git='/usr/bin/git'
alias gstatus='git status'
alias gcommit='git commit'
alias gadd='git add'
alias gpush='git push'
# custom shell aliases
alias bashrc_load='source $HOME/.bashrc'
alias bashrc_edit='nano $HOME/.bashrc'
alias nano='nano -F'
alias py='python'
alias winpath='cygpath -a -w'
alias git='/usr/bin/git'
alias rbundle='/usr/bin/bundle.bat'
alias bexec='/usr/bin/bundle.bat exec'
alias pyhttp='python -m SimpleHTTPServer'
alias remind-hr='rubykron --in 1 --message'
alias remind-30='rubykron --in 0.5 --message'
alias webserver='python -m SimpleHTTPServer'

# UTF-8
#######
export LANG=en_US.UTF-8
export LOCALE=UTF-8
export LESSCHARSET='utf-8'

# PATH=$PATH:/cygdrive/d/static/gwt-2.5.1/
# PATH=$PATH:/cygdrive/d/static/ant/bin
# PATH=$PATH:/cygdrive/c/Program\ Files\ \(x86\)/Beyond\ Compare\ 3/
# PATH=~/bin:$PATH
# PATH=$PATH:/cygdrive/c/HashiCorp/Vagrant/bin
# PATH=$PATH:/cygdrive/c/ProgramData/chocolatey/bin

if [ -d "/cygdrive/d/static/tools" ] ; then
    PATH=/cygdrive/d/static/tools:$PATH
fi

if [ "$(expr substr $(uname) 1 6)" == "CYGWIN" ]; then
    export EDITOR="emacs -nw"
    [[ "${TERM}" =~ "eterm" ]] && export EDITOR="emacsclient"
    export GIT_EDITOR=${EDITOR}
    if [ -z "${TMUX}" ] && [[ ! "${TERM}" =~ "eterm" ]] ; then
        tmux new -s default >&/dev/null || tmux attach -t default
    fi
fi
