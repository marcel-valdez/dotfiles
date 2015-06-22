# To the extent possible under law, the author(s) have dedicated all
# copyright and related and neighboring rights to this software to the
# public domain worldwide. This software is distributed without any warranty.
# You should have received a copy of the CC0 Public Domain Dedication along
# with this software.
# If not, see <http://creativecommons.org/publicdomain/zero/1.0/>.

# base-files version 4.1-1

# ~/.bashrc: executed by bash(1) for interactive shells.

# The latest version as installed by the Cygwin Setup program can
# always be found at /etc/defaults/etc/skel/.bashrc

# Modifying /etc/skel/.bashrc directly will prevent
# setup from updating it.

# The copy in your home directory (~/.bashrc) is yours, please
# feel free to customise it to create a shell
# environment to your liking.  If you feel a change
# would be benifitial to all, please feel free to send
# a patch to the cygwin mailing list.

# User dependent .bashrc file

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

if ! agent_is_running; then
    echo "Loading SSH environment."
    agent_load_env
fi

if ! agent_is_running; then
    echo "Starting ssh-agent."
    agent_start
    ssh-add
elif ! agent_has_keys; then
    ssh-add
fi

unset env

# If not running interactively, don't do anything
[[ "$-" != *i* ]] && return

# Shell Options
#
# See man bash for more options...
#
# Don't wait for job termination notification
 set -o notify
#
# Don't use ^D to exit
 set -o ignoreeof
#
# Use case-insensitive filename globbing
 shopt -s nocaseglob
#
# Make bash append rather than overwrite the history on disk
 shopt -s histappend
#
# When changing directory small typos can be ignored by bash
# for example, cd /vr/lgo/apaache would find /var/log/apache
 shopt -s cdspell

# Completion options
#
# These completion tuning parameters change the default behavior of bash_completion:
#
# Define to access remotely checked-out files over passwordless ssh for CVS
# COMP_CVS_REMOTE=1
#
# Define to avoid stripping description in --option=description of './configure --help'
 COMP_CONFIGURE_HINTS=1
#
# Define to avoid flattening internal contents of tar files
 COMP_TAR_INTERNAL_PATHS=1
#
# Uncomment to turn on programmable completion enhancements.
# Any completions you add in ~/.bash_completion are sourced last.
 echo "Loading bash completions."
 [[ -f /etc/bash_completion ]] && . /etc/bash_completion

# export GIT_PS1_SHOWDIRTYSTATE=1
# export PS1='\[\033[0;32m\]\u@\h\[\033[0;36m\]
# \w\[\033[0;33m\]$(__git_ps1)\[\033[1;32m\] \$\[\033[00m\] '

# History Options
#
# Don't put duplicate lines in the history.
 export HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
#
# Ignore some controlling instructions
# HISTIGNORE is a colon-delimited list of patterns which should be excluded.
# The '&' is a special pattern which suppresses duplicate entries.
 export HISTIGNORE=$'[ \t]*:&:[fb]g:exit'
 export HISTIGNORE=$'[ \t]*:&:[fb]g:exit:ls' # Ignore the ls command as well
#
# Whenever displaying the prompt, write the previous line to disk
 export PROMPT_COMMAND="history -a"

export HISTSIZE=10000
export HISTFILESIZE=10000
shopt -s histappend

# Aliases
#
# Some people use a different file for aliases
 if [ -f "${HOME}/.bash_aliases" ]; then
   source "${HOME}/.bash_aliases"
 fi
#
# Some example alias instructions
# If these are enabled they will be used instead of any instructions
# they may mask.  For example, alias rm='rm -i' will mask the rm
# application.  To override the alias instruction use a \ before, ie
# \rm will call the real rm not the alias.
#
# Interactive operation...
 alias rm='rm -i'
 alias cp='cp -i'
 alias mv='mv -i'
#
# Default to human readable figures
 alias df='df -h'
 alias du='du -h'
#
# Misc :)
 alias less='less -r'                          # raw control characters
 alias whence='type -a'                        # where, of a sort
 alias grep='grep --color'                     # show differences in colour
 alias egrep='egrep --color=auto'              # show differences in colour
 alias fgrep='fgrep --color=auto'              # show differences in colour
#
# Some shortcuts for different directory listings
 alias ls='ls -hF --color=tty'                 # classify files in colour
 alias dir='ls --color=auto --format=vertical'
 alias vdir='ls --color=auto --format=long'
 alias ll='ls -l'                              # long list
 alias la='ls -A'                              # all but . and ..
 alias l='ls -CF'                              #

# Umask
#
# /etc/profile sets 022, removing write perms to group + others.
# Set a more restrictive umask: i.e. no exec perms for others:
 umask 027
# Paranoid: neither group nor others have any perms:
# umask 077

# Functions
#
# Some people use a different file for functions
 if [ -f "${HOME}/.bash_functions" ]; then
   echo "Loading bash functions."
   source "${HOME}/.bash_functions"
 fi
#
# Some example functions:
#
# a) function settitle
 # settitle ()
 # {
 #   echo -ne "\e]2;$@\a\e]1;$@\a";
 # }


 # Custom alias
 alias cdd=cd_func
 alias top_ten="history | sed 's/^ \+//;s/  / /' | cut -d' ' -f2- | awk '{ count[\$0]++ } END { for (i in count) print count[i], i }' | sort -rn |head -10"
 alias mgit='/cygdrive/c/Program\ Files\ \(x86\)/Git/bin/git.exe'
 alias rm_merge_files='rm -f `find | grep ".*\.\(REMOTE\|LOCAL\|BASE\).*\.java"`'
 alias load-bashrc='source $HOME/.bashrc'
 alias edit-bashrc=edit_bashrc_func
 alias git-current-branch='git rev-parse --abbrev-ref HEAD'
# alias git-push-marcel='git push marcel `git-current-branch`'
# alias git-push-f-marcel='git push -f marcel `git-current-branch`'
 alias ant-refresh-projects='ant -Dmaven.quick=true -Dproject.refs=true eclipse-projects-clean eclipse-projects'
 alias ant-stop-db='ant stop-k && ant stop-rdbms'
 alias ant-start-db='ant start-k && ant start-rdbms'
 alias ant-restart-db='ant-stop-db && ant-start-db'
 alias mysql_connect='mysql.exe --host=localhost --password=appian --user=appian --port=3306 --protocol=tcp'
 alias c='cd /cygdrive/c/'
 alias d='cd /cygdrive/d/'
 alias z='cd /cygdrive/z/'
 alias a='cd /cygdrive/a/'
 alias wget_page='wget -q -O -'
 alias wget_appian='wget_page http://localhost:8080/ae/tempo'
 alias git-diff-staged='git diff --staged'
 alias git-unstage='git reset HEAD'
 alias g-log='git log'
 alias g-diff-staged='git diff --staged'
 alias g-unstage='git reset HEAD'
 alias g-add='git add'
 alias g-status='git status'
 alias g-commit='git commit'
 alias g-amend='git commit --amend'
 alias winpath='cygpath -a -w'
 alias g='git'
 alias webserver='python -m SimpleHTTPServer'
 alias search='find -iregex'
 alias nano='nano -F'
 alias ant-no-gwt='ant -Dskip.gwt=true'
 alias ant-clean-dev='ant clean dev'
 alias cls='clear'
 alias search-history='history | grep -v history | grep '
 alias antt='ant -Dmaven.quick=true -Dproject.refs=true'
 alias tmux-restart='rm -rf /tmp/tmux* && tmux start-server'
 alias git-log-one-author='git log --pretty=format:"%H %an %ad" --date=short'
 alias echo='echo -n'
 alias watch='$HOME/bin/watch'

 # PATH modifications
 export PATH=$PATH:/cygdrive/c/static/gwt-2.5.1
 export PATH=$PATH:/cygdrive/c/static/mpg123
 # REMOVE ARAXIS MERGE FROM CLASS PATH
 export PATH=$PATH:/cygdrive/c/Program\ Files\ \(x86\)/Araxis/Araxis\ Merge\ v6.5
 export PATH=$PATH:/cygdrive/c/programs/beyond_compare_3/
 export PATH=$PATH:/cygdrive/c/program_files/Sublime\ Text\ 2/
 export PATH=$PATH:/cygdrive/c/program_files/JPSoft/TCCLE13x64/
 export PATH=$PATH:/cygdrive/c/static/tools
 export PATH=$PATH:/cygdrive/d/static/tools
 export PATH=$PATH:/cygdrive/c/programs/nodejs
 export PATH=$PATH:~/bin

# custom environment variables
 source ~/.ghi_token
# setup jenkins utility
 source ~/bin/auto-jenkins.sh
 setup-jenkins "https://ci.appian.com" "marcel.valdez" "Others"
 export TMPDIR=/tmp

 if [ "$(expr substr $(uname) 1 6)" == "CYGWIN" ]; then
  export CYGWIN_ENV=TRUE
  export GIT_EDITOR=nano
  alias apt-get='apt-cyg -m http://mirror.symnds.com/software/cygwin/x86_64/'
  alias multipane='tmux new -s CYGWIN'
  if [[ "$TERM" != "screen" ]]; then
    tmux attach -d || tmux new-session
  fi
 fi
