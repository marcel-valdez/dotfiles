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
    echo "Starting ssh-agent"
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
 echo "Loading BASH completions"
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

# quick checkout
git_quick_checkout ()
{
 touch artifacts/*.touch
 git checkout $1
 touch artifacts/*.touch
}

# b) function cd_func
# This function defines a 'cd' replacement function capable of keeping,
# displaying and accessing history of visited directories, up to 10 entries.
# To use it, uncomment it, source this file and try 'cd --'.
# acd_func 1.0.5, 10-nov-2004
# Petar Marinov, http:/geocities.com/h2428, this is public domain
 cd_func ()
 {
   local x2 the_new_dir adir index
   local -i cnt

   if [[ $1 ==  "--" ]]; then
     dirs -v
     return 0
   fi

   the_new_dir=$1
   [[ -z $1 ]] && the_new_dir=$HOME

   if [[ ${the_new_dir:0:1} == '-' ]]; then
     #
     # Extract dir N from dirs
     index=${the_new_dir:1}
     [[ -z $index ]] && index=1
     adir=$(dirs +$index)
     [[ -z $adir ]] && return 1
     the_new_dir=$adir
   fi

    #
   # '~' has to be substituted by ${HOME}
   [[ ${the_new_dir:0:1} == '~' ]] && the_new_dir="${HOME}${the_new_dir:1}"

   #
   # Now change to the new dir and add to the top of the stack
   pushd "${the_new_dir}" > /dev/null
   [[ $? -ne 0 ]] && return 1
   the_new_dir=$(pwd)

   #
   # Trim down everything beyond 11th entry
   popd -n +11 2>/dev/null 1>/dev/null

   #
   # Remove any other occurence of this dir, skipping the top of the stack
   for ((cnt=1; cnt <= 10; cnt++)); do
     x2=$(dirs +${cnt} 2>/dev/null)
     [[ $? -ne 0 ]] && return 0
     [[ ${x2:0:1} == '~' ]] && x2="${HOME}${x2:1}"
     if [[ "${x2}" == "${the_new_dir}" ]]; then
       popd -n +$cnt 2>/dev/null 1>/dev/null
       cnt=cnt-1
     fi
   done

   return 0
 }

 # Appian development functions

 gwt_refresh() {
  cdd "/cygdrive/c/work/ae$1"
  echo 'Deleting previous gwt components SNAPSHOT on ae$1'
  rm -f `find . -iregex .*appian-gwt-components-0-SNAPSHOT.jar`
  rm -f `find . -iregex .*appian-gwt-components-0-SNAPSHOT-sources.jar`

  cdd ~/.m2/
  rm -f `find . -iregex .*appian-gwt-components-0-SNAPSHOT.jar`
  rm -f `find . -iregex .*appian-gwt-components-0-SNAPSHOT-sources.jar`

  cdd "/cygdrive/c/work/ae$1/build"
  echo 'Recompiling tempo on ae$1'
  ant -Dmaven.quick=true -Dproject.refs=true compile-tempo

  echo 'Copying new components SNAPSHOT on ae$1'
  echo "yes" | cp -f "/cygdrive/c/work/ae$1/gwt/appian-gwt-components/target/appian-gwt-components-0-SNAPSHOT.jar" "/cygdrive/c/work/ae$1/web/maven-lib/"
  echo "yes" | cp -f "/cygdrive/c/work/ae$1/gwt/appian-gwt-components/target/appian-gwt-components-0-SNAPSHOT-sources.jar" "/cygdrive/c/work/ae$1/web/maven-lib-src/"
  echo "yes" | cp -f "/cygdrive/c/work/ae$1/gwt/appian-gwt-components/target/appian-gwt-components-0-SNAPSHOT.jar" "/cygdrive/c/work/ae$1/test/maven-lib/"
  echo "yes" | cp -f "/cygdrive/c/work/ae$1/gwt/appian-gwt-components/target/appian-gwt-components-0-SNAPSHOT-sources.jar" "/cygdrive/c/work/ae$1/test/maven-lib-src/"
  echo "yes" | cp -f "/cygdrive/c/work/ae$1/gwt/appian-gwt-components/target/appian-gwt-components-0-SNAPSHOT.jar" "/cygdrive/c/work/ae$1/test-gwt/maven-lib/"
  echo "yes" | cp -f "/cygdrive/c/work/ae$1/gwt/appian-gwt-components/target/appian-gwt-components-0-SNAPSHOT-sources.jar" "/cygdrive/c/work/ae$1/test-gwt/maven-lib-src/"
  echo "yes" | cp -f "/cygdrive/c/work/ae$1/gwt/appian-gwt-components/target/appian-gwt-components-0-SNAPSHOT.jar" "/cygdrive/c/work/ae$1/exprdesigner/maven-lib/"
  echo "yes" | cp -f "/cygdrive/c/work/ae$1/gwt/appian-gwt-components/target/appian-gwt-components-0-SNAPSHOT-sources.jar" "/cygdrive/c/work/ae$1/exprdesigner/maven-lib-src/"
  echo "yes" | cp -f "/cygdrive/c/work/ae$1/gwt/appian-gwt-components/target/appian-gwt-components-0-SNAPSHOT.jar" "~/.m2/repository/com/appian/appian-gwt-components/0-SNAPSHOT/"
  echo "yes" | cp -f "/cygdrive/c/work/ae$1/gwt/appian-gwt-components/target/appian-gwt-components-0-SNAPSHOT-sources.jar" "~/.m2/repository/com/appian/appian-gwt-components/0-SNAPSHOT/"

  cdd +2
 }

 gwt_components_delete() {
  cdd "/cygdrive/c/work/ae$1"
  rm -f `find . -iregex .*appian-gwt-components-0-SNAPSHOT.jar`
  rm -f `find . -iregex .*appian-gwt-components-0-SNAPSHOT-sources.jar`

  cdd ~/.m2/
  rm -f `find . -iregex .*appian-gwt-components-0-SNAPSHOT.jar`
  rm -f `find . -iregex .*appian-gwt-components-0-SNAPSHOT-sources.jar`

  cdd +1
 }

 gwt_components_copy() {
  echo "yes" | cp -f "/cygdrive/c/work/ae$1/gwt/appian-gwt-components/target/appian-gwt-components-0-SNAPSHOT.jar" "/cygdrive/c/work/ae$1/web/maven-lib/"
  echo "yes" | cp -f "/cygdrive/c/work/ae$1/gwt/appian-gwt-components/target/appian-gwt-components-0-SNAPSHOT-sources.jar" "/cygdrive/c/work/ae$1/web/maven-lib-src/"

  echo "yes" | cp -f "/cygdrive/c/work/ae$1/gwt/appian-gwt-components/target/appian-gwt-components-0-SNAPSHOT.jar" "/cygdrive/c/work/ae$1/test/maven-lib/"
  echo "yes" | cp -f "/cygdrive/c/work/ae$1/gwt/appian-gwt-components/target/appian-gwt-components-0-SNAPSHOT-sources.jar" "/cygdrive/c/work/ae$1/test/maven-lib-src/"

  echo "yes" | cp -f "/cygdrive/c/work/ae$1/gwt/appian-gwt-components/target/appian-gwt-components-0-SNAPSHOT.jar" "/cygdrive/c/work/ae$1/test-gwt/maven-lib/"
  echo "yes" | cp -f "/cygdrive/c/work/ae$1/gwt/appian-gwt-components/target/appian-gwt-components-0-SNAPSHOT-sources.jar" "/cygdrive/c/work/ae$1/test-gwt/maven-lib-src/"

  echo "yes" | cp -f "/cygdrive/c/work/ae$1/gwt/appian-gwt-components/target/appian-gwt-components-0-SNAPSHOT.jar" "/cygdrive/c/work/ae$1/exprdesigner/maven-lib/"
  echo "yes" | cp -f "/cygdrive/c/work/ae$1/gwt/appian-gwt-components/target/appian-gwt-components-0-SNAPSHOT-sources.jar" "/cygdrive/c/work/ae$1/exprdesigner/maven-lib-src/"

  echo "yes" | cp -f "/cygdrive/c/work/ae$1/gwt/appian-gwt-components/target/appian-gwt-components-0-SNAPSHOT.jar" ~/.m2/repository/com/appian/appian-gwt-components/0-SNAPSHOT/
  echo "yes" | cp -f "/cygdrive/c/work/ae$1/gwt/appian-gwt-components/target/appian-gwt-components-0-SNAPSHOT-sources.jar" ~/.m2/repository/com/appian/appian-gwt-components/0-SNAPSHOT/
 }

 edit_bashrc_func() {
  subl $HOME/.bashrc
 }

 ae() {
  if [ "$(expr substr $(uname) 1 6)" == "CYGWIN" ]; then
    cd "/cygdrive/c/work/ae$1"
  else
    cd "/c/work/ae$1"
  fi
 }

 in_git_repo() {
   git rev-parse > /dev/null 2>&1 && echo true
   return 0
 }

 merge-with-appian() {
  if [ -z "$1" ]; then
    echo "This function creates a new branch off of your current branch, and then"
    echo "merges that branch with a appian/xxx branch, where xxx is the parameter"
    echo "this function accepts."
    echo ""
    echo "Error: No appian branch to merge with was supplied, you must supply one."
    echo "Example: "
    echo "merge-with-appian 7.4.0.0-maint"

    return 1;
  fi

  is_git_repo=`git rev-parse > /dev/null 2>&1 && echo true`
  if [ "$is_git_repo" != "true" ]; then
    echo "You are not inside a git repository, cannot proceed."
    return 1;
  fi

  appian_exists=`git ls-remote appian > /dev/null 2>&1 && echo true`
  if [ "$appian_exists" != "true" ]; then
    echo "The remote repository alias 'appian' does not exist."
    return 1;
  fi

  remote_branch=`git ls-remote appian | grep "$1\$"`
  if [ -z "$remote_branch" ]; then
    echo "The branch $1 does not exist in remote repository 'appian'"
    return 1;
  fi

  current_branch=`git rev-parse --abbrev-ref HEAD`
  new_branch="$current_branch-$1"
  echo "Merging branch $current_branch with: appian/$1 on new branch $new_branch"
  git branch $new_branch && git checkout $new_branch && git merge appian/$1

  return 0;
 }

 list-files() {
   sed_expr="s/#\t$1\:\s*//"
   echo `git status | grep "$1" | sed $sed_expr`

   return 0
 }

 alias cdd=cd_func

 # Custom alias
 alias top_ten="history | sed 's/^ \+//;s/  / /' | cut -d' ' -f2- | awk '{ count[\$0]++ } END { for (i in count) print count[i], i }' | sort -rn |head -10"
 alias mgit='/cygdrive/c/Program\ Files\ \(x86\)/Git/bin/git.exe'
 alias rm_merge_files='rm -f `find | grep ".*\.\(REMOTE\|LOCAL\|BASE\).*\.java"`'
 alias gstatus='git status'
 alias gcommit='git commit'
 alias load_bashrc='source $HOME/.bashrc'
 alias gadd='git add'
 alias edit_bashrc=edit_bashrc_func
 alias current-branch='git rev-parse --abbrev-ref HEAD'
 alias refresh-projects='ant -Dmaven.quick=true -Dproject.refs=true eclipse-projects-clean eclipse-projects'

 # alias edit_bashrc='nano $HOME/.bashrc; source $HOME/.bashrc'
 alias read_log='tail -f -n 3000'
 alias mysql_connect='mysql.exe --host=localhost --password=appian --user=appian --port=3306 --protocol=tcp'
 alias c='cd /cygdrive/c/'
 alias d='cd /cygdrive/d/'
 alias z='cd /cygdrive/z/'
 alias a='cd /cygdrive/a/'
 alias wget_page='wget -q -O -'
 alias wget_appian='wget_page http://localhost:8080/ae/tempo'
 alias diff_staged='git diff --staged'
 alias unstage='git reset HEAD'
 alias winpath='cygpath -a -w'
 alias g='git'
 alias webserver='python -m SimpleHTTPServer'
 alias search='find -iregex'
 alias nano='nano -F'

 # PATH modifications
 export PATH=$PATH:/cygdrive/c/static/gwt-2.5.1
 export PATH=$PATH:/cygdrive/c/Program\ Files\ \(x86\)/Araxis/Araxis\ Merge\ v6.5
 export PATH=$PATH:/cygdrive/c/programs/beyond_compare_3/
 export PATH=$PATH:/cygdrive/c/program_files/Sublime\ Text\ 2/
 export PATH=$PATH:/cygdrive/c/program_files/JPSoft/TCCLE13x64/
 export PATH=$PATH:/cygdrive/c/static/tools
 export PATH=$PATH:/cygdrive/d/static/tools
 export PATH=$PATH:/cygdrive/c/programs/nodejs
 export PATH=$PATH:~/bin

# custom environment variables
 source .ghi_token
 export TMPDIR=/tmp

 if [ "$(expr substr $(uname) 1 6)" == "CYGWIN" ]; then
  export CYGWIN_ENV=TRUE
  export GIT_EDITOR=nano
  alias apt-get='apt-cyg -m http://mirror.symnds.com/software/cygwin/x86_64/'
  alias multipane='tmux new -s CYGWIN'
  if [[ "$TERM" != "screen" ]]; then
    tmux attach -d
  fi
 fi
