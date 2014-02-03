# To the extent possible under law, the author(s) have dedicated all 
# copyright and related and neighboring rights to this software to the 
# public domain worldwide. This software is distributed without any warranty. 
# You should have received a copy of the CC0 Public Domain Dedication along 
# with this software. 
# If not, see <http://creativecommons.org/publicdomain/zero/1.0/>. 

# base-files version 4.1-1

# ~/.bash_profile: executed by bash(1) for login shells.

# The latest version as installed by the Cygwin Setup program can
# always be found at /etc/defaults/etc/skel/.bash_profile

# Modifying /etc/skel/.bash_profile directly will prevent
# setup from updating it.

# The copy in your home directory (~/.bash_profile) is yours, please
# feel free to customise it to create a shell
# environment to your liking.  If you feel a change
# would be benifitial to all, please feel free to send
# a patch to the cygwin mailing list.

# User dependent .bash_profile file
. /etc/.git-completion.sh
[ -r /etc/git-prompt.sh ] && . /etc/git-prompt.sh

# non-printable characters must be enclosed inside \[ and \]
PS1='\[\033[0m\]'              # VT100 compat: reset all colors
PS1="$PS1"'\[\033[32m\]'       # change color
PS1="$PS1"'\u@\h '             # user@host<space>
PS1="$PS1"'\[\033[33m\]'       # change color
PS1="$PS1"'\w'                 # current working directory
if test -z "$WINELOADERNOEXEC"
then
    PS1="$PS1"'$(__git_ps1)'   # bash function
fi
PS1="$PS1"'\[\033[0m\]'        # change color
PS1="$PS1"'\n'                 # new line
                               # show MSYSTEM if non-standard
# if [ "$MSYSTEM" != "MINGW32" ]; then
#     PS1="$PS1"'\[\033[35m\]($MSYSTEM)\[\033[0m\] '
# fi
PS1="$PS1"'$ '                 # prompt: always $
# source the users bashrc if it exists
if [ -f "${HOME}/.bashrc" ] ; then
  source "${HOME}/.bashrc"
fi

# Set PATH so it includes user's private bin if it exists
# if [ -d "${HOME}/bin" ] ; then
#   PATH="${HOME}/bin:${PATH}"
# fi

# Set MANPATH so it includes users' private man if it exists
# if [ -d "${HOME}/man" ]; then
#   MANPATH="${HOME}/man:${MANPATH}"
# fi

# Set INFOPATH so it includes users' private info if it exists
# if [ -d "${HOME}/info" ]; then
#   INFOPATH="${HOME}/info:${INFOPATH}"
# fi
