# base-files version 3.9-3
# set -x
# To pick up the latest recommended .bash_profile content,
# look in /etc/defaults/etc/skel/.bash_profile

# Modifying /etc/skel/.bash_profile directly will prevent
# setup from updating it.

# The copy in your home directory (~/.bash_profile) is yours, please
# feel free to customise it to create a shell
# environment to your liking.  If you feel a change
# would be benifitial to all, please feel free to send
# a patch to the cygwin mailing list.

if [ "$HOME" != "/home/marcel" ]; then
  export HOME="/home/marcel"
fi

# source the system wide bashrc if it exists
if [ -e /etc/bash.bashrc ] ; then
  source /etc/bash.bashrc
fi

# ~/.bash_profile: executed by bash for login shells.

# source the users bashrc if it exists
if [ -e "${HOME}/.bashrc" ] ; then
  source "${HOME}/.bashrc"
fi

# Set PATH so it includes user's private bin if it exists
if [ -d "${HOME}/bin" ] ; then
   PATH=${HOME}/bin:${PATH}
fi

# Set MANPATH so it includes users' private man if it exists
# if [ -d "${HOME}/man" ]; then
#   MANPATH=${HOME}/man:${MANPATH}
# fi

# Set INFOPATH so it includes users' private info if it exists
# if [ -d "${HOME}/info" ]; then
#   INFOPATH=${HOME}/info:${INFOPATH}
# fi

#PATH=/cygdrive/c/Windows/Microsoft.NET/Framework/v4.0.30319:${PATH}
#if [ -d "/cygdrive/d/static/tools" ] ; then
#  PATH=/cygdrive/d/static/tools:${PATH}
#fi
