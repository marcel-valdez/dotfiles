# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
  # include .bashrc if it exists
  if [ -f "$HOME/.bashrc" ]; then
    source "$HOME/.bashrc"
  fi
else
  # As a general thing, try not to put anything here and always use
  # BASH to execute your scripts, unless you want them to be -fast-
  # and not load all the .bashrc file
  # This is mainly useful for scripts that are not run from the
  # command line, and therefore don't need to execute .bashrc

  # Instructions required even when not in bash shell
  if [ -d "$HOME/bin" ]; then
    export PATH=$PATH:"$HOME/bin"
  fi

  # NOTE: I am uncertain if NVM can be run like this in a non-interactive
  # shell session.
  [ -s "$NVM_DIR/nvm.sh" ] && source "$NVM_DIR/nvm.sh"
  [[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
fi
