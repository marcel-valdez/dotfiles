# ~/.bash_profile: bash login shells (Terminal.app, tmux panes, kitty with
# --login) read only the FIRST of ~/.bash_profile, ~/.bash_login, ~/.profile, so
# load ~/.profile (which sources ~/.bashrc) explicitly, then the machine-local
# overrides.
if [ -f "${HOME}/.profile" ]; then
  . "${HOME}/.profile"
fi

# Machine-local overrides (~/.bash_profile.local; only a commented template is
# committed).
if [ -r "${HOME}/.bash_profile.local" ]; then
  . "${HOME}/.bash_profile.local"
fi

# Finish with a successful command so the first prompt's [Exit: ...]
# (PIPESTATUS) does not show the status of the test above.
true
