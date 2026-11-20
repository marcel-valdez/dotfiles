
[[ -s "${HOME}/.profile" ]] && source "${HOME}/.profile" # Load the default .profile

[[ -s "${HOME}/.rvm/scripts/rvm" ]] && source "${HOME}/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

# Machine-local overrides (~/.bash_profile.local; only a commented template is
# committed).
if [ -r "${HOME}/.bash_profile.local" ]; then
  . "${HOME}/.bash_profile.local"
fi

# Finish with a successful command so the first prompt's [Exit: ...]
# (PIPESTATUS) does not show the status of the test above.
true
