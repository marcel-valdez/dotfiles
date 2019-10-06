
[[ -s "$HOME/.profile" ]] && source "$HOME/.profile" # Load the default .profile

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

[[ -r "${HOME}/.bashrc" ]] && source "${HOME}/.bashrc"
# BEGIN: Block added by chef, to set environment strings
# Please see https://fburl.com/AndroidProvisioning if you do not use bash
# or if you would rather this bit of code 'live' somewhere else
[[ -r "${HOME}/.fbchef/environment" ]] && source "${HOME}/.fbchef/environment"
# END: Block added by chef
