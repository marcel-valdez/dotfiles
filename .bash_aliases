alias show-keyboard='gkbd-keyboard-display -g 1'
alias ipconfig='ifconfig'
alias python-webserver='python3 -m http.server'
alias copy-to-clip='xclip -sel clip'
alias top-ten="history | sed 's/^ \+//;s/  / /' | cut -d' ' -f2- | awk '{ count[\$0]++ } END { for (i in count) print count[i], i }' | sort -rn |head -10"
alias start-android-notifier='~/modules/android-notifier-desktop/run.sh 2>&1 >> ~/modules/android-notifier-desktop/status.log  &'
alias intellij='/home/marcel/modules/idea-IC-141.1010.3/bin/idea.sh'
alias now='date "+%H:%M:%S  %d/%m/%y"'
alias appian-openvpn='sudo openvpn --script-security 2 --up /etc/openvpn/openvpn-dns --down /etc/openvpn/openvpn-dns --config ~/.openvpn/client.ovpn'
alias nano-crypto=crypto-file
alias rm-secure='shred -u -z'

# start git commands

alias g='git'
alias g-add='git add'
alias g-fetch='git fetch'
alias g-clean-dfx='git-clean-dfx'
alias g-commit-sha='git-commit-sha'
alias g-fix-whitespace='git-fix-whitespace'
alias g-log-styled='git-log-styled'
alias g-merge-log='git-merge-log'
alias g-show-files='git-show-files'

alias g-commit='git commit'
alias g-cm='git commit'

alias g-status='git status'
alias g-st='git status'

alias g-push-f-marcel='git-push-f-marcel'
alias g-push-marcel='git-push-marcel'

alias git-amend='git commit --amend'
alias g-amend='git-amend'

alias git-amend-no-edit='git commit --amend --no-edit'
alias g-amend-no-edit='git-amend-no-edit'

alias git-diff-staged='git diff --staged'
alias g-diff-staged='git-diff-staged'

alias g-log='git log'
alias git-log-one-author='git log --pretty=format:"%h %an %ad %s" --date=short'
alias g-log-one-author='git-log-one-author'

alias git-status-uno='git status -uno'
alias g-status-uno='git-status-uno'

alias git-unstage='git reset HEAD'
alias g-unstage='git-unstage'

alias git-current-branch='git rev-parse --abbrev-ref HEAD'
alias g-current-branch='git-current-branch'

alias git-pull-rebase='git pull --rebase'
alias g-pull-rebase='git-pull-rebase'

# aliases to things not created by me
alias g-shell='git-shell'
alias g-upload-archive='git-upload-archive'
alias g-receive-pack='git-receive-pack'
alias g-upload-pack='git-load-pack'

# end git commands
