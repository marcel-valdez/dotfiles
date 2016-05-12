#!/bin/env bash

alias run-commits-script='make-commits-script > ~/tmp/commits.sh && nano ~/tmp/commits.sh && ~/tmp/commits.sh'
alias ant-clean-dev='ant clean dev'
alias ant-no-gwt='ant -Dskip.gwt=true'
alias ant-refresh-projects='ant -Dmaven.quick=true -Dproject.refs=true eclipse-projects-clean eclipse-projects'
alias ant-restart-db='ant-stop-db && ant-start-db'
alias ant-start-db='ant start-k && ant start-rdbms'
alias ant-stop-db='ant stop-k && ant stop-rdbms'
alias antt='ant-notify -Dmaven.quick=true -Dproject.refs=true'
alias cls='clear'
alias edit-bashrc='edit_bashrc_func'
alias load-bashrc='source $HOME/.bashrc'
alias mysql_connect='mysql.exe --host=localhost --password=appian --user=appian --port=3306 --protocol=tcp'
alias nano='nano -F'
alias rm_merge_files='rm -f `find | grep ".*\.\(REMOTE\|LOCAL\|BASE\).*\.java"`'
alias search='find -iregex'
alias search-history='history | grep -v history | grep '
alias top_ten='history | sed '\''s/^ \+//;s/  / /'\'' | cut -d'\'' '\'' -f2- | awk '\''{ count[$0]++ } END { for (i in count) print count[i], i }'\'' | sort -rn |head -10'
alias vdir='ls --color=auto --format=long'
alias webserver='python -m SimpleHTTPServer'
alias wget_appian='wget_page http://localhost:8080/ae/tempo'
alias wget_page='wget -q -O -'
alias appian-openvpn='sudo openvpn --script-security 2 --up /etc/openvpn/openvpn-dns --down /etc/openvpn/openvpn-dns --config ~/.openvpn/client.ovpn'
alias copy-to-clip="perl -pe 'chomp if eof' | xclip -sel clip"
alias less-lineno='less -N'


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
