#!/bin/bash

now() {
  date "+%H:%M:%S"
}

diff-lines() {
  local path=
  local line=
  while read; do
    esc=$'\033'
    if [[ $REPLY =~ ---\ (a/)?.* ]]; then
      continue
    elif [[ $REPLY =~ \+\+\+\ (b/)?([^[:blank:]$esc]+).* ]]; then
      path=${BASH_REMATCH[2]}
    elif [[ $REPLY =~ @@\ -[0-9]+(,[0-9]+)?\ \+([0-9]+)(,[0-9]+)?\ @@.* ]]; then
      line=${BASH_REMATCH[2]}
    elif [[ $REPLY =~ ^($esc\[[0-9;]+m)*([\ +-]) ]]; then
      echo "$path:$line:$REPLY"
      if [[ ${BASH_REMATCH[2]} != - ]]; then
        ((line++))
      fi
    fi
  done
}

# start git functions

git-diff-lines() {
  git diff $1 $2 $3 $4 | diff-lines
}

g-diff-lines() {
  git-diff-lines $1 $2 $3 $4
}

git-branch-out() {
  new_branch_name=$1
  treeish=$2
  git checkout -b $1 $2
}

g-branch-out() {
  git-branch-out $1 $2
}

# end git functions

# start npm functions

node-check-use() {
  node_version=$(node --version 2>/dev/null)
  if [ "$node_version" != "v$NODE_VERSION" ]; then
    node_version_installed=$(nvm ls 2>/dev/null | grep $NODE_VERSION)
    if [ "$node_version_installed" == "" ]; then
      echo Node v$NODE_VERSION is not installed, installing now.
      nvm install "v$NODE_VERSION"
    fi

    if [ "$1" == "--silent" ]; then
      nvm use $NODE_VERSION >/dev/null 2>&1
    else
      nvm use $NODE_VERSION
    fi
  fi
}

npm-tdd() {
  node-check-use
  npm run test:watch
}

npm-install() {
  node-check-use
  npm install
}

npm-test() {
  node-check-use
  npm run test
}

npm-start() {
  node-check-use
  npm start
}

npm-lint() {
  node-check-use
  npm run lint
}

# end npm functions

mlocate-here() {
  directory="$2"
  if [ "$directory" == "" ]; then
    directory="$(pwd)/"
  fi
  mlocate -b "$1" | grep --color=never "^$directory"
}

run-step() {
  $1
  __result=$?
  time=$(date +%H:%M)
  if [ "$__result" == "0" ]; then
    lemonbar-show --fg "#99FF99" "($time) <$1> done ($2)"
  else
    lemonbar-show --fg "#C45454" "($time) <$1> failed ($2)"
  fi
}

npm-run-all() {
  time=$(date +%H:%M)
  lemonbar-show --fg "#FFFF00" "($time) Running npm-run-all"

  run-step "npm install" "1/5"
  if [ "$__result" == "1" ]; then
    exit 1
  fi

  run-step "npm run test" "2/5"
  run-step "npm run test-tools" "3/5"
  run-step "npm run lint" "4/5"
  run-step "npm run validateLicenses" "5/5"
}

function top-n() {
  history \
  | sed 's/^ \+//;s/  / /' \
  | cut -d']' -f2- \
  | awk '{ count[$0]++ } END { for (i in count) print count[i], i }' \
  | sort -rn \
  | head -$1
}

function top-ten() {
  top-n 10
}

function java-package-to-path() {
  package="$1"
  if [ "$package" == "" ]; then
    package=$(cat)
  fi

  echo "$package" | sed 's/\./\//g'
}

function copy-to-clip() {
  get-arg-or-stdin $* | perl -pe 'chomp if eof' | xclip -sel clip
}

function paste-clip() {
  xclip -o
}

function get-arg-or-stdin() {
  # if there is less than one argument, then echo from stdin, otherwise echo all arguments
  ( [ $# -lt 1 ] && echo $(cat)) || echo "$*"
}

function tmux-to-clip() {
  tmux show-buffer | copy-to-clip
}

function copy-to-tmux() {
  tmux set-buffer "$(get-arg-or-stdin $*)"
}

function history-cmd-only() {
  history | sed 's/^[^]]*\]//'
}

function cmd-exists() {
  ( type "$1" >/dev/null 2>&1 && echo "true" ) || echo "false"
}

function tmux-session-name() {
  tmux display-message -p '#S'
}

function tmux-goto-session-client() {
  g4d $(tmux-session-name)
}
