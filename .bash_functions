#!/bin/bash

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

git-diff-lines() {
  git diff $1 $2 $3 $4 | diff-lines
}

ae() {
  cd ~/repositories/ae$1
}

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
