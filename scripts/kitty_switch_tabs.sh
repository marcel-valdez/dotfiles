#!/usr/bin/env bash

for _dir in "${HOME}/bin" "${HOME}/.local/bin" "${HOME}/.fzf/bin" "/usr/local/bin" "/usr/local/go/bin" "/opt/homebrew/bin" "/Applications/kitty.app/Contents/MacOS"; do
  if [[ -d "${_dir}" && ":${PATH}:" != *":${_dir}:"* ]]; then
    export PATH="${_dir}:${PATH}"
  fi
done
unset _dir

tabs="$(kitten @ ls | jq -r '.[].tabs[] | "\(.id) \(.title)"')"
echo "${tabs}" | fzf '--with-nth=2..' --header='Switch to Kitty Tab' |\
  cut -d' ' -f1 |\
  xargs -I__id__ kitten @ focus-tab --match id:__id__
