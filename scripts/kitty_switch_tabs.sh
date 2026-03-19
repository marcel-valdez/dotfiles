#!/usr/bin/env bash

tabs="$(kitten @ ls | jq -r '.[].tabs[] | "\(.id) \(.title)"')"
echo "${tabs}" | fzf '--with-nth=2..' --header='Switch to Kitty Tab' |\
  cut -d' ' -f1 |\
  xargs -I__id__ kitten @ focus-tab --match id:__id__
