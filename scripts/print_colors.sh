#!/usr/bin/env bash

colors=$1
n=0
while [[ ${n} -lt ${colors} ]]; do
  n=$((n+1))
  printf " [%03d] $(tput setaf $n)%s$(tput sgr0)" ${n} "wMwMwMwMwMwMw"
  if [[ $((n%5)) -eq 0 ]]; then
    echo ""
  fi
done
