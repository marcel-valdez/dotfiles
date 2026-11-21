#!/usr/bin/env bash
# Wrapper forwarding legacy mdcat.sh invocations to mdview.

set -euo pipefail

extra_args=()
if [[ -n "${COLS:-}" ]]; then
  has_width=0
  for arg in "$@"; do
    case "${arg}" in
      -w|--columns|--columns=*)
        has_width=1
        ;;
    esac
  done
  if (( has_width == 0 )); then
    extra_args+=(--columns "${COLS}")
  fi
fi

exec "${HOME}/bin/mdview" ${extra_args[@]+"${extra_args[@]}"} "$@"
