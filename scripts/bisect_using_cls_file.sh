#!/usr/bin/env bash

cls_file="$1"
shift
end=$(wc -l "${cls_file}" | cut -d' ' -f1)
test_command="$@"
[[ -z "${start}" ]] && start=1
[[ -z "${retries}" ]] && retries=0
[[ -z "${repeats}" ]] && repeats=0
[[ -z "${repeats_in_parallel}" ]] && repeats_in_parallel=1

cat<<EOF
Options:
cls_file=${cls_file}
start=${start}
end=${end}
retries=${retries}
repeats=${repeats}
repeats_in_parallel=${repeats_in_parallel}
EOF

function get_cl {
  local line="$1"
  cat "${cls_file}" | head "-${line}" | tail -1
}

function run_test_async {
  local pids=()
  echo "Executing $((repeats+1)) commands in parallel, all of them need to pass for the run to be considered successful." >&2
  # Run commands in parallel
  for ((i=0; i<=repeats; i++)); do
    "$@" &
    pids+=($!)
  done

  # Wait for all commands and capture exit codes
  local failure_count=0
  for pid in "${pids[@]}"; do
    if ! wait "${pid}"; then
      failure_count=$((failure_count+1))
      echo "Run with PID ${pid} failed." >&2
    fi
  done

  echo "There were ${failure_count} failures out of $((repeats+1)) attempts." >&2

  return ${failure_count}
}

function run_test_sync {
  local attempt_no=0
  while [[ ${attempt_no} -le ${repeats} ]]; do
    if [[ ${repeats} -gt 0 ]]; then
      echo "Running trial $((attempt_no+1)) out of $((repeats+1))"
    fi
    echo "$@"
    if ! "$@"; then
      result=1
      echo "Trial $((attempt_no+1)) out of $((repeats+1)) FAILED" >&2
      echo "A single failure reports the test as a failure." >&2
      break
    fi
    attempt_no=$((attempt_no+1))
  done
}

function run_test {
  local cl=
  cl=$(get_cl "$1") || return 1
  echo "Testing CL: ${cl}"
  shift
  cd "/google/src/files/${cl}/depot/google3" || return 1
  pwd
  local result=0
  local attempt_no=0
  while [[ ${attempt_no} -le ${retries} ]]; do
    if [[ ${retries} -gt 0 ]]; then
      echo "Test attempt #$((attempt_no+1)) out $((retries+1))."
    fi
    if ! [[ "${repeats_in_parallel}" ]]; then
      run_test_sync "$@"
      result=$?
    else
      run_test_async "$@"
      result=$?
    fi

    attempt_no=$((attempt_no+1))
    if [[ ${result} -eq 0 ]]; then
      return ${result}
    fi
  done

  return ${result}
}

function main {
  export -f run_test
  export -f get_cl
  export -f run_test_async
  export -f run_test_sync
  export cls_file
  export test_command
  export repeats
  export retries
  /google/data/ro/teams/tetralight/bin/bisect -low="${start}" -high="${end}" "run_test \$X ${test_command[@]}"
}

main
