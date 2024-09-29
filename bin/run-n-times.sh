#!/usr/bin/env bash

function usage () {
  cat <<EOF
run-n-times.sh [--sync|-s] [--no-join|-nj] [--help|-h] [--count|-n <amount>] -- CMD ARG1 ARG2 ... ARGN

--sync: Execute commands sequentially and synchronously. Default: false. Optional.
--no-join: Don't wait for all commands to finish executing. Default: false. Optional.
--count|-n: Number of times to execute the command. Required.
--help: Show this help message.
--exit-status|-e: The exist status code strategy to use. Default: all-success.
                  all-success: All need to succeed for the program to exit with status code 0.
                  any-success: Any command needs to succeed for the program to exit with status code 0.
CMD ARG1 ARG2 ... ARGN: This is the command to actually execute n times.
EOF
}

CMD=()
SYNC=
JOIN=1
REPEAT_COUNT=
EXIT_STRATEGY="all-success"
EXIT_STRATEGY_OPTIONS=("all-success" "any-success")
function parse_args () {
  while [[ $# -gt 0 ]]; do
    local arg=$1
    case "${arg}" in
      --sync|-s)
        SYNC=1
        ;;
      --no-join|-nj)
        JOIN=
        ;;
      --count|-n)
        REPEAT_COUNT="$2"
        shift
        ;;
      --exit-status|-e)
        EXIT_STRATEGY="$2"
        shift
        ;;
      --help|-h)
        usage
        exit 0
        ;;
      --)
        shift
        break
        ;;
      *)
        echo "Unknown parameter ${arg}, use -- to separate the command from arguments." >&2
        exit 1
        ;;
    esac
    shift
  done

  if ! [[ " ${EXIT_STRATEGY_OPTIONS[@]} " =~ " ${EXIT_STRATEGY} " ]]; then
    echo "Unknown exit status strategy: ${EXIT_STRATEGY}" >&2
    exit 1
  fi

  if [[ -z ${REPEAT_COUNT} ]]; then
    echo "--count|-n argument not specified but is required." >&2
    exit 1
  fi

  CMD=("$@")
  if [[ ${#CMD[@]} -eq 0 ]]; then
    echo "No value for the command was specified." >&2
    exit 1
  fi

  cat<<EOF
CMD=${CMD[@]}
JOIN=${JOIN}
SYNC=${SYNC}
REPEAT_COUNT=${REPEAT_COUNT}
EOF
}

function execute() {
  local pids=()
  local exit_statuses=()
  for i in $(seq "${REPEAT_COUNT}"); do
    if [[ "${SYNC}" ]]; then
      "${CMD[@]}"
      exit_statuses+=($?)
    else
      "${CMD[@]}" &
      pids+=($!)
    fi
  done

  for pid in ${pids[@]}; do
    wait ${pid}
    exit_statuses+=($?)
  done

  local any_failed=
  local any_succeeded=
  for exit_status in ${exit_statuses[@]}; do
    if [[ ${exit_status} -ne 0 ]]; then
      any_failed=1
    else
      any_succeeded=1
    fi
  done

  if [[ "${EXIT_STRATEGY}" == "all-success" ]]; then
    if [[ "${any_failed}" ]]; then
      return 1
    else
      return 0
    fi
  elif [[ "${EXIT_STRATEGY}" == "any-success" ]]; then
    if [[ "${any_succeeded}" ]]; then
      return 1
    else
      return 0
    fi
  else
    echo "Internal error: Unknown exit strategy: ${EXIT_STRATEGY}" >&2
    exit 1
  fi
}

function main() {
  execute
}

parse_args "$@"
main
