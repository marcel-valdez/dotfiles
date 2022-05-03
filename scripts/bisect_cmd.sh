#!/usr/bin/env bash

function usage() {
  cat <<EOF
bisect_cmd.sh [--retries|-r <number>] [--cl <CL>] [--success_regex|-r <regex>]
              [--manual_ok|-m] [--success_status|-s <code> ...] [--help|-h] [--]
              CMD ARG1 ARG2 ... ARGN
n
retries: Number of failed retries before declaring failure. (optional)
cl: CL number to run the command against. (optional)
success_regex: Regular expression to run against the stdout & stderr output of
               the command. (optional)
manual_ok: Prompts the user to determine whether the command executed
           succesfully, instead of using an automated mechanism.
           Note that if success_regex and/or success_status are specified, then
           first those are used to automatically determine if it was a success,
           and if neither find success, then the user is prompted for a
           determination.
success_status: The valid success statuses for the command, if the exit code of
                the command matches any of these success statuses, then it is
                considered a successful execution. (optional, repeatable)

CMD ARG1 ARG2... ARGN: The command to execute.
EOF
}

CL=
MANUAL_OK=
CMD=()
ATTEMPTS=1
GOOD_EXIT_STATUS=()
GOOD_EXIT_STATUS_SPECIFIED=
SUCCESS_REGX=
CL=
function parse_args() {
  local cmd_found=
  while [ $# -gt 0 ]; do
    local arg="$1"
    case "${arg}" in
      --retries|-r)
        ATTEMPTS=$((ATTEMPTS + "$2"))
        shift
        ;;
      --cl)
        CL="$2"
        shift
        ;;
      --manual_ok|-m)
        MANUAL_OK=1
        ;;
      --success_regex|-r)
        # Let the user specify a regx on the result to determine success
        SUCCESS_REGX="$2"
        shift
        ;;
      --success_status|-s)
        # Let the user specify a successful exit status
        GOOD_EXIT_STATUS+=("$2")
        GOOD_EXIT_STATUS_SPECIFIED=1
        shift
        ;;
      --help|-h)
        usage
        exit 1
        ;;
      --)
        shift
        break
        ;;
      *)
        if [[ -z "${cmd_found}" ]]; then
          # Allow to execute specifying only a single command
          break
        else
          echo "Unknown parameter: ${arg}. Use -- to separate arguments from the command" >&2
          exit 1
        fi
        ;;
    esac
    cmd_found=1
    shift
  done

  if [[ "${#GOOD_EXIT_STATUS[@]}" -eq 0 ]] && ! [[ "${MANUAL_OK}" ]]; then
    GOOD_EXIT_STATUS+=(0)
  fi

  CMD=("$@")
  if [[ ${#CMD[@]} -eq 0 ]]; then
    echo "There is no command specified" >&2
    exit 1
  fi

  cat<<EOF
ATTEMPTS=${ATTEMPTS}
CL=${CL}
SUCCESS_REGEX=${SUCCESS_REGX}
MANUAL_OK=${MANUAL_OK}
GOOD_EXIT_STATUS=${GOOD_EXIT_STATUS[@]}
CMD=${CMD[@]}
EOF
}


function attempt() {
  "${CMD[@]}"
}

function run_test() {
  local output=
  local exit_status=
  local retries_left=${ATTEMPTS}
  while [[ ${retries_left} -gt 0 ]]; do
    echo "Attempt #$((ATTEMPTS-retries_left+1))" >&2
    output=$(attempt 2>&1)
    exit_status=$?
    # If a success regex was specified
    if [[ "${SUCCESS_REGX}" ]]; then
      if echo "${output}" | grep -P "${SUCCESS_REGX}" &>/dev/null; then
        echo "Success regex matched output." >&2
        return 0
      fi
    fi

    # If this is not a manual OK run, or a valid exit status was specified.
    if [[ -z "${MANUAL_OK}" ]] || [[ "${GOOD_EXIT_STATUS_SPECIFIED}" ]]; then
      for good_status in "${GOOD_EXIT_STATUS[@]}"; do
        if [[ ${good_status} -eq ${exit_status} ]]; then
          echo "Got good exit status ${good_status} from the command." >&2
          return 0
        fi
      done
    fi

    if [[ "${MANUAL_OK}" ]]; then
      echo "${output}"
      local question_msg="Was the run successful? (y/n): "
      if [[ "${CL}" ]]; then
        question_msg="Was CL ${CL} successful? (y/n): "
      fi
      read -p "${question_msg}" ok < /dev/tty;
      if [[ "${ok}" == "y" ]] || [[ "${ok}" == "Y" ]]; then
        return 0
      fi
    fi

    local result_msg="Command Failed."
    if [[ "${CL}" ]]; then
      result_msg="CL ${CL} failed."
    fi
    echo "${results_msg}" >&2
    retries_left=$((retries_left - 1))
  done

  return 1
}

function main() {
  run_test
  exit $?
}

parse_args "$@"
main

