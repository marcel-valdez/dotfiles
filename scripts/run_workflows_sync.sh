#!/usr/bin/env bash
CURR_SCRIPT=${BASH_SOURCE[0]}

USE_KNOCK=
USE_KNOCKER=
if [[ -e /google/bin/releases/knock/knock.sh ]]; then
  source /google/bin/releases/knock/knock.sh --auto
  USE_KNOCK=1
  if [[ -e "${HOME}/.googlerc.d/.google_functions" ]]; then
    source "${HOME}/.googlerc.d/.google_functions"
    USE_KNOCKER=1
  fi
fi

function help {
  cat <<EOF
  ${CURR_SCRIPT} workflow1 [worfkflow2 [workflow 3 ...]] -- [blaze_test_arg1 [blaze_test_arg2 [blaze_test_arg3 ...]]]

  workflowN: The guitar workflows to run in sequence.
  blaze_test_argN: The blaze test arguments to provide for every run.
EOF

}

WORKFLOWS=()
BLAZE_TEST_ARGS=()

function parse_args {
  local separator_found=
  while [[ $# -gt 0 ]]; do
    local arg=$1
    case "${arg}" in
      --help)
        help
        exit 0
        ;;
      --)
        separator_found=1
        ;;
      *)
        if [[ -z "${separator_found}" ]]; then
          WORKFLOWS+=("$1")
        else
          # We add this by default.
          if ! [[ "$1" == "--guitar_detach" ]]; then
            BLAZE_TEST_ARGS+=("$1")
          fi
        fi
        ;;
    esac
    shift
  done

  if [[ ${#WORKFLOWS[@]} -eq 0 ]]; then
    echo "No workflows specified, nothing to run. Exiting." >&2
    help
    exit 1
  fi

  if [[ ${#WORKFLOWS[@]} -eq 1 ]]; then
    echo "WARNING: You specified a single workflow." >&2
  fi

  if [[ ${#BLAZE_TEST_ARGS[@]} -eq 0 ]]; then
    echo "WARNING: You didn't specify any blaze test args, normally you need to specify the guitar cluster at least." >&2
    echo "         The test may not run at all." >&2
  fi
}

function notify {
  echo "$@"
  if [[ "${USE_KNOCK}" ]]; then
    knock "$@"
  else
    "$@"
  fi
}

function knocker_ {
  if [[ "${USE_KNOCKER}" ]]; then
    knocker "$@"
  else
    "$@"
  fi
}

function main {
  local total_workflows=${#WORKFLOWS[@]}
  local workflow_count=1
  for workflow in "${WORKFLOWS[@]}"; do
    local progress="(${workflow_count}/${total_workflows})"
    echo "${progress} Running workflow: ${workflow}"
    blaze test "${workflow}" "${BLAZE_TEST_ARGS[@]}" --guitar_detach
    # Give guitar some time to propagate the workflow execution.
    sleep 3
    echo " ${progress} Waiting for workflow ${workflow} to finish."
    if knocker_ "${HOME}/scripts/wait_for_guitar_workflow.sh" "${workflow}"; then
      notify "${progress} Workflow ${workflow} finished successfully, running next workflow."
    else
      notify " ${progress} FAILURE: Guitar Workflow ${workflow} failed, stopping execution." >&2
      exit 1
    fi
    workflow_count=$((current_workflow_count+1))
  done

  notify "SUCCESS: Done running all ${total_workflows} Guitar workflows!"
}


parse_args "$@"
main
