#!/usr/bin/env bash

# Script to wait for the latest run of a Guitar workflow for the current user
# and report its status.

if [[ $# -ne 1 ]]; then
  echo "Usage: $0 <workflow_blaze_target>"
  echo "Example: $0 //path/to:my_guitar_workflow"
  exit 1
fi

WORKFLOW_TARGET="$1"
CURRENT_USER="${USER}"
SEARCH_SERVICE="blade:guitar.search.guitarsearchservice-prod"
RESULT_SERVICE="blade:guitar-result-service"
POLL_INTERVAL_SECONDS=30
[[ -z "${MAX_WAIT_TIME_SECONDS}" ]] && MAX_WAIT_TIME_SECONDS=10800 # 3 hours

echo "Fetching most recent run for workflow: ${WORKFLOW_TARGET} triggered by user: ${CURRENT_USER}"

# Construct the filter for the Search service
GSS_FILTER="user:\"${CURRENT_USER}\" workflow_name:\"${WORKFLOW_TARGET}\""

# Call GuitarSearchService to find the most recent workflow execution ID for the user.
# Results are returned in reverse order of trigger time, so the first one is the latest.
GSS_RESPONSE=$(stubby call ${SEARCH_SERVICE} GuitarSearchService.SearchWorkflowExecutions \
  "filter: '${GSS_FILTER}' page_size: 1")

if [[ $? -ne 0 ]]; then
  echo "Error: Failed to call GuitarSearchService.SearchWorkflowExecutions"
  exit 1
fi

# Parse the response to get the workflow_id
WORKFLOW_ID=$(echo "${GSS_RESPONSE}" | gqui from textproto:- \
  proto guitar.search.SearchWorkflowExecutionsResponse \
  select results[0].workflow_execution_id format '%s' 2>/dev/null)

if [[ -z "${WORKFLOW_ID}" ]]; then
  echo "Error: Could not find any runs for workflow: ${WORKFLOW_TARGET} by user: ${CURRENT_USER}"
  exit 1
fi

echo "Found most recent execution ID: ${WORKFLOW_ID}"
FUSION_LINK="http://fusion2/${WORKFLOW_ID}"

echo "Waiting for workflow to complete... Polls every ${POLL_INTERVAL_SECONDS}s." >&2
echo "Fusion link: ${FUSION_LINK}"

WAIT_TIMEOUT_SECONDS=$((SECONDS + MAX_WAIT_TIME_SECONDS))

while true; do
  if [[ ${SECONDS} -gt ${WAIT_TIMEOUT_SECONDS} ]]; then
    echo "TIMEOUT: Workflow did not complete within ${MAX_WAIT_TIME_SECONDS} seconds."
    echo "FAILURE: ${FUSION_LINK}"
    exit 1
  fi

  EXECUTION_RESPONSE=$(stubby call ${RESULT_SERVICE} ResultService.GetWorkflowExecutions \
    'workflow_execution_ids: "'"${WORKFLOW_ID}"'"')

  if [[ $? -ne 0 ]]; then
    echo "Warning: Failed to call ResultService.GetWorkflowExecutions. Retrying in ${POLL_INTERVAL_SECONDS}s..." >&2
    sleep ${POLL_INTERVAL_SECONDS}
    continue
  fi

  # Extract the final_status from the last attempt.
  # gqui will return an empty string if final_status is not set.
  FINAL_STATUS=$(echo "${EXECUTION_RESPONSE}" | gqui from textproto:- \
    proto guitar.GetWorkflowExecutionsResponse \
    select workflow_executions[0].attempts[-1].final_status format '%s' 2>/dev/null)

  if [[ -n "${FINAL_STATUS}" ]]; then
    echo "Workflow finished with status: ${FINAL_STATUS}"
    if [[ "${FINAL_STATUS}" == "WORKFLOW_HEALTHY" ]]; then
      echo "SUCCESS: ${FUSION_LINK}"
      exit 0
    else
      echo "FAILURE: ${FUSION_LINK}"
      exit 1
    fi
  else
    # final_status is not yet set, so the workflow is still running.
    echo "Still waiting for ${WORKFLOW_ID} to complete..." >&2
    sleep ${POLL_INTERVAL_SECONDS}
  fi
done
