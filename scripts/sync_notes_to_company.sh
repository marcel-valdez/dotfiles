#!/usr/bin/env bash

set -o errexit  # Exit if a command fails
set -o nounset  # Exit if we use an undeclared variable
set -o pipefail  # Use the exit status of the last command taht threw a non-zero exit code
# set -o xtrace  # Trace what gets executed, useful for debugging.

LOG_FILE="/tmp/sync_notes_to_company.log"

function now {
  date "+%H:%M:%S"
}

function log {
  local msg=
  msg="$(now) $*"
  echo "${msg}" >> "${LOG_FILE}"
  echo "${msg}"
}

INPUT_DIRECTORY="${HOME}/notes"
STAGING_DIRECTORY="/tmp/${USER}_notes_staging"
PUBLISH_DIRECTORY="/tmp/${USER}_notes_publish"
OUTPUT_DIRECTORY="/google/src/cloud/marcelvaldez/personal_notes/company/users/marcelvaldez"

EXCLUDED_FILES=(
  playground.org
  TODO.org
  misc.org
  ali_one_on_one.md
  contacts.md
  peak_performance.md
)

function get_all_content_files {
  ls -a "${INPUT_DIRECTORY}" | grep -P '(METADATA|\.(md|org|png|jpg|jpeg|gif|svg|org_settings|settings))$'
}

function get_included_content_files {
  local _filter="${EXCLUDED_FILES[*]}"
  for _file in $(get_all_content_files); do
    if ! [[ -f "${INPUT_DIRECTORY}/${_file}" ]]; then
      continue  # malformed filename somewhere
    fi
    if ! [[ " ${_filter} "  =~ " ${_file} " ]]; then
      echo "${_file}"
    fi
  done
}

function copy_to_staging_directory {
  if [[ -d "${STAGING_DIRECTORY}" ]]; then
    rm -fr "${STAGING_DIRECTORY}"
  fi
  mkdir -p "${STAGING_DIRECTORY}"

  for _file in $(get_included_content_files); do
    cp "${INPUT_DIRECTORY}/${_file}" "${STAGING_DIRECTORY}/"
  done
}

# Files that are needed in staging, but shouldn't be published.
ONLY_STAGING_FILTER=".*\.(settings)$"
function convert_staging_directory {
  if [[ -d "${PUBLISH_DIRECTORY}" ]]; then
    rm -fr "${PUBLISH_DIRECTORY}"
  fi
  mkdir -p "${PUBLISH_DIRECTORY}"

  for _file in $(ls "${STAGING_DIRECTORY}"); do
    local full_input_file="${STAGING_DIRECTORY}/${_file}"
    if [[ " ${_file} " =~ ".org " ]]; then
      local full_output_file="${PUBLISH_DIRECTORY}/${_file%.org}.md"
      local md_converted_file="${STAGING_DIRECTORY}/${_file%.org}.md"
      pandoc "${full_input_file}" --output="${md_converted_file}" \
        --from=org --to=markdown_strict --standalone --toc --strip-comments \
        --toc-depth=2 --tab-stop 4
      cp "${md_converted_file}" "${full_output_file}"
    else
      if ! echo "${_file}" | grep -P "${ONLY_STAGING_FILTER}" &>/dev/null; then
        local full_output_file="${PUBLISH_DIRECTORY}/${_file}"
        cp "${full_input_file}" "${full_output_file}"
      fi
    fi
  done
}

SKIP_SUBMIT=
function sync_publish_with_company {
  pushd "${OUTPUT_DIRECTORY}"
  g4 sync
  # NOTE: We need the forward slash at the end to copy directory contents
  rsync --archive --verbose --human-readable --delete \
    "${PUBLISH_DIRECTORY}/" "${OUTPUT_DIRECTORY}/"
  # NOTE: This does not handle recursive files copied over.
  export SKIP_SUBMIT
  EDITOR="${HOME}/scripts/gen_notes_cl_description.sh" g4 change
  g4 fix
  g4 upload
  if [[ -z "${SKIP_SUBMIT}" ]]; then
    g4 submit
  fi
  popd
}

function main {
  if /usr/bin/gcertstatus -check_remaining=60s --quiet; then
    log "Found a valid gcert, proceeding to sync notes with company docs"
    copy_to_staging_directory
    convert_staging_directory
    sync_publish_with_company
  else
    log "WARNING: gcert is no longer valid, therefore we can't sync notes to company doc." >&2
  fi
}

main "$@"
