#!/usr/bin/env bash

set -o errexit  # Exit if a command fails
set -o nounset  # Exit if we use an undeclared variable
set -o pipefail  # Use the exit status of the last command taht threw a non-zero exit code
# set -o xtrace  # Trace what gets executed, useful for debugging.

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
  ls "${INPUT_DIRECTORY}" | grep -P '(METADATA|\.(md|org|png|jpg|jpeg|gif|svg))$'
}

function get_included_content_files {
  local _filter="${EXCLUDED_FILES[*]}"
  for _file in $(get_all_content_files); do
    if ! [[ " ${_filter} "  =~ " ${_file} " ]]; then
      echo "${_file}"
    fi
  done
}

function copy_to_staging_directory {
  if [[ -d "${STAGING_DIRECTORY}" ]]; then
    rm -fr  "${STAGING_DIRECTORY}/"*
  fi
  mkdir -p "${STAGING_DIRECTORY}"

  for _file in $(get_included_content_files); do
    cp "${INPUT_DIRECTORY}/${_file}" "${STAGING_DIRECTORY}/"
  done
}

function convert_staging_directory {
  if [[ -d "${PUBLISH_DIRECTORY}" ]]; then
    rm -fr "${PUBLISH_DIRECTORY}/"*
  fi
  mkdir -p "${PUBLISH_DIRECTORY}"

  for _file in $(ls "${STAGING_DIRECTORY}"); do
    local full_input_file="${STAGING_DIRECTORY}/${_file}"
    if [[ " ${_file} " =~ ".org " ]]; then
      local full_output_file="${PUBLISH_DIRECTORY}/${_file%.org}.md"
      printf "yes\\r\\nyes\\r\\nyes\\r\\nyes\\r\\nyes\\r\\n" | /usr/local/google/home/marcelvaldez/.local/bin/org2md -y "${full_input_file}"
      local full_converted_file="${STAGING_DIRECTORY}/${_file%.org}.md"
      cp "${full_converted_file}" "${full_output_file}"
      #pandoc "${full_input_file}" --output="${full_output_file}" \
      #  --from=org --to=markdown --standalone --toc --strip-comments \
      #  --tab-stop 4
    else
      local full_output_file="${PUBLISH_DIRECTORY}/${_file}"
      cp "${full_input_file}" "${full_output_file}"
    fi
  done
}

SKIP_SUBMIT=
function sync_publish_with_company {
  rsync --archive --verbose --human-readable --delete \
    "${PUBLISH_DIRECTORY}/" "${OUTPUT_DIRECTORY}/"
  pushd "${OUTPUT_DIRECTORY}"
  # NOTE: This does not handle recursive files copied over.
  export SKIP_SUBMIT
  EDITOR="${HOME}/scripts/gen_notes_cl_description.sh" g4 change
  g4 upload
  if [[ -z "${SKIP_SUBMIT}" ]]; then
    g4 submit
  fi
  popd
}

function main {
  if /usr/bin/gcertstatus -check_remaining=60s --quiet; then
    copy_to_staging_directory
    convert_staging_directory
    sync_publish_with_company
  else
    echo "WARNING: gcert is no longer valid, therefore we can't sync notes to company doc." >&2
  fi
}

main "$@"
