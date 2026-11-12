#!/usr/bin/env bash

output_file="$1"

rest=$(cat "${output_file}" | tail '--lines=+4')

cat << EOF >"${output_file}"
*** Enter description below (required):
${USER} notes update for $(date).

EOF

if [ -z "${SKIP_SUBMIT+x}" ]; then
  cat << EOF >>"${output_file}"
APPROVED=marcelvaldez
REQUIRED_REVIEW=0
AUTOSUBMIT_BEHAVIOR=SYNC_SUBMIT

EOF
fi

if [ -n "${NO_G3MARK_OK:-}" ]; then
  cat << EOF >>"${output_file}"
NO_G3MARK_OK=${NO_G3MARK_OK}

EOF
fi

echo "${rest}" >> "${output_file}"
