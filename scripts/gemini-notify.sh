#!/usr/bin/env bash

# Read the incoming JSON context from Gemini CLI
INPUT=$(cat)

# Extract tool name and workspace from the Notification payload
PARSED_DATA=$(echo "$INPUT" | python3 -c "
import sys, json, os
data = json.load(sys.stdin)
# Ensure this is a ToolPermission event
if data.get('notification_type') != 'ToolPermission':
    sys.exit(0)
tool = data.get('details', {}).get('toolName', 'a tool')
cwd = data.get('cwd', '')
workspace = os.path.basename(cwd)
if '/google3' in cwd:
    workspace = cwd.split('/google3')[0].split('/')[-1]
print(f'{tool}|{workspace}')
")

# If PARSED_DATA is empty, exit gracefully
if [ -z "$PARSED_DATA" ]; then
  echo "{}"
  exit 0
fi

IFS='|' read -r TOOL_NAME WORKSPACE <<< "$PARSED_DATA"

# Trigger Knock with the custom message
# Ensure knock.sh is sourced in your shell or the environment where this script runs.

if [[ -e /google/bin/releases/knock/knock.sh ]]; then
  # If we don't have access to knock due to missing gcert, skip.
  source /google/bin/releases/knock/knock.sh
  knock "[Cider Workspace $WORKSPACE] Gemini CLI needs approval for '$TOOL_NAME'"
fi

# Return an empty JSON object so Gemini CLI can proceed
echo "{}"
