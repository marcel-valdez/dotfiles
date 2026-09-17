#!/usr/bin/env bash
# ==============================================================================
# Unit Test Suite: Pre-Connection Credential Gate for ssh_keep_alive.sh
# ==============================================================================
# Verifies requirements from gmosh_auto_reconnect_gcert_plan.md:
# 1. Syntax integrity of ssh_keep_alive.sh
# 2. Immediate pass-through when gcertstatus is valid (exit 0)
# 3. Background wait loop when gcertstatus is expired (exit 1)
# 4. Auto-reconnection when credentials become valid in the background
# 5. Interactive 'g' keypress triggers gcert invocation
# 6. Escape sequence noise immunity (Kitty focus events ^[[I / ^[[O)
# 7. Non-Google pass-through when gcertstatus is not in PATH
# ==============================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TARGET_SCRIPT="${TARGET_SCRIPT:-${SCRIPT_DIR}/../ssh_keep_alive.sh}"

PASS_COUNT=0
FAIL_COUNT=0

assert_eq() {
  local label="$1"
  local expected="$2"
  local actual="$3"
  if [[ "$expected" == "$actual" ]]; then
    echo "  [PASS] $label"
    PASS_COUNT=$((PASS_COUNT + 1))
  else
    echo "  [FAIL] $label (Expected: '$expected', Got: '$actual')"
    FAIL_COUNT=$((FAIL_COUNT + 1))
  fi
}

assert_true() {
  local label="$1"
  local condition="$2"
  if eval "$condition"; then
    echo "  [PASS] $label"
    PASS_COUNT=$((PASS_COUNT + 1))
  else
    echo "  [FAIL] $label (Condition: '$condition')"
    FAIL_COUNT=$((FAIL_COUNT + 1))
  fi
}

# Temporary test workspace
TEST_TMP="$(mktemp -d "/tmp/test_ssh_keep_alive_XXXXXX")"
trap 'rm -rf "${TEST_TMP}"' EXIT

MOCK_BIN="${TEST_TMP}/bin"
mkdir -p "${MOCK_BIN}"

# Mock log_lib.sh if needed
MOCK_LIB="${TEST_TMP}/lib"
mkdir -p "${MOCK_LIB}"
cat << 'EOF' > "${MOCK_LIB}/log_lib.sh"
log::info() { echo "[INFO] $*"; }
log::warn() { echo "[WARN] $*"; }
log::error() { echo "[ERROR] $*" >&2; }
log::fatal() { echo "[FATAL] $*" >&2; exit 128; }
EOF

export TEST_MOCK_LOG=""

echo "--- Test 1: Syntax Integrity ---"
assert_true "Syntax check on ssh_keep_alive.sh" "bash -n '${TARGET_SCRIPT}'"

echo "--- Test 2: Immediate Execution when Credentials Valid ---"
{
  cat << 'EOF' > "${MOCK_BIN}/gcertstatus"
#!/usr/bin/env bash
exit 0
EOF
  chmod +x "${MOCK_BIN}/gcertstatus"

  cat << 'EOF' > "${MOCK_BIN}/gmosh"
#!/usr/bin/env bash
echo "GMOSH_INVOKED:$*" >> "${TEST_MOCK_LOG}"
# Exit with 0 so the keepalive loop terminates cleanly (UNRETRIABLE_SUCCESS)
exit 0
EOF
  chmod +x "${MOCK_BIN}/gmosh"

  TEST_MOCK_LOG="${TEST_TMP}/test2_gmosh.log"
  rm -f "${TEST_MOCK_LOG}"

  output=$(HOME="${TEST_TMP}" PATH="${MOCK_BIN}:${PATH}" \
    timeout 5s bash "${TARGET_SCRIPT}" --gmosh gcloud "tmux attach -t default" 2>&1 || true)

  assert_true "gmosh was invoked immediately" "grep -q 'GMOSH_INVOKED' '${TEST_MOCK_LOG}'"
  assert_true "No waiting message emitted when credentials valid" "! echo '${output}' | grep -q 'Credentials expired'"
}

echo "--- Test 3: Auto-Reconnection when Credentials Renew in Background ---"
{
  # gcertstatus initially fails, but succeeds after STATE_FILE is created
  STATE_FILE="${TEST_TMP}/gcert_state"
  rm -f "${STATE_FILE}"

  cat << EOF > "${MOCK_BIN}/gcertstatus"
#!/usr/bin/env bash
if [[ -f "${STATE_FILE}" ]]; then
  exit 0
fi
exit 1
EOF
  chmod +x "${MOCK_BIN}/gcertstatus"

  TEST_MOCK_LOG="${TEST_TMP}/test3_gmosh.log"
  rm -f "${TEST_MOCK_LOG}"

  # Background process that simulates renewing gcert after 0.5 seconds
  (
    sleep 0.5
    touch "${STATE_FILE}"
  ) &
  BG_PID=$!

  output=$(HOME="${TEST_TMP}" PATH="${MOCK_BIN}:${PATH}" GCERT_POLL_INTERVAL=0.2 \
    timeout 5s bash "${TARGET_SCRIPT}" --gmosh gcloud "tmux attach -t default" </dev/null 2>&1 || true)

  wait "${BG_PID}" 2>/dev/null || true

  assert_true "Detected expired credentials and printed waiting notice" \
    "echo '${output}' | grep -q 'Waiting for gcert renewal'"
  assert_true "Detected renewed credentials and printed resume notice" \
    "echo '${output}' | grep -q 'Credentials valid. Resuming connection'"
  assert_true "gmosh was successfully invoked after renewal" \
    "grep -q 'GMOSH_INVOKED' '${TEST_MOCK_LOG}'"
}

echo "--- Test 4: Interactive 'g' Keypress Triggers gcert ---"
{
  # gcertstatus fails until GCERT_CALLED is created by gcert mock
  GCERT_CALLED_FILE="${TEST_TMP}/gcert_called"
  rm -f "${GCERT_CALLED_FILE}"

  cat << EOF > "${MOCK_BIN}/gcertstatus"
#!/usr/bin/env bash
if [[ -f "${GCERT_CALLED_FILE}" ]]; then
  exit 0
fi
exit 1
EOF
  chmod +x "${MOCK_BIN}/gcertstatus"

  cat << EOF > "${MOCK_BIN}/gcert"
#!/usr/bin/env bash
echo "GCERT_INVOKED" >> "${GCERT_CALLED_FILE}"
exit 0
EOF
  chmod +x "${MOCK_BIN}/gcert"

  TEST_MOCK_LOG="${TEST_TMP}/test4_gmosh.log"
  rm -f "${TEST_MOCK_LOG}"

  # Send 'g' on stdin after 0.2s using a subshell fifo/pipe
  FIFO="${TEST_TMP}/test4_fifo"
  rm -f "${FIFO}"
  mkfifo "${FIFO}"

  (
    sleep 0.2
    printf "g" > "${FIFO}"
  ) &
  INPUT_PID=$!

  output=$(HOME="${TEST_TMP}" PATH="${MOCK_BIN}:${PATH}" GCERT_POLL_INTERVAL=0.5 \
    timeout 5s bash "${TARGET_SCRIPT}" --gmosh gcloud "tmux attach -t default" < "${FIFO}" 2>&1 || true)

  wait "${INPUT_PID}" 2>/dev/null || true
  rm -f "${FIFO}"

  assert_true "gcert was called via interactive keypress" \
    "[[ -f '${GCERT_CALLED_FILE}' ]]"
  assert_true "gmosh was invoked following interactive gcert" \
    "grep -q 'GMOSH_INVOKED' '${TEST_MOCK_LOG}'"
}

echo "--- Test 5: Kitty Focus Escape Sequences Do Not Trigger gcert ---"
{
  GCERT_CALLED_FILE="${TEST_TMP}/test5_gcert_called"
  STATE_FILE="${TEST_TMP}/test5_state"
  rm -f "${GCERT_CALLED_FILE}" "${STATE_FILE}"

  cat << EOF > "${MOCK_BIN}/gcertstatus"
#!/usr/bin/env bash
if [[ -f "${STATE_FILE}" ]]; then
  exit 0
fi
exit 1
EOF
  chmod +x "${MOCK_BIN}/gcertstatus"

  cat << EOF > "${MOCK_BIN}/gcert"
#!/usr/bin/env bash
echo "GCERT_CALLED" > "${GCERT_CALLED_FILE}"
exit 0
EOF
  chmod +x "${MOCK_BIN}/gcert"

  FIFO="${TEST_TMP}/test5_fifo"
  rm -f "${FIFO}"
  mkfifo "${FIFO}"

  # Send focus-in sequence ^[[I followed by state file touch
  (
    sleep 0.2
    printf '\e[I' > "${FIFO}"
    sleep 0.5
    touch "${STATE_FILE}"
  ) &
  INPUT_PID=$!

  output=$(HOME="${TEST_TMP}" PATH="${MOCK_BIN}:${PATH}" GCERT_POLL_INTERVAL=0.2 \
    timeout 5s bash "${TARGET_SCRIPT}" --gmosh gcloud "tmux attach -t default" < "${FIFO}" 2>&1 || true)

  wait "${INPUT_PID}" 2>/dev/null || true
  rm -f "${FIFO}"

  assert_true "gcert was NOT triggered by focus escape sequence" \
    "[[ ! -f '${GCERT_CALLED_FILE}' ]]"
  assert_true "Loop unblocked once STATE_FILE was touched" \
    "echo '${output}' | grep -q 'Credentials valid. Resuming connection'"
}

echo "--- Test 6: Non-Google Pass-Through (no gcertstatus in PATH) ---"
{
  # Create an isolated PATH containing system utilities and mock gmosh,
  # but explicitly WITHOUT gcertstatus or gcert.
  NOGCERT_BIN="${TEST_TMP}/nogcert_bin"
  mkdir -p "${NOGCERT_BIN}"
  cp "${MOCK_BIN}/gmosh" "${NOGCERT_BIN}/gmosh"

  # Populate symlinks to system binaries, skipping any gcert* tools
  for dir in /bin /usr/bin; do
    if [[ -d "${dir}" ]]; then
      for bin_path in "${dir}"/*; do
        bin_name="$(basename "${bin_path}")"
        if [[ "${bin_name}" != gcert* && ! -e "${NOGCERT_BIN}/${bin_name}" ]]; then
          ln -s "${bin_path}" "${NOGCERT_BIN}/${bin_name}" 2>/dev/null || true
        fi
      done
    fi
  done

  TEST_MOCK_LOG="${TEST_TMP}/test6_gmosh.log"
  rm -f "${TEST_MOCK_LOG}"

  output=$(HOME="${TEST_TMP}" PATH="${NOGCERT_BIN}" \
    timeout 5s bash "${TARGET_SCRIPT}" --gmosh gcloud "tmux attach -t default" 2>&1 || true)

  assert_true "gmosh invoked directly without gcertstatus error" \
    "grep -q 'GMOSH_INVOKED' '${TEST_MOCK_LOG}'"
}

echo "=============================================================================="
echo "TEST SUMMARY: ${PASS_COUNT} Passed, ${FAIL_COUNT} Failed"
echo "=============================================================================="

if [[ ${FAIL_COUNT} -gt 0 ]]; then
  exit 1
fi
exit 0
