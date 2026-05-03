#!/usr/bin/env bash
# imboy_ctl smoke: verify all CLI commands return exit 0.
# Usage:   ./ctl_smoke.sh
# Exit 0 = ALL PASS, non-zero = FAIL.

set -u
set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CTL="${SCRIPT_DIR}/../imboy_ctl"

PASS=0
FAIL=0
TOTAL=0

run_test() {
    local name="$1"
    local expect_rc="$2"
    shift 2
    TOTAL=$((TOTAL + 1))
    local output
    output=$("$CTL" "$@" 2>&1)
    local rc=$?
    if [ "$rc" -eq "$expect_rc" ]; then
        echo "  PASS: $name"
        PASS=$((PASS + 1))
    else
        echo "  FAIL: $name (rc=$rc expected=$expect_rc)"
        echo "        $output" | head -3
        FAIL=$((FAIL + 1))
    fi
}

echo "=== imboy_ctl smoke ==="
echo "ctl=${CTL}"
echo

# User commands
run_test "user token"      0 user token 1000000051
run_test "user detail"     0 user detail 1000000051
run_test "user list"       0 user list

# Msg commands — send first, capture MSG_ID for status check
SEND_OUTPUT=$("$CTL" msg send 1000000051 1000000056 2>&1) || true
MSG_ID=$(echo "$SEND_OUTPUT" | awk -F= '/^MSG_ID=/ {print $2}' | tr -d '[:space:]')
run_test "msg send"        0 msg send 1000000051 1000000056
run_test "msg status"      0 msg status "${MSG_ID:-ctl_0}"
run_test "msg archive"     0 msg archive -k "c2c:1000000051:1000000056" -l 5

# Node commands
run_test "node status"     0 node status
run_test "node routes"     0 node routes
run_test "node connections" 0 node connections
run_test "node pools"      0 node pools

# DB commands
run_test "db ping"         0 db ping
run_test "db migrate"      0 db migrate
run_test "db pool"         0 db pool

# Diagnose commands
run_test "diagnose config"    0 diagnose config
run_test "diagnose boundaries" 0 diagnose boundaries

# Smoke commands (smoke ws exits 2 = not implemented, that is expected)
run_test "smoke c2c"       0 smoke c2c
run_test "smoke ws"        2 smoke ws
run_test "smoke all"       0 smoke all

# Plugin commands
run_test "plugin list"     0 plugin list

echo
echo "=== Results: ${PASS}/${TOTAL} passed, ${FAIL} failed ==="

[ "$FAIL" -eq 0 ]
