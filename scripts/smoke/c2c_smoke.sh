#!/usr/bin/env bash
# C2C smoke: send one programmatic message, then verify msg_store row landed.
# Usage:   ./c2c_smoke.sh [FROM_UID] [TO_UID]
# Default: FROM=1000000051 (Alice) -> TO=1000000056 (Bob)
# Exit 0 = PASS, non-zero = FAIL.

set -u
set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ESCRIPT_FILE="${SCRIPT_DIR}/../imboy_ctl"

FROM="${1:-1000000051}"
TO="${2:-1000000056}"

# Local PG (from config/sys.local.config)
PGHOST="${PGHOST:-127.0.0.1}"
PGPORT="${PGPORT:-4323}"
PGUSER="${PGUSER:-imboy_user}"
PGDATABASE="${PGDATABASE:-imboy_v1}"
export PGPASSWORD="${PGPASSWORD:-abc54321}"

echo "=== C2C smoke ==="
echo "from=${FROM} to=${TO}"
echo "pg=${PGHOST}:${PGPORT}/${PGDATABASE} user=${PGUSER}"
echo

# 1) send via RPC
chmod +x "${ESCRIPT_FILE}"
SEND_OUT="$(${ESCRIPT_FILE} msg send "${FROM}" "${TO}" 2>&1)"
SEND_RC=$?
echo "--- escript output ---"
echo "${SEND_OUT}"
echo "--- escript rc=${SEND_RC} ---"

if [[ ${SEND_RC} -ne 0 ]]; then
    echo "FAIL: escript exited non-zero"
    exit 1
fi

MSG_ID="$(echo "${SEND_OUT}" | awk -F= '/^MSG_ID=/ {print $2}' | tr -d '[:space:]')"
if [[ -z "${MSG_ID}" ]]; then
    echo "FAIL: could not parse MSG_ID"
    exit 1
fi
echo "MSG_ID=${MSG_ID}"
echo

# 2) verify msg_store row (poll up to 10s; write is async)
SQL="SELECT chat_type, from_id, to_id, msg_type FROM msg_store WHERE msg_id = '${MSG_ID}' LIMIT 1;"
ROW=""
for i in $(seq 1 20); do
    ROW="$(psql -h "${PGHOST}" -p "${PGPORT}" -U "${PGUSER}" -d "${PGDATABASE}" -At -F '|' -c "${SQL}" 2>&1)"
    PSQL_RC=$?
    if [[ ${PSQL_RC} -ne 0 ]]; then
        echo "FAIL: psql error on attempt ${i}"
        echo "${ROW}"
        exit 2
    fi
    if [[ -n "${ROW}" ]]; then
        echo "msg_store row found on attempt ${i}"
        break
    fi
    sleep 0.5
done

if [[ -z "${ROW}" ]]; then
    echo "FAIL: msg_store has no row for msg_id=${MSG_ID} after 10s"
    exit 3
fi

CHAT_TYPE="$(echo "${ROW}" | cut -d'|' -f1)"
FROM_ID="$(echo "${ROW}" | cut -d'|' -f2)"
TO_ID="$(echo "${ROW}" | cut -d'|' -f3)"
MSG_TYPE="$(echo "${ROW}" | cut -d'|' -f4)"

echo "--- msg_store row ---"
echo "  chat_type = ${CHAT_TYPE}"
echo "  from_id   = ${FROM_ID}"
echo "  to_id     = ${TO_ID}"
echo "  msg_type  = ${MSG_TYPE}"
echo

if [[ "${CHAT_TYPE}" != "c2c" ]]; then
    echo "FAIL: expected chat_type=c2c, got ${CHAT_TYPE}"
    exit 4
fi
if [[ "${FROM_ID}" != "${FROM}" ]]; then
    echo "FAIL: expected from_id=${FROM}, got ${FROM_ID}"
    exit 5
fi
if [[ "${TO_ID}" != "${TO}" ]]; then
    echo "FAIL: expected to_id=${TO}, got ${TO_ID}"
    exit 6
fi
if [[ "${MSG_TYPE}" != "text" ]]; then
    echo "FAIL: expected msg_type=text, got ${MSG_TYPE}"
    exit 7
fi

echo "PASS: C2C smoke green (msg_id=${MSG_ID})"
exit 0
