#!/usr/bin/env bash
set -euo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
RUN_ID=${REST_RUN_ID:-"$(date -u +%Y%m%dT%H%M%SZ)-$$"}
DB_NAME=${REST_TEST_DB:-"imboy_rest_${RUN_ID//[^a-zA-Z0-9_]/_}"}
REPORT_ROOT=${REST_REPORT_ROOT:-"$ROOT/.reports/rest/$RUN_ID"}
PG_HOST=${REST_PG_HOST:-${IMBOY_PG_HOST:-127.0.0.1}}
PG_PORT=${REST_PG_PORT:-${IMBOY_PG_PORT:-4323}}
PG_USER=${REST_PG_USER:-${IMBOY_PG_USERNAME:-${IMBOY_PG_USER:-imboy_user}}}
PG_PASSWORD=${REST_PG_PASSWORD:-${IMBOY_PG_PASSWORD:-abc54321}}
CT_CONFIG=${REST_CT_CONFIG:-config/sys.local.config}

if [[ ! "$DB_NAME" =~ ^imboy_rest_[a-zA-Z0-9_]+$ ]]; then
  echo "Refusing unsafe scratch database name: $DB_NAME" >&2
  exit 2
fi
if [[ ! -f "$ROOT/$CT_CONFIG" ]]; then
  CT_CONFIG=config/sys.config.example
fi

export PGPASSWORD="$PG_PASSWORD"
export IMBOYENV=test
export IMBOY_PG_HOST="$PG_HOST"
export IMBOY_PG_PORT="$PG_PORT"
export IMBOY_PG_USERNAME="$PG_USER"
export IMBOY_PG_PASSWORD="$PG_PASSWORD"
export IMBOY_PG_DATABASE="$DB_NAME"
export HTTP_PORT=0
export TEST_HTTP_PORT=0
export REST_COMMIT_SHA
REST_COMMIT_SHA=$(git -C "$ROOT" rev-parse HEAD)
export REST_EVIDENCE_DIR="$REPORT_ROOT/evidence"

cleanup() {
  dropdb --if-exists --force --host "$PG_HOST" --port "$PG_PORT" --username "$PG_USER" "$DB_NAME" >/dev/null 2>&1 || true
}
trap cleanup EXIT INT TERM

mkdir -p "$REST_EVIDENCE_DIR" "$REPORT_ROOT/ct"
pg_isready --host "$PG_HOST" --port "$PG_PORT" --username "$PG_USER" >/dev/null
cleanup
createdb --host "$PG_HOST" --port "$PG_PORT" --username "$PG_USER" "$DB_NAME"

echo "REST run: $RUN_ID"
echo "Scratch DB: $DB_NAME"
echo "Evidence: $REPORT_ROOT"

make -C "$ROOT" rest-contract-check
make -C "$ROOT" ct-api_v1_login \
  CT_CONFIG="$CT_CONFIG" \
  TEST_HTTP_PORT=0 \
  CT_LOGS_DIR="$REPORT_ROOT/ct"
