#!/usr/bin/env bash
# E2E-01：Agent Hub 本地 Golden Flow harness。
# 从空 scratch 环境复现：建库 → 扩展 → 全链迁移(1→93+) → 核心套件 → 证据 → 清理。
# 所有资源带 marker（库名前缀 imboy_ah_e2e_），清理只删除带 marker 的对象。
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
WORKSPACE_ROOT="$(cd "$ROOT/.." && pwd -P)"
PROFILE="local-fixture"
EVIDENCE_DIR="${IMBOY_EVIDENCE_ROOT:-${TMPDIR:-/tmp}/imboy-agent-hub}/E2E-01"
MARKER_DB_PREFIX="imboy_ah_e2e_"
CONFIG_TMP_DIR=""
CURRENT_STEP="initialization"
RUN_FINISHED=0
SUITES_PASSED=0
TRACE_EXIT=2
CLEANUP_PASSED=0
CLEANUP_DONE=0
SENSITIVE_SCAN_PASSED=0

while [[ $# -gt 0 ]]; do
  case "$1" in
    --profile) PROFILE="$2"; shift 2 ;;
    --profile=*) PROFILE="${1#*=}"; shift ;;
    --evidence-dir) EVIDENCE_DIR="$2"; shift 2 ;;
    --evidence-dir=*) EVIDENCE_DIR="${1#*=}"; shift ;;
    *) echo "unknown arg: $1" >&2; exit 2 ;;
  esac
done

EVIDENCE_DIR="$(python3 -c 'import os, sys; print(os.path.realpath(sys.argv[1]))' "$EVIDENCE_DIR")"
case "$EVIDENCE_DIR/" in
  "$WORKSPACE_ROOT/"*)
    echo "[golden] refusing evidence directory inside workspace" >&2
    exit 2
    ;;
esac

DB="${MARKER_DB_PREFIX}$(date +%s)_$$"
PGHOST="${PGHOST:-127.0.0.1}"
PGPORT="${PGPORT:-4323}"
PGUSER="${PGUSER:-imboy_user}"
export PGPASSWORD="${PGPASSWORD:-abc54321}"

case "$PGHOST" in
  127.0.0.1|::1) ;;
  *) echo "[golden] refusing non-loopback PostgreSQL host" >&2; exit 2 ;;
esac
if [[ -n "${PGHOSTADDR:-}" || -n "${PGSERVICE:-}" || -n "${PGSERVICEFILE:-}" ]]; then
  echo "[golden] refusing libpq connection target override" >&2
  exit 2
fi
unset PGHOSTADDR PGSERVICE PGSERVICEFILE

PSQL="psql -h $PGHOST -p $PGPORT -U $PGUSER -v ON_ERROR_STOP=1 -q"
BASE_IMBOY="$(git -C "$ROOT" rev-parse HEAD)"
BASE_IMBOYAPP="$(git -C "$WORKSPACE_ROOT/imboyapp" rev-parse HEAD)"
BASE_IMBOYADMIN="$(git -C "$WORKSPACE_ROOT/imboyadmin" rev-parse HEAD)"

cleanup() {
  local failed=0
  local remaining=""
  if [[ "$CLEANUP_DONE" -eq 1 && "$CLEANUP_PASSED" -eq 1 ]]; then
    return
  fi
  echo "[golden] cleanup: drop $DB"
  psql -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" -d postgres \
    -c "DROP DATABASE IF EXISTS $DB" > /dev/null 2>&1 || failed=1
  case "$CONFIG_TMP_DIR" in
    "${TMPDIR:-/tmp}"/imboy-ah-e2e-config.*) rm -rf -- "$CONFIG_TMP_DIR" ;;
  esac
  if ! remaining="$(psql -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" -d postgres -tAc \
      "SELECT 1 FROM pg_database WHERE datname='$DB'" 2>/dev/null)"; then
    failed=1
  elif [[ "$remaining" == *1* ]]; then
    failed=1
  fi
  if [[ -n "$CONFIG_TMP_DIR" && -e "$CONFIG_TMP_DIR" ]]; then
    failed=1
  fi
  [[ "$failed" -eq 0 ]] && CLEANUP_PASSED=1 || CLEANUP_PASSED=0
  CLEANUP_DONE=1
}

write_manifest() {
  (
    cd "$EVIDENCE_DIR"
    find . -type f ! -name 'manifest.sha256*' -exec shasum -a 256 {} \; \
      > manifest.sha256.tmp
    mv manifest.sha256.tmp manifest.sha256
  )
}

write_evidence() {
  local status="$1"
  local failed_code="${2:-0}"
  python3 "$ROOT/scripts/write_agent_hub_e2e_evidence.py" \
    --evidence-dir "$EVIDENCE_DIR" --repo-root "$ROOT" --status "$status" \
    --imboy-sha "$BASE_IMBOY" --imboyapp-sha "$BASE_IMBOYAPP" \
    --imboyadmin-sha "$BASE_IMBOYADMIN" --suites-passed "$SUITES_PASSED" \
    --trace-exit "$TRACE_EXIT" --cleanup-passed "$CLEANUP_PASSED" \
    --sensitive-scan-passed "$SENSITIVE_SCAN_PASSED" \
    --failed-step "$CURRENT_STEP" --failed-code "$failed_code"
}

# shellcheck disable=SC2329 # Invoked indirectly by the EXIT trap below.
on_exit() {
  local code="$?"
  trap - EXIT
  set +e
  cleanup
  if [[ "$RUN_FINISHED" -eq 0 ]]; then
    write_evidence FAIL "$code"
    python3 "$ROOT/scripts/verify_agent_hub_task_evidence.py" \
      --task "$EVIDENCE_DIR/evidence.json" > "$EVIDENCE_DIR/evidence-verifier.json"
    write_manifest
  fi
  exit "$code"
}
trap on_exit EXIT

echo "[golden] profile=$PROFILE db=$DB evidence=$EVIDENCE_DIR"
mkdir -p "$EVIDENCE_DIR"
CONFIG_TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/imboy-ah-e2e-config.XXXXXX")"
cp "$ROOT/config/sys.config.example" "$CONFIG_TMP_DIR/sys.eunit.config"
EUNIT_CONFIG="$CONFIG_TMP_DIR/sys.eunit"

# 1) scratch 库 + 扩展
CURRENT_STEP="create marker scratch database"
psql -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" -d postgres \
  -c "DROP DATABASE IF EXISTS $DB" -c "CREATE DATABASE $DB" > /dev/null
for E in pg_jieba postgis postgis_raster timescaledb pgcrypto uuid-ossp pg_trgm \
         btree_gin btree_gist unaccent pg_stat_statements; do
  $PSQL -d "$DB" -c "CREATE EXTENSION IF NOT EXISTS $E" > /dev/null 2>&1 || true
done

# 2) 全链迁移 up（1→93+），由生产同口径 strict 迁移器维护 tracking。
CURRENT_STEP="make app"
make -C "$ROOT" app > "$EVIDENCE_DIR/make-app.log" 2>&1
CURRENT_STEP="apply migrations to marker scratch database"
PGHOST="$PGHOST" PGPORT="$PGPORT" PGUSER="$PGUSER" \
  PGDATABASE="$DB" IMBOY_DIR="$ROOT" "$ROOT/scripts/drill_migrate.escript" up \
  > "$EVIDENCE_DIR/migration-up.log" 2>&1
echo "[golden] migrations up OK"

# 3) 核心套件（golden flow 的可自动化子集；测试内含正负例）
export IMBOYENV=local
export IMBOY_PG_HOST="$PGHOST"
export IMBOY_PG_PORT="$PGPORT"
export IMBOY_PG_USERNAME="$PGUSER"
export IMBOY_PG_PASSWORD="$PGPASSWORD"
export IMBOY_PG_DATABASE="$DB"
SUITES=(
  channel_webhook_logic_tests
  ai_agent_reply_tests
  ai_agent_tool_loop_tests
  mcp_authz_gate_tests
  mcp_client_repo_tests
  imboy_mcp_task_tools_tests
  agent_task_repo_tests
  agent_task_logic_tests
  bot_group_mention_tests
  bot_logic_tests
  bot_webhook_logic_tests
  bot_webhook_delivery_repo_tests
  bot_webhook_delivery_worker_tests
  agent_hub_runtime_trace_tests
)
for S in "${SUITES[@]}"; do
  CURRENT_STEP="make eunit-local t=$S"
  echo "[golden] eunit $S"
  EUNIT_CONFIG="$EUNIT_CONFIG" make -C "$ROOT" eunit-local "t=$S" \
    > "$EVIDENCE_DIR/eunit-$S.log" 2>&1 || {
    echo "[golden] SUITE_FAIL $S"; exit 1; }
  SUITES_PASSED=$((SUITES_PASSED + 1))
done

# 3.5) 日志扫描：不回显命中内容，避免扫描器本身扩散凭证或 PII（A04）。
SENSITIVE_PATTERN='bearer[[:space:]]+[A-Za-z0-9._-]{16,}|api[_-]?key['"'"'"[:space:]:=]+[A-Za-z0-9._-]{12,}|(verify|api|access|refresh)[_-]?token['"'"'"[:space:]:=]+[A-Za-z0-9._-]{12,}|[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}|1[3-9][0-9]{9}'
if grep -qiE "$SENSITIVE_PATTERN" "$EVIDENCE_DIR"/eunit-*.log 2>/dev/null; then
  echo "[golden] SECRET LEAK in logs"; exit 1
fi
SENSITIVE_SCAN_PASSED=1

# 4) 负例断言：E2EE/禁用路径由套件内覆盖（A03/A04 语义），
#    此处追加跨层断言：迁移后的表约束存在性
$PSQL -d "$DB" -tAc "SELECT 1 FROM information_schema.table_constraints
  WHERE constraint_name='bot_delivery_status_check'" | grep -q 1 \
  || { echo "[golden] bot_delivery status check missing"; exit 1; }
$PSQL -d "$DB" -tAc "SELECT 1 FROM information_schema.table_constraints
  WHERE constraint_name='agent_task_status_check'" | grep -q 1 \
  || { echo "[golden] agent_task status check missing"; exit 1; }

# 5) 从真实持久化 fixture 行导出 correlation 链，再由冻结 verifier 复核。
CURRENT_STEP="verify runtime correlation trace"
TRACE_CORR="$($PSQL -d "$DB" -tAc \
  "SELECT correlation_id FROM public.agent_task
   WHERE tool='agent_hub.trace.v1' ORDER BY created_at DESC LIMIT 1")"
if [[ ! "$TRACE_CORR" =~ ^corr-[A-Za-z0-9_-]{16,59}$ ]]; then
  echo "[golden] runtime trace seed correlation missing" >&2
  exit 1
fi
set +e
$PSQL -d "$DB" -At -v correlation_id="$TRACE_CORR" \
  -f "$ROOT/scripts/export_agent_hub_correlation_trace.sql" \
  > "$EVIDENCE_DIR/runtime-correlation-trace.json"
EXPORT_EXIT=$?
if [[ "$EXPORT_EXIT" -eq 0 ]]; then
  python3 "$ROOT/scripts/verify_agent_hub_correlation_trace.py" \
  "$EVIDENCE_DIR/runtime-correlation-trace.json" > "$EVIDENCE_DIR/trace-verifier.json"
  TRACE_EXIT=$?
else
  TRACE_EXIT="$EXPORT_EXIT"
  printf '{"decision":"VIOLATION","errors":["export.command_failed"]}\n' \
    > "$EVIDENCE_DIR/trace-verifier.json"
fi
set -e

# 6) 先清理并核验，再结算证据。当前自动化范围不足以满足 E2E-01，固定 PARTIAL。
CURRENT_STEP="cleanup marker scratch resources"
cleanup
write_evidence PARTIAL
set +e
python3 "$ROOT/scripts/verify_agent_hub_task_evidence.py" \
  --task "$EVIDENCE_DIR/evidence.json" > "$EVIDENCE_DIR/evidence-self-verifier.json"
EVIDENCE_EXIT=$?
set -e
if [[ "$EVIDENCE_EXIT" -ne 1 ]]; then
  echo "[golden] evidence verifier expected PARTIAL (exit 1), got $EVIDENCE_EXIT" >&2
  exit 2
fi
write_manifest
RUN_FINISHED=1
echo "[golden] evidence written to $EVIDENCE_DIR"
echo "[golden] PARTIAL: runtime HTTP trace, restart recovery, and final Base rerun remain open"
exit 1
