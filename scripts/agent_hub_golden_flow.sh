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

cleanup() {
  echo "[golden] cleanup: drop $DB"
  psql -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" -d postgres \
    -c "DROP DATABASE IF EXISTS $DB" > /dev/null 2>&1 || true
  case "$CONFIG_TMP_DIR" in
    "${TMPDIR:-/tmp}"/imboy-ah-e2e-config.*) rm -rf -- "$CONFIG_TMP_DIR" ;;
  esac
}
trap cleanup EXIT

echo "[golden] profile=$PROFILE db=$DB evidence=$EVIDENCE_DIR"
mkdir -p "$EVIDENCE_DIR"
CONFIG_TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/imboy-ah-e2e-config.XXXXXX")"
cp "$ROOT/config/sys.config.example" "$CONFIG_TMP_DIR/sys.eunit.config"
EUNIT_CONFIG="$CONFIG_TMP_DIR/sys.eunit"

# 1) scratch 库 + 扩展
psql -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" -d postgres \
  -c "DROP DATABASE IF EXISTS $DB" -c "CREATE DATABASE $DB" > /dev/null
for E in pg_jieba postgis postgis_raster timescaledb pgcrypto uuid-ossp pg_trgm \
         btree_gin btree_gist unaccent pg_stat_statements; do
  $PSQL -d "$DB" -c "CREATE EXTENSION IF NOT EXISTS $E" > /dev/null 2>&1 || true
done

# 2) 全链迁移 up（1→93+），由生产同口径 strict 迁移器维护 tracking。
make -C "$ROOT" app > "$EVIDENCE_DIR/make-app.log" 2>&1
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
)
for S in "${SUITES[@]}"; do
  echo "[golden] eunit $S"
  EUNIT_CONFIG="$EUNIT_CONFIG" make -C "$ROOT" eunit-local "t=$S" \
    > "$EVIDENCE_DIR/eunit-$S.log" 2>&1 || {
    echo "[golden] SUITE_FAIL $S"; exit 1; }
done

# 3.5) 日志扫描：不回显命中内容，避免扫描器本身扩散凭证或 PII（A04）。
SENSITIVE_PATTERN='bearer[[:space:]]+[A-Za-z0-9._-]{16,}|api[_-]?key['"'"'"[:space:]:=]+[A-Za-z0-9._-]{12,}|(verify|api|access|refresh)[_-]?token['"'"'"[:space:]:=]+[A-Za-z0-9._-]{12,}|[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}|1[3-9][0-9]{9}'
if grep -qiE "$SENSITIVE_PATTERN" "$EVIDENCE_DIR"/eunit-*.log 2>/dev/null; then
  echo "[golden] SECRET LEAK in logs"; exit 1
fi

# 4) 负例断言：E2EE/禁用路径由套件内覆盖（A03/A04 语义），
#    此处追加跨层断言：迁移后的表约束存在性
$PSQL -d "$DB" -tAc "SELECT 1 FROM information_schema.table_constraints
  WHERE constraint_name='bot_delivery_status_check'" | grep -q 1 \
  || { echo "[golden] bot_delivery status check missing"; exit 1; }
$PSQL -d "$DB" -tAc "SELECT 1 FROM information_schema.table_constraints
  WHERE constraint_name='agent_task_status_check'" | grep -q 1 \
  || { echo "[golden] agent_task status check missing"; exit 1; }

# 5) 证据清单
(
  cd "$EVIDENCE_DIR"
  find . -type f ! -name 'manifest.sha256*' -exec shasum -a 256 {} \; \
    > manifest.sha256.tmp
  mv manifest.sha256.tmp manifest.sha256
)
echo "[golden] evidence written to $EVIDENCE_DIR"
echo "[golden] PASS"
