#!/usr/bin/env bash
# E2E-01：Agent Hub 本地 Golden Flow harness。
# 从空 scratch 环境复现：建库 → 扩展 → 全链迁移(1→93+) → 核心套件 → 证据 → 清理。
# 所有资源带 marker（库名前缀 imboy_ah_e2e_），清理只删除带 marker 的对象。
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PROFILE="local-fixture"
EVIDENCE_DIR="${IMBOY_EVIDENCE_ROOT:-$ROOT/docs/compliance}/agent-hub-e2e"
MARKER_DB_PREFIX="imboy_ah_e2e_"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --profile) PROFILE="$2"; shift 2 ;;
    --profile=*) PROFILE="${1#*=}"; shift ;;
    --evidence-dir) EVIDENCE_DIR="$2"; shift 2 ;;
    --evidence-dir=*) EVIDENCE_DIR="${1#*=}"; shift ;;
    *) echo "unknown arg: $1" >&2; exit 2 ;;
  esac
done

DB="${MARKER_DB_PREFIX}$(date +%s)"
PGHOST="${PGHOST:-127.0.0.1}"
PGPORT="${PGPORT:-4323}"
PGUSER="${PGUSER:-imboy_user}"
export PGPASSWORD="${PGPASSWORD:-abc54321}"

PSQL="psql -h $PGHOST -p $PGPORT -U $PGUSER -v ON_ERROR_STOP=1 -q"

cleanup() {
  echo "[golden] cleanup: drop $DB"
  psql -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" -d postgres \
    -c "DROP DATABASE IF EXISTS $DB" > /dev/null 2>&1 || true
}
trap cleanup EXIT

echo "[golden] profile=$PROFILE db=$DB evidence=$EVIDENCE_DIR"
mkdir -p "$EVIDENCE_DIR"

# 1) scratch 库 + 扩展
psql -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" -d postgres \
  -c "DROP DATABASE IF EXISTS $DB" -c "CREATE DATABASE $DB" > /dev/null
for E in pg_jieba postgis postgis_raster timescaledb pgcrypto uuid-ossp pg_trgm \
         btree_gin btree_gist unaccent pg_stat_statements; do
  $PSQL -d "$DB" -c "CREATE EXTENSION IF NOT EXISTS $E" > /dev/null 2>&1 || true
done

# 2) 全链迁移 up（1→93+）
FAIL=0
for f in "$ROOT"/priv/migrations/*.up.sql; do
  $PSQL -d "$DB" -f "$f" > /dev/null 2>&1 || { echo "[golden] UP_FAIL $f"; FAIL=1; }
done
[ "$FAIL" = "0" ] || { echo "[golden] migration up FAILED"; exit 1; }
echo "[golden] migrations up OK"

# 3) 核心套件（golden flow 的可自动化子集；测试内含正负例）
export IMBOYENV=local
SUITES=(agent_task_repo_tests agent_task_logic_tests bot_webhook_delivery_repo_tests
        bot_group_mention_tests mcp_client_repo_tests)
for S in "${SUITES[@]}"; do
  echo "[golden] eunit $S"
  make -C "$ROOT" eunit-local "t=$S" > "/tmp/gf-$S.log" 2>&1 || {
    echo "[golden] SUITE_FAIL $S"; cp "/tmp/gf-$S.log" "$EVIDENCE_DIR/" 2>/dev/null || true; exit 1; }
  cp "/tmp/gf-$S.log" "$EVIDENCE_DIR/eunit-$S.log"
done

# 3.5) 日志扫描：无 secret/主密钥字样（A04）
if grep -qiE 'postgre_aes_key|wh-verify-secret|topsecret' /tmp/gf-*.log 2>/dev/null; then
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
  find . -type f ! -name manifest.sha256 -exec shasum -a 256 {} \; > manifest.sha256
)
echo "[golden] evidence written to $EVIDENCE_DIR"
echo "[golden] PASS"
