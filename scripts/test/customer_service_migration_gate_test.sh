#!/usr/bin/env bash
# =============================================================================
# 客服迁移门自测（CP-ASSET-01 验收面）—— 在一次性 scratch 库上复现门脚本的
# 全部判定分支：
#
#   N1 空库          —— 全新库（无 schema_migrations）→ 门必须非零
#   N2 foreign sentinel —— a) schema_migrations.version 改成目录全集外版本(0)
#                          b) history 混入目录全集外版本(0)  → 门必须非零
#   N3 脏库          —— a) schema_migrations.dirty=true
#                          b) history 删掉中间客服版本(148)   → 门必须非零
#   N4 空 oracle     —— --migrations-dir 指向空目录           → 门必须非零
#   P1 正例          —— 全量应用迁移 + 按 erlang_migrate 语义记账后 → 门必须 exit 0
#
# scratch 库名固定前缀 imboy_cp12_<短hex>_as01*（用后 DROP，脚本自身带清理）。
# 正例的"应用全部迁移"用 psql 按序执行 priv/migrations/*.up.sql，随后按
# erlang_migrate 落库语义写 schema_migrations（单行 max 版本 + dirty=false）与
# schema_migrations_history（每版本一行）——终态与逐步执行 erlang_migrate:up/1
# 完全一致（两表只有集合语义、无顺序字段）。
#
# 前置：可写的 PG 集群（需能 CREATE DATABASE + 安装扩展），连接参数走
# PGHOST/PGPORT/PGUSER/PGPASSWORD 环境变量；不可达时整体 SKIP（exit 3，
# "没测"不冒充"通过"）。N4 不依赖 PG，永远执行。
# =============================================================================
set -uo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$REPO_ROOT"
GATE="$REPO_ROOT/scripts/customer_service_migration_gate.sh"

: "${PGHOST:=127.0.0.1}"
: "${PGPORT:=4323}"
: "${PGUSER:=imboy_user}"
: "${PGPASSWORD:=}"
export PGHOST PGPORT PGUSER PGPASSWORD

HEX="$(printf '%s' "$$" | md5 | head -c 6)"
DB_MAIN="imboy_cp12_${HEX}_as01"
DB_EMPTY="imboy_cp12_${HEX}_as01_empty"
DBS_DROPPED=""

PASS=0
FAIL_CNT=0

say()  { printf '%s\n' "$*"; }
pass() { say "  ✅ $*"; PASS=$((PASS + 1)); }
fail() { say "  ❌ $*" >&2; FAIL_CNT=$((FAIL_CNT + 1)); }

psql_admin() {
  PGPASSWORD="$PGPASSWORD" psql -X -v ON_ERROR_STOP=1 -h "$PGHOST" -p "$PGPORT" \
    -U "$PGUSER" "$@" >/dev/null
}
psql_db() { # 在指定库上执行
  PGPASSWORD="$PGPASSWORD" psql -X -v ON_ERROR_STOP=1 -h "$PGHOST" -p "$PGPORT" \
    -U "$PGUSER" -d "$1" "${@:2}"
}

drop_dbs() {
  for db in "$DB_MAIN" "$DB_EMPTY"; do
    if psql_admin -d postgres -c "DROP DATABASE IF EXISTS \"$db\"" 2>/dev/null; then
      DBS_DROPPED="${DBS_DROPPED}${db} "
    else
      say "  ⚠️  DROP DATABASE $db 失败，需人工清理" >&2
    fi
  done
}
cleanup() {
  drop_dbs
  say ""
  say "清理证明：已 DROP 的 scratch 库：${DBS_DROPPED:-<无>}"
  remaining="$(PGPASSWORD="$PGPASSWORD" psql -X -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" \
    -d postgres -tAc "SELECT datname FROM pg_database WHERE datname LIKE 'imboy_cp12_${HEX}%'" 2>/dev/null || true)"
  if [ -n "$remaining" ]; then
    say "  ❌ 仍有残留库：$remaining" >&2
    [ "$FAIL_CNT" -eq 0 ] && FAIL_CNT=1
  else
    say "  ✅ pg_database 无 imboy_cp12_${HEX}% 残留"
  fi
}
trap cleanup EXIT

# 判定辅助：运行门并断言退出码（库经 PGDATABASE 传入，与 libpq 惯例一致）
expect_gate() { # $1=期望码 $2=场景名 $3=库名（空=不连库场景）其余=门的参数
  local want="$1" label="$2" db="$3"; shift 3
  set +e
  PGDATABASE="$db" "$GATE" "$@" >/tmp/cp12_gate_out.$$ 2>&1
  local got=$?
  set -e
  say "--- $label ---"
  sed 's/^/    | /' /tmp/cp12_gate_out.$$
  if [ "$got" -eq "$want" ]; then
    # bash 3.2 + UTF-8：$var 后紧跟全角标点会被并进变量名，必须用 ${var}。
    pass "${label}：exit=${got}（期望 ${want}）"
  else
    fail "${label}：exit=${got}，期望 ${want}"
  fi
  rm -f /tmp/cp12_gate_out.$$
}

say "=== 客服迁移门自测（scratch 前缀 imboy_cp12_${HEX}_as01*）==="

# ---- N4 空 oracle（不依赖 PG，先跑）----------------------------------------
EMPTY_ORACLE_DIR="$(mktemp -d /tmp/cp12_empty_oracle.XXXXXX)"
expect_gate 4 "N4 空 oracle（--migrations-dir 空目录）" "" \
  --migrations-dir "$EMPTY_ORACLE_DIR"
rmdir "$EMPTY_ORACLE_DIR"

# ---- PG 可达性 ---------------------------------------------------------------
if ! PGPASSWORD="$PGPASSWORD" psql -X -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" \
     -d postgres -tAc "SELECT 1" >/dev/null 2>&1; then
  say "SKIP: PG ${PGUSER}@${PGHOST}:${PGPORT} 不可达——N1/N2/N3/P1 未测（不冒充通过）"
  [ "$FAIL_CNT" -eq 0 ] && exit 3 || exit 1
fi
say "PG 可达：${PGUSER}@${PGHOST}:${PGPORT}"

# ---- scratch 库准备 ----------------------------------------------------------
say "--- 准备 scratch 库（扩展 + 全量迁移 + erlang_migrate 记账）---"
psql_admin -d postgres -c "CREATE DATABASE \"$DB_MAIN\""
psql_admin -d postgres -c "CREATE DATABASE \"$DB_EMPTY\""
# 迁移不含 CREATE EXTENSION（见 run_pg_behavior_harnesses.sh 前置说明），
# 与 imboy_test_v1 同款扩展集预铺。
psql_db "$DB_MAIN" -c "CREATE EXTENSION IF NOT EXISTS pgcrypto" \
                    -c "CREATE EXTENSION IF NOT EXISTS postgis" \
                    -c "CREATE EXTENSION IF NOT EXISTS timescaledb" \
                    -c "CREATE EXTENSION IF NOT EXISTS vector" \
                    -c "CREATE EXTENSION IF NOT EXISTS pg_jieba" >/dev/null || {
  say "  ❌ 扩展安装失败（集群需预装 pg_jieba/postgis/timescaledb/vector/pgcrypto）" >&2
  exit 1
}

APPLY_FAIL=0
for up in priv/migrations/*.up.sql; do
  if ! psql_db "$DB_MAIN" -q -f "$up" >/dev/null 2>&1; then
    say "  ❌ 迁移执行失败：$up" >&2
    APPLY_FAIL=1
    break
  fi
done
[ "$APPLY_FAIL" -eq 0 ] || exit 1

# 按 erlang_migrate 终态记账：schema_migrations 单行（head, false）+
# schema_migrations_history 每版本一行（含 head）。
ALL_VERSIONS="$(ls priv/migrations/*.up.sql | sed 's/.*\///; s/_.*//' | sort -n)"
HEAD_VERSION="$(printf '%s\n' "$ALL_VERSIONS" | tail -1)"
psql_db "$DB_MAIN" \
  -c "CREATE TABLE schema_migrations (version BIGINT PRIMARY KEY, dirty BOOLEAN NOT NULL DEFAULT false, applied_at TIMESTAMPTZ NOT NULL DEFAULT now())" \
  -c "CREATE TABLE schema_migrations_history (version BIGINT PRIMARY KEY, applied_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP)" \
  >/dev/null
for v in $ALL_VERSIONS; do
  psql_db "$DB_MAIN" -q -c "INSERT INTO schema_migrations_history (version) VALUES ($((10#${v})))" >/dev/null
done
psql_db "$DB_MAIN" -q -c "INSERT INTO schema_migrations (version, dirty) VALUES ($((10#${HEAD_VERSION})), false)" >/dev/null
say "  全量迁移 + 记账完成（head=$((10#${HEAD_VERSION}))）"

# ---- P1 正例 ----------------------------------------------------------------
expect_gate 0 "P1 正例（全量应用后）" "$DB_MAIN"

# ---- N1 空库 ----------------------------------------------------------------
expect_gate 2 "N1 空库（无 schema_migrations）" "$DB_EMPTY"

# ---- N2 foreign sentinel ----------------------------------------------------
psql_db "$DB_MAIN" -q -c "UPDATE schema_migrations SET version = 0" >/dev/null
expect_gate 2 "N2a foreign sentinel（schema_migrations.version=0）" "$DB_MAIN"
psql_db "$DB_MAIN" -q -c "UPDATE schema_migrations SET version = $((10#${HEAD_VERSION}))" >/dev/null

psql_db "$DB_MAIN" -q -c "INSERT INTO schema_migrations_history (version) VALUES (0)" >/dev/null
expect_gate 2 "N2b foreign sentinel（history 混入版本 0）" "$DB_MAIN"
psql_db "$DB_MAIN" -q -c "DELETE FROM schema_migrations_history WHERE version = 0" >/dev/null

# ---- N3 脏库 ----------------------------------------------------------------
psql_db "$DB_MAIN" -q -c "UPDATE schema_migrations SET dirty = true" >/dev/null
expect_gate 2 "N3a 脏库（dirty=true）" "$DB_MAIN"
psql_db "$DB_MAIN" -q -c "UPDATE schema_migrations SET dirty = false" >/dev/null

psql_db "$DB_MAIN" -q -c "DELETE FROM schema_migrations_history WHERE version = 148" >/dev/null
expect_gate 2 "N3b 脏库（history 缺中间客服版本 148）" "$DB_MAIN"
psql_db "$DB_MAIN" -q -c "INSERT INTO schema_migrations_history (version) VALUES (148)" >/dev/null

# ---- 汇总 -------------------------------------------------------------------
say ""
say "=== 自测汇总：PASS=${PASS} FAIL=${FAIL_CNT} ==="
[ "$FAIL_CNT" -eq 0 ] || exit 1
exit 0
