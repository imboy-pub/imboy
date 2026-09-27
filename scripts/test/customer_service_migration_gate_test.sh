#!/usr/bin/env bash
# =============================================================================
# 客服迁移门自测（CP-ASSET-01 验收面）—— 在一次性 fixture 目录 / scratch 库上
# 复现门脚本的全部判定分支：
#
#   静态四负例（mktemp -d 临时 fixture 目录，用完删除，不污染真 migrations）：
#   S-a 重复版本号   —— 同版本两个名字的 up/down → 门 exit 4（"版本号重复"）
#   S-b 缺 down 文件 —— 只有 up                    → 门 exit 4（"缺 down 文件"）
#   S-c 不配对       —— 同版本 up/down 名字错开    → 门 exit 4（"缺 up 文件"）
#   S-d 版本乱序     —— 同名 down 挂在更高版本上   → 门 exit 4（跨版本错位成
#                       两条缺件，即 up/down 链乱序）
#   N4 空 oracle     —— --migrations-dir 指向空目录 → 门必须非零
#   库状态四负例（一次性 scratch 库）：
#   N1 空库          —— 全新库（无 schema_migrations）→ 门必须非零
#   N2 foreign sentinel —— a) schema_migrations.version 改成目录全集外版本(0)
#                           b) history 混入目录全集外版本(0)  → 门必须非零
#   N3 脏库          —— a) schema_migrations.dirty=true
#                       b) history 删掉中间客服版本(148)   → 门必须非零
#   P1 正例          —— 全量应用迁移 + 按 erlang_migrate 语义记账后 → 门必须 exit 0
#
# scratch 正例库固定名 imboy_cp12_gate01（已存在则先 DROP 再重建，脚本自身带
# 清理退出时 DROP）；N1 辅助库 imboy_cp12_gate01_empty 同批清理。
#
# 正例的"应用全部迁移"用 psql 按序执行 priv/migrations/*.up.sql，随后按
# erlang_migrate 落库语义写 schema_migrations（单行 max 版本 + dirty=false）与
# schema_migrations_history（每版本一行）——终态与逐步执行 erlang_migrate:up/1
# 完全一致（两表只有集合语义、无顺序字段）。
#
# 前置：可写的 PG 集群（需能 CREATE DATABASE + 安装扩展），连接参数走
# PGHOST/PGPORT/PGUSER/PGPASSWORD 环境变量；不可达时整体 SKIP（exit 3，
# "没测"不冒充"通过"）。静态四负例与 N4 不依赖 PG，永远执行。
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

# 正例 scratch 库：本卡契约固定名（已存在则先 DROP 再重建）；N1 辅助空库带
# 同名 _empty 后缀，二者均在 EXIT 清理中 DROP。
DB_MAIN="imboy_cp12_gate01"
DB_EMPTY="imboy_cp12_gate01_empty"
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
    -d postgres -tAc "SELECT datname FROM pg_database WHERE datname LIKE 'imboy_cp12_gate01%'" 2>/dev/null || true)"
  if [ -n "$remaining" ]; then
    say "  ❌ 仍有残留库：$remaining" >&2
    [ "$FAIL_CNT" -eq 0 ] && FAIL_CNT=1
  else
    say "  ✅ pg_database 无 imboy_cp12_gate01% 残留"
  fi
}
trap cleanup EXIT

# 判定辅助：运行门并断言退出码（库经 PGDATABASE 传入，与 libpq 惯例一致）
# $1=期望码 $2=场景名 $3=库名（空=不连库场景）$4=输出须命中的 grep -E 模式
#（空=不断言消息）其余=门的参数
expect_gate() { # $1=期望码 $2=场景名 $3=库名 $4=消息模式 其余=门的参数
  local want="$1" label="$2" db="$3" pat="$4"; shift 4
  set +e
  PGDATABASE="$db" "$GATE" "$@" >/tmp/cp12_gate_out.$$ 2>&1
  local got=$?
  set -e
  say "--- $label ---"
  sed 's/^/    | /' /tmp/cp12_gate_out.$$
  local code_ok=1 msg_ok=1
  [ "$got" -eq "$want" ] || code_ok=0
  if [ -n "$pat" ] && ! grep -Eq "$pat" /tmp/cp12_gate_out.$$; then
    msg_ok=0
  fi
  if [ "$code_ok" -eq 1 ] && [ "$msg_ok" -eq 1 ]; then
    # bash 3.2 + UTF-8：$var 后紧跟全角标点会被并进变量名，必须用 ${var}。
    if [ -n "$pat" ]; then
      pass "${label}：exit=${got}（期望 ${want}）+ 命中「${pat}」"
    else
      pass "${label}：exit=${got}（期望 ${want}）"
    fi
  else
    [ "$code_ok" -eq 0 ] && fail "${label}：exit=${got}，期望 ${want}"
    [ "$msg_ok" -eq 0 ] && fail "${label}：退出码对但未命中「${pat}」"
  fi
  rm -f /tmp/cp12_gate_out.$$
}

# 静态负例 fixture：在 mktemp -d 临时目录里拼迁移文件对，跑门（--migrations-dir
# 指向 fixture），断言 exit 4 + 静态门消息；用完 rm -rf，不碰真 priv/migrations。
mk_pair() { # $1=fixture目录 其余=文件名列表（写入最小合法 SQL，避免"空 up"噪声）
  local d="$1"; shift
  local f
  for f in "$@"; do printf 'SELECT 1;\n' > "$d/$f"; done
}
static_negative() { # $1=标签 $2=消息模式 $3...=文件名列表
  local label="$1" pat="$2"; shift 2
  local d
  d="$(mktemp -d /tmp/cp12_static.XXXXXX)"
  mk_pair "$d" "$@"
  expect_gate 4 "$label" "" "$pat" --migrations-dir "$d"
  rm -rf "$d"
}

say "=== 客服迁移门自测（scratch 库 ${DB_MAIN} + 临时 fixture 目录）==="

# ---- 静态四负例（mktemp fixture，不依赖 PG，先跑）---------------------------
static_negative "S-a 重复版本号（同版本两个名字）" "版本号重复" \
  00000001_a.up.sql 00000001_a.down.sql 00000001_b.up.sql 00000001_b.down.sql
static_negative "S-b 缺 down 文件（只有 up）" "缺 down 文件" \
  00000001_a.up.sql
static_negative "S-c up/down 不配对（同版本名字错开）" "缺 up 文件" \
  00000001_a.up.sql 00000001_b.down.sql
static_negative "S-d 版本乱序（同名 down 挂到更高版本，跨版本错位）" "缺 down 文件" \
  00000001_a.up.sql 00000002_a.down.sql

# ---- N4 空 oracle（不依赖 PG，先跑）----------------------------------------
EMPTY_ORACLE_DIR="$(mktemp -d /tmp/cp12_empty_oracle.XXXXXX)"
expect_gate 4 "N4 空 oracle（--migrations-dir 空目录）" "" "" \
  --migrations-dir "$EMPTY_ORACLE_DIR"
rmdir "$EMPTY_ORACLE_DIR"

# ---- PG 可达性 ---------------------------------------------------------------
if ! PGPASSWORD="$PGPASSWORD" psql -X -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" \
     -d postgres -tAc "SELECT 1" >/dev/null 2>&1; then
  say "SKIP: PG ${PGUSER}@${PGHOST}:${PGPORT} 不可达——N1/N2/N3/P1 未测（不冒充通过）"
  [ "$FAIL_CNT" -eq 0 ] && exit 3 || exit 1
fi
say "PG 可达：${PGUSER}@${PGHOST}:${PGPORT}"

# ---- scratch 库准备（固定名，已存在则先 DROP 再重建）------------------------
say "--- 准备 scratch 库（扩展 + 全量迁移 + erlang_migrate 记账）---"
psql_admin -d postgres -c "DROP DATABASE IF EXISTS \"$DB_MAIN\""
psql_admin -d postgres -c "DROP DATABASE IF EXISTS \"$DB_EMPTY\""
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
expect_gate 0 "P1 正例（全量应用后）" "$DB_MAIN" ""

# ---- N1 空库 ----------------------------------------------------------------
expect_gate 2 "N1 空库（无 schema_migrations）" "$DB_EMPTY" ""

# ---- N2 foreign sentinel ----------------------------------------------------
psql_db "$DB_MAIN" -q -c "UPDATE schema_migrations SET version = 0" >/dev/null
expect_gate 2 "N2a foreign sentinel（schema_migrations.version=0）" "$DB_MAIN" "foreign"
psql_db "$DB_MAIN" -q -c "UPDATE schema_migrations SET version = $((10#${HEAD_VERSION}))" >/dev/null

psql_db "$DB_MAIN" -q -c "INSERT INTO schema_migrations_history (version) VALUES (0)" >/dev/null
expect_gate 2 "N2b foreign sentinel（history 混入版本 0）" "$DB_MAIN" "foreign"
psql_db "$DB_MAIN" -q -c "DELETE FROM schema_migrations_history WHERE version = 0" >/dev/null

# ---- N3 脏库 ----------------------------------------------------------------
psql_db "$DB_MAIN" -q -c "UPDATE schema_migrations SET dirty = true" >/dev/null
expect_gate 2 "N3a 脏库（dirty=true）" "$DB_MAIN" "脏库"
psql_db "$DB_MAIN" -q -c "UPDATE schema_migrations SET dirty = false" >/dev/null

psql_db "$DB_MAIN" -q -c "DELETE FROM schema_migrations_history WHERE version = 148" >/dev/null
expect_gate 2 "N3b 脏库（history 缺中间客服版本 148）" "$DB_MAIN" "历史缺中间版本"
psql_db "$DB_MAIN" -q -c "INSERT INTO schema_migrations_history (version) VALUES (148)" >/dev/null

# ---- 汇总 -------------------------------------------------------------------
say ""
say "=== 自测汇总：PASS=${PASS} FAIL=${FAIL_CNT} ==="
[ "$FAIL_CNT" -eq 0 ] || exit 1
exit 0
