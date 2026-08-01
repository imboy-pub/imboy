#!/usr/bin/env bash
# ============================================================
# PostgreSQL 恢复 / Restore PostgreSQL from a pg_dump (-Fc) backup
# ------------------------------------------------------------
# 被 docs/guides/operations/deployment/BACKUP-RESTORE.md 引用。
# 含恢复后行数抽样校验，用于备份/恢复演练（A4 验收）。
#
# 用法 / Usage:
#   bash script/restore_pg.sh <backup-file.dump>
#   bash script/restore_pg.sh <backup-file.dump> --target imboy_restore_test
#
# ⚠️ 危险：--target 指向已有库会先 DROP 再重建。务必确认目标库。
# ============================================================
set -euo pipefail

PG_CONTAINER="${PG_CONTAINER:-imboy_pg18}"
POSTGRES_USER="${POSTGRES_USER:-imboy_user}"
SOURCE_DB="${POSTGRES_DB:-imboy_pro}"

RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[1;33m'; NC='\033[0m'
info() { echo -e "${GREEN}[restore_pg]${NC} $*"; }
warn() { echo -e "${YELLOW}[restore_pg]${NC} $*"; }
fail() { echo -e "${RED}[restore_pg] ERROR:${NC} $*" >&2; exit 1; }

# ---------- 参数解析 ----------
BACKUP_FILE="${1:-}"
TARGET_DB="$SOURCE_DB"
shift || true
while [ $# -gt 0 ]; do
  case "$1" in
    --target) TARGET_DB="${2:-}"; shift 2 ;;
    *) fail "未知参数: $1" ;;
  esac
done

[ -n "$BACKUP_FILE" ] || fail "用法: bash script/restore_pg.sh <backup-file.dump> [--target DB]"

# ---------- B-28 生产库守卫（在任何连库动作之前）----------
# 本脚本第 50 行起会 `DROP DATABASE IF EXISTS <target>` —— 目标写错就是**不可逆的
# 生产数据删除**。而 --target 缺省值恰好就是生产库名（SOURCE_DB），
# 也就是说"忘了带 --target"这一个手滑，默认后果就是删生产。
#
# 真实灾难恢复确实需要恢复回生产库，所以守卫**可覆盖但必须显式**：
#   ALLOW_PRODUCTION_TARGET=1 bash scripts/restore_pg.sh <file> --target imboy_pro
# 且即使覆盖，也仍要走下面的交互确认（FORCE=1 不能同时绕过两道）。
assert_target_not_production() {
  local target="$1" production="$2"
  [ "$target" = "$production" ] || return 0

  if [ "${ALLOW_PRODUCTION_TARGET:-}" = "1" ]; then
    warn "⚠️  目标是生产库 '${production}'，已由 ALLOW_PRODUCTION_TARGET=1 显式放行"
    if [ "${FORCE:-}" = "1" ]; then
      fail "拒绝执行：恢复到生产库时不允许同时使用 FORCE=1 跳过人工确认"
    fi
    return 0
  fi

  fail "拒绝执行：目标库 '${target}' 就是生产库。
  本脚本会先 DROP 目标库，误操作不可逆。
  - 演练/验证请指定独立目标：--target imboy_restore_test
  - 确属灾难恢复：ALLOW_PRODUCTION_TARGET=1 bash \$0 <file> --target ${production}
    （该模式下不接受 FORCE=1，必须人工输入 yes）"
}
assert_target_not_production "$TARGET_DB" "$SOURCE_DB"

[ -f "$BACKUP_FILE" ] || fail "备份文件不存在: $BACKUP_FILE"
command -v docker >/dev/null 2>&1 || fail "docker 未安装"
docker ps --format '{{.Names}}' | grep -qx "$PG_CONTAINER" || fail "PG 容器 '$PG_CONTAINER' 未运行"

# ---------- 危险操作确认 ----------
warn "即将把备份恢复到数据库: ${TARGET_DB}（容器 ${PG_CONTAINER}）"
warn "若目标库已存在，其全部数据将被覆盖。"
if [ "${FORCE:-}" != "1" ]; then
  read -r -p "确认继续？输入 yes 继续: " ANS
  [ "$ANS" = "yes" ] || fail "已取消"
fi

# ---------- 重建目标库 ----------
info "重建数据库 ${TARGET_DB} ..."
docker exec -i "$PG_CONTAINER" psql -U "$POSTGRES_USER" -d postgres \
  -c "DROP DATABASE IF EXISTS \"${TARGET_DB}\";" \
  -c "CREATE DATABASE \"${TARGET_DB}\" OWNER \"${POSTGRES_USER}\";" \
  || fail "重建数据库失败"

# ---------- 拷贝备份进容器 ----------
# pg_restore 不支持从 stdin 并行恢复（-j 需要可 seek 的文件），故先拷入容器。
CONTAINER_DUMP="/tmp/imboy_restore_$$.dump"
info "拷贝备份进容器 ${PG_CONTAINER}:${CONTAINER_DUMP} ..."
docker cp "$BACKUP_FILE" "${PG_CONTAINER}:${CONTAINER_DUMP}" || fail "拷贝备份进容器失败"

# ---------- timescaledb 特殊处理（关键）----------
# imboy 的核心消息表（msg_c2c/msg_c2g/msg_store 等）是 timescaledb hypertable。
# 普通 pg_restore 无法恢复 chunk 的 dimension slices，会导致父表查询报
# "chunk ... has no dimension slices" 且 hypertable 数据全部丢失。
# 必须用 timescaledb_pre_restore()/post_restore() 包裹；该模式下禁止并行(-j)。
HAS_TSDB="$(docker exec -i "$PG_CONTAINER" pg_restore --list "$CONTAINER_DUMP" 2>/dev/null \
            | grep -c 'EXTENSION - timescaledb' || true)"
RESTORE_JOBS=(-j 4)
if [ "${HAS_TSDB:-0}" -gt 0 ]; then
  warn "检测到 timescaledb，启用 pre_restore 模式（串行恢复，禁用 -j）"
  docker exec -i "$PG_CONTAINER" psql -U "$POSTGRES_USER" -d "$TARGET_DB" -v ON_ERROR_STOP=1 \
    -c "CREATE EXTENSION IF NOT EXISTS timescaledb;" \
    -c "SELECT timescaledb_pre_restore();" || fail "timescaledb_pre_restore 失败"
  RESTORE_JOBS=()
fi

# ---------- 恢复 ----------
info "恢复中（pg_restore ${RESTORE_JOBS[*]:-串行}）..."
if docker exec -i "$PG_CONTAINER" \
     pg_restore -U "$POSTGRES_USER" -d "$TARGET_DB" --no-owner --no-privileges "${RESTORE_JOBS[@]+"${RESTORE_JOBS[@]}"}" \
     "$CONTAINER_DUMP"; then
  info "pg_restore 完成"
else
  warn "pg_restore 返回非零（自定义格式恢复常有可忽略的告警，继续校验）"
fi

# ---------- timescaledb 退出恢复模式 ----------
if [ "${HAS_TSDB:-0}" -gt 0 ]; then
  docker exec -i "$PG_CONTAINER" psql -U "$POSTGRES_USER" -d "$TARGET_DB" \
    -c "SELECT timescaledb_post_restore();" || warn "timescaledb_post_restore 返回非零"
fi

docker exec -i "$PG_CONTAINER" rm -f "$CONTAINER_DUMP" 2>/dev/null || true

# ---------- 恢复后抽样校验 ----------
info "抽样校验恢复结果（各表行数 Top 10）..."
docker exec -i "$PG_CONTAINER" psql -U "$POSTGRES_USER" -d "$TARGET_DB" -tAc "
  SELECT relname || ': ' || n_live_tup
  FROM pg_stat_user_tables
  ORDER BY n_live_tup DESC
  LIMIT 10;" || fail "校验查询失败 —— 恢复可能不完整"

TABLE_COUNT="$(docker exec -i "$PG_CONTAINER" psql -U "$POSTGRES_USER" -d "$TARGET_DB" -tAc \
  "SELECT count(*) FROM information_schema.tables WHERE table_schema='public';")"
[ "${TABLE_COUNT:-0}" -gt 0 ] || fail "恢复后 public schema 无任何表 —— 恢复失败"

info "恢复完成：${TARGET_DB} 共 ${TABLE_COUNT} 张表。请人工核对上面的行数是否符合预期。"
