#!/usr/bin/env bash
# ============================================================
# 备份恢复冒烟 / Restore smoke test
# ------------------------------------------------------------
# 把最新一份 pg 备份恢复到「一次性临时库」，断言恢复结果非空，
# 然后删除临时库。用于证明备份文件真的可恢复，而不只是能被
# pg_restore --list 解析。
#
# 安全红线：临时库名由脚本自己生成且必须带 _smoke_ 前缀，
# 与生产库同名时直接拒绝执行。任何情况下不 DROP 生产库。
#
# 用法 / Usage:
#   bash scripts/restore_smoke.sh
#   BACKUP_DIR=/data/backups/pg bash scripts/restore_smoke.sh
#   DRY_RUN=1 bash scripts/restore_smoke.sh    # 只校验参数与守卫，不连库
# ============================================================
set -euo pipefail

PG_CONTAINER="${PG_CONTAINER:-imboy_pg18}"
POSTGRES_USER="${POSTGRES_USER:-imboy_user}"
POSTGRES_DB="${POSTGRES_DB:-imboy_pro}"
BACKUP_DIR="${BACKUP_DIR:-./data/backups/pg}"
DRY_RUN="${DRY_RUN:-0}"
# 恢复后至少应有多少张表才算通过
MIN_TABLES="${MIN_TABLES:-10}"

RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[1;33m'; NC='\033[0m'
info() { echo -e "${GREEN}[restore_smoke]${NC} $*"; }
warn() { echo -e "${YELLOW}[restore_smoke]${NC} $*"; }
fail() { echo -e "${RED}[restore_smoke] ERROR:${NC} $*" >&2; exit 1; }

# 临时库名：固定前缀 + 时间戳 + 进程号，保证唯一且可被守卫识别
SMOKE_DB="imboy_smoke_$(date -u +%Y%m%d%H%M%S)_$$"

# ---------- 安全守卫（在连任何库之前执行）----------
assert_safe_target() {
  local target="$1" production="$2"

  case "$target" in
    imboy_smoke_*) ;;
    *) fail "临时库名 '${target}' 不含 imboy_smoke_ 前缀，拒绝执行" ;;
  esac

  if [ "$target" = "$production" ]; then
    fail "临时库名与生产库 '${production}' 相同，拒绝执行"
  fi
}

assert_safe_target "$SMOKE_DB" "$POSTGRES_DB"
info "临时库名: ${SMOKE_DB}（生产库 ${POSTGRES_DB} 不受影响）"

if [ "$DRY_RUN" = "1" ]; then
  info "DRY_RUN=1：守卫校验通过，跳过实际恢复"
  exit 0
fi

# ---------- 找最新备份 ----------
command -v docker >/dev/null 2>&1 || fail "docker 未安装"
[ -d "$BACKUP_DIR" ] || fail "备份目录不存在: ${BACKUP_DIR}"

LATEST="$(find "$BACKUP_DIR" -name "${POSTGRES_DB}_*.dump" -type f -print0 2>/dev/null \
  | xargs -0 ls -t 2>/dev/null | head -1 || true)"
[ -n "$LATEST" ] || fail "在 ${BACKUP_DIR} 未找到 ${POSTGRES_DB}_*.dump 备份"
info "使用备份: ${LATEST}"

# ---------- 恢复到临时库；无论成败都删掉临时库 ----------
cleanup() {
  docker exec -i "$PG_CONTAINER" \
    psql -U "$POSTGRES_USER" -d postgres -c "DROP DATABASE IF EXISTS \"${SMOKE_DB}\";" >/dev/null 2>&1 || true
  info "已清理临时库 ${SMOKE_DB}"
}
trap cleanup EXIT

docker exec -i "$PG_CONTAINER" \
  psql -U "$POSTGRES_USER" -d postgres -c "CREATE DATABASE \"${SMOKE_DB}\";" >/dev/null \
  || fail "创建临时库失败"

# pg_restore 对扩展/属主类对象常有非致命告警，只要后续断言通过即算成功
docker exec -i "$PG_CONTAINER" \
  pg_restore -U "$POSTGRES_USER" -d "$SMOKE_DB" --no-owner --no-privileges < "$LATEST" \
  >/dev/null 2>&1 || warn "pg_restore 有非零退出（可能是扩展/属主告警），继续做数据断言"

# ---------- 断言：恢复出来的库确实有内容 ----------
TABLE_COUNT="$(docker exec -i "$PG_CONTAINER" \
  psql -U "$POSTGRES_USER" -d "$SMOKE_DB" -tAc \
  "SELECT count(*) FROM information_schema.tables WHERE table_schema='public';" 2>/dev/null | tr -d ' ')"

[ -n "$TABLE_COUNT" ] || fail "无法查询临时库表数量"
info "恢复出 ${TABLE_COUNT} 张表（阈值 ${MIN_TABLES}）"

if [ "$TABLE_COUNT" -lt "$MIN_TABLES" ]; then
  fail "恢复结果表数量 ${TABLE_COUNT} 低于阈值 ${MIN_TABLES}，备份可能不完整"
fi

info "恢复冒烟通过：备份可恢复且内容非空"
