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

# ---------- B-22：演练结果推 Pushgateway（供 Alertmanager 消费）----------
# 没有这一步，演练失败只会安静地留在 cron 日志里 —— 没有人会去看，
# 等于"备份不可恢复"这件事永远不会有人被通知到。
START_TS="$(date -u +%s)"
DRILL_ROWS=0
# shellcheck source=scripts/lib/metrics_push.sh
. "$(dirname "$0")/lib/metrics_push.sh"

# 无论从哪条路径退出都推一次结果：exit 0 记成功，非 0 记失败。
# 挂在 EXIT 上而不是散在各个 fail 点 —— 散着写迟早漏掉一条分支。
report_drill() {
  local rc=$?
  if [ "$rc" -eq 0 ]; then
    push_restore_result 1 "$START_TS" "$DRILL_ROWS" || true
  else
    push_restore_result 0 "$START_TS" "$DRILL_ROWS" || true
  fi
  return $rc
}

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
  # ⚠️ DRY_RUN **不推成功指标**：它没有真的恢复过任何东西。
  # 若这里推 1，cron 里误设 DRY_RUN=1 会让"备份可恢复"永远显示为绿。
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
trap 'cleanup; report_drill' EXIT

# ---------- B-21：演练必须走与真实恢复**同一份代码** ----------
# 此前这里是就地一句 `pg_restore ... < $LATEST`，而真实恢复
# （scripts/restore_pg.sh）额外包了 timescaledb_pre_restore()/post_restore()。
# imboy 的核心消息表是 hypertable，缺这层包裹时 chunk 的 dimension slices 恢复不出来
# —— **表还在、数据没了**。于是每日演练稳稳变绿，真实灾难恢复丢掉全部消息。
#
# 修法不是"把那段包裹也抄一份到这里"：重复正是这个 bug 的成因，抄完照样会再次漂移。
# 直接委托给 restore_pg.sh，**只留一份恢复实现**，"同路径"就变成结构上成立而不是
# 靠人记得同步。
#
# 传参说明：
#   FORCE=1  跳过交互确认（cron 里没有 tty）。目标是 imboy_smoke_* 不是生产库，
#            B-28 的生产库守卫会拦住任何写错成生产库的情况，且那条路径下
#            FORCE=1 会被显式拒绝 —— 这里用 FORCE 是安全的。
#   --target 临时库；restore_pg.sh 自己会 DROP+CREATE，不必先建。
info "委托 scripts/restore_pg.sh 执行恢复（与真实灾难恢复同一代码路径）"
RESTORE_LOG="$(mktemp)"
trap 'rm -f "$RESTORE_LOG"; cleanup; report_drill' EXIT

if FORCE=1 PG_CONTAINER="$PG_CONTAINER" POSTGRES_USER="$POSTGRES_USER" POSTGRES_DB="$POSTGRES_DB" \
     bash "$(dirname "$0")/restore_pg.sh" "$LATEST" --target "$SMOKE_DB" >"$RESTORE_LOG" 2>&1; then
  info "restore_pg.sh 返回成功"
else
  warn "restore_pg.sh 返回非零（自定义格式恢复常有可忽略告警），继续做数据断言"
  tail -20 "$RESTORE_LOG" >&2 || true
fi

# ---------- 断言 1：表结构恢复出来了 ----------
TABLE_COUNT="$(docker exec -i "$PG_CONTAINER" \
  psql -U "$POSTGRES_USER" -d "$SMOKE_DB" -tAc \
  "SELECT count(*) FROM information_schema.tables WHERE table_schema='public';" 2>/dev/null | tr -d ' ')"

[ -n "$TABLE_COUNT" ] || fail "无法查询临时库表数量"
info "恢复出 ${TABLE_COUNT} 张表（阈值 ${MIN_TABLES}）"

if [ "$TABLE_COUNT" -lt "$MIN_TABLES" ]; then
  fail "恢复结果表数量 ${TABLE_COUNT} 低于阈值 ${MIN_TABLES}，备份可能不完整"
fi

# ---------- 断言 2（B-22）：hypertable 的**数据**也恢复出来了 ----------
# 只数表数量是抓不到 timescaledb 那个坑的：缺 pre/post_restore 包裹时
# **表照样都在、只是全空**，表数量断言稳稳通过。必须数行。
#
# 判据写的是"消息表行数 > 0"，但直接断言 >0 会误报：msg_c2c 是投递队列，
# 全部设备 ACK 后行会被删，一个安静的小部署里它合法地就是 0 行。
# 改成**与源库对照**：源库有数据而恢复出来是 0 行，才是真的丢了。
# 空库演练因此不会假失败，而 timescaledb 丢数据一定被抓到。
row_count() {  # row_count <db> <table>
  docker exec -i "$PG_CONTAINER" psql -U "$POSTGRES_USER" -d "$1" -tAc \
    "SELECT count(*) FROM public.\"$2\";" 2>/dev/null | tr -d ' \r'
}

MSG_TABLES="${MSG_TABLES:-msg_c2c msg_c2g msg_store msg_s2c}"
CHECKED=0
DATA_OK=0
for TBL in $MSG_TABLES; do
  SRC="$(row_count "$POSTGRES_DB" "$TBL")"
  DST="$(row_count "$SMOKE_DB" "$TBL")"
  # 表不存在/查不到就跳过（不同版本表集合会变，不能因此判失败）
  case "$SRC$DST" in ''|*[!0-9]*) warn "跳过 ${TBL}（源或临时库查不到该表）"; continue ;; esac
  CHECKED=$((CHECKED + 1))
  info "  ${TBL}: 源库 ${SRC} 行 / 恢复 ${DST} 行"
  if [ "$SRC" -gt 0 ] && [ "$DST" -eq 0 ]; then
    fail "${TBL} 源库有 ${SRC} 行但恢复后 0 行 —— hypertable 数据未恢复（典型为缺少 timescaledb_pre_restore 包裹）"
  fi
  [ "$DST" -gt 0 ] && DATA_OK=1
  DRILL_ROWS=$((DRILL_ROWS + DST))
done

[ "$CHECKED" -gt 0 ] || warn "未能核对任何消息表行数（表名可能已变，请核对 MSG_TABLES）"
if [ "$DATA_OK" = "1" ]; then
  info "消息表数据核对通过（至少一张表恢复出非空数据）"
else
  warn "所有被核对的消息表在源库也是 0 行 —— 本次演练只证明了结构可恢复，未证明数据可恢复"
fi

info "恢复冒烟通过：备份可恢复且内容非空"
