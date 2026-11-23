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
#
# 顺序（L-03 演练实证 2026-11-21，对齐 TimescaleDB 官方 dump/restore 指南）：
#   CREATE EXTENSION → 恢复 pre-data（表结构）→ timescaledb_pre_restore()
#   → 恢复 data → 恢复 post-data → timescaledb_post_restore()
# 实证偏差后果（勿改序）：
#   * pre_restore 放在 pre-data **之前**：catalog metadata COPY 撞
#     duplicate key（seed 行未被 pre-data 阶段重置，data 段装载重复）；
#   * post-data 放在 post_restore **之后**：hypertable 上的
#     ALTER TABLE ... ONLY 报 "ONLY option not supported on hypertable"。
RESTORE_LIST="$(docker exec -i "$PG_CONTAINER" pg_restore --list "$CONTAINER_DUMP")" \
  || fail "备份目录无法解析，恢复失败"
HAS_TSDB="$(printf '%s\n' "$RESTORE_LIST" | grep -c 'EXTENSION - timescaledb' || true)"
RESTORE_JOBS=(-j 4)
TSDB_PRE_RESTORE=""
if [ "${HAS_TSDB:-0}" -gt 0 ]; then
  warn "检测到 timescaledb，启用 pre_restore 模式（串行恢复，禁用 -j）"
  docker exec -i "$PG_CONTAINER" psql -U "$POSTGRES_USER" -d "$TARGET_DB" -v ON_ERROR_STOP=1 \
    -c "CREATE EXTENSION IF NOT EXISTS timescaledb;" || fail "创建 timescaledb 扩展失败"
  TSDB_PRE_RESTORE=1
  RESTORE_JOBS=()
fi

# ---------- 恢复（分段：pre-data / data / post-data）----------
# 分段原因（L-03 演练实证 2026-11-21）：
#   1. timescaledb 要求 pre_restore 夹在 pre-data 与 data 之间（见上），
#      一次性 pg_restore 做不到；
#   2. FK 约束在 post-data 段以 ALTER TABLE ... ADD CONSTRAINT 重建并
#      **重新校验存量行**。源库自身已存在的违约行（如 attach_direct_attempt
#      对 attach_pending 的孤儿行，FK convalidated=t 却被后续写路径绕过）
#      会让恢复失败。分段后：
#      * pre-data / data 段严格失败（结构或数据装载问题 = 恢复失败）；
#      * post-data 段失败时，提取**具体失败约束名**，以 NOT VALID 补挂
#        —— 如实告警"源库自带违约行"，绝不静默丢约束，也不让单个
#        预存数据缺陷卡死整个灾难恢复路径。
RESTORE_FAILED=0

info "恢复 pre-data 段（表结构）..."
if ! docker exec -i "$PG_CONTAINER" \
     pg_restore -U "$POSTGRES_USER" -d "$TARGET_DB" --no-owner --no-privileges \
     --section=pre-data "${RESTORE_JOBS[@]+"${RESTORE_JOBS[@]}"}" \
     "$CONTAINER_DUMP"; then
  RESTORE_FAILED=1
  warn "pre-data 段失败（表结构）"
fi

if [ "$RESTORE_FAILED" -eq 0 ] && [ -n "$TSDB_PRE_RESTORE" ]; then
  if docker exec -i "$PG_CONTAINER" psql -U "$POSTGRES_USER" -d "$TARGET_DB" -v ON_ERROR_STOP=1 \
       -c "SELECT timescaledb_pre_restore();"; then
    info "timescaledb_pre_restore 完成（pre-data 与 data 之间）"
  else
    RESTORE_FAILED=1
    warn "timescaledb_pre_restore 失败"
  fi
fi

if [ "$RESTORE_FAILED" -eq 0 ]; then
  # 注意：data 段**不要**用 --disable-triggers——它会 SET
  # session_replication_role=replica，绕过 timescaledb 的 catalog COPY
  # 拦截（replace 语义），导致 _timescaledb_catalog.metadata COPY 撞
  # duplicate key（L-03 演练实证）。FK 均在 post-data 段重建，
  # data 段无需禁触发器。
  info "恢复 data 段（数据装载）..."
  if ! docker exec -i "$PG_CONTAINER" \
       pg_restore -U "$POSTGRES_USER" -d "$TARGET_DB" --no-owner --no-privileges \
       --section=data "${RESTORE_JOBS[@]+"${RESTORE_JOBS[@]}"}" \
       "$CONTAINER_DUMP"; then
    RESTORE_FAILED=1
    warn "data 段失败（数据装载）"
  fi
fi

NOT_VALID_CONSTRAINTS=""
if [ "$RESTORE_FAILED" -eq 0 ]; then
  info "恢复 post-data 段（索引/约束）..."
  POST_ERR_FILE="$(mktemp)"
  if docker exec -i "$PG_CONTAINER" \
       pg_restore -U "$POSTGRES_USER" -d "$TARGET_DB" --no-owner --no-privileges \
       --section=post-data "${RESTORE_JOBS[@]+"${RESTORE_JOBS[@]}"}" \
       "$CONTAINER_DUMP" 2>"$POST_ERR_FILE"; then
    info "post-data 段完成"
  else
    # 提取校验失败的约束名（violates ... constraint "<name>"），
    # 把这些约束改以 NOT VALID 补挂，其余 post-data 项 pg_restore 已继续完成。
    FAILED_CONSTRAINTS="$(grep -oE 'violates [a-z ]*constraint "[^"]+"' "$POST_ERR_FILE" \
      | sed -E 's/.*"([^"]+)"$/\1/' | sort -u || true)"
    if [ -n "$FAILED_CONSTRAINTS" ]; then
      POST_SQL_FILE="$(mktemp)"
      docker exec -i "$PG_CONTAINER" pg_restore -f - --section=post-data "$CONTAINER_DUMP" \
        > "$POST_SQL_FILE" 2>/dev/null || true
      for CNAME in $FAILED_CONSTRAINTS; do
        # post-data 的 ALTER TABLE ... ADD CONSTRAINT 是多行语句：从
        # 匹配行回溯最近的 ALTER TABLE 行，拼到分号为止，再整句补挂。
        ADD_SQL="$(awk -v pat="ADD CONSTRAINT ${CNAME} " '
          !instr && index($0, pat) { instr = NR }
          { lines[NR] = $0 }
          END {
            if (instr) {
              start = instr - 1
              if (start < 1 || lines[start] !~ /^ALTER TABLE/) start = instr
              out = lines[start]
              for (i = start + 1; i <= NR; i++) {
                out = out " " lines[i]
                if (lines[i] ~ /;[[:space:]]*$/) break
              }
              sub(/;[[:space:]]*$/, "", out)
              print out
            }
          }' "$POST_SQL_FILE")"
        if [ -n "$ADD_SQL" ]; then
          if docker exec -i "$PG_CONTAINER" psql -U "$POSTGRES_USER" -d "$TARGET_DB" \
               -v ON_ERROR_STOP=1 -c "${ADD_SQL} NOT VALID;"; then
            NOT_VALID_CONSTRAINTS="${NOT_VALID_CONSTRAINTS} ${CNAME}"
            warn "⚠️  约束 ${CNAME} 以 NOT VALID 补挂：源库存在违约行（源数据缺陷，非恢复丢失），请核对该约束覆盖的孤儿数据。"
          else
            RESTORE_FAILED=1
            warn "约束 ${CNAME} 补挂失败"
          fi
        else
          RESTORE_FAILED=1
          warn "post-data 段失败且无法定位约束 ${CNAME} 的 DDL"
        fi
      done
      rm -f "$POST_SQL_FILE"
    else
      RESTORE_FAILED=1
      warn "post-data 段失败（非约束校验类错误，详见上方 pg_restore 输出）"
      cat "$POST_ERR_FILE" >&2 || true
    fi
  fi
  rm -f "$POST_ERR_FILE"
fi

# ---------- timescaledb 退出恢复模式 ----------
if [ "${HAS_TSDB:-0}" -gt 0 ]; then
  if ! docker exec -i "$PG_CONTAINER" psql -U "$POSTGRES_USER" -d "$TARGET_DB" -v ON_ERROR_STOP=1 \
    -c "SELECT timescaledb_post_restore();"; then
    warn "timescaledb_post_restore 失败"
    RESTORE_FAILED=1
  fi
fi

docker exec -i "$PG_CONTAINER" rm -f "$CONTAINER_DUMP" 2>/dev/null || true
[ "$RESTORE_FAILED" -eq 0 ] || fail "恢复未完成，请调查恢复日志；禁止使用不完整目标库"

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

# ---------- L-03 恢复后防复活提示（如实声明；本脚本不做重放）----------
# 备份是时点快照：备份时点之后已删除的数据、已撤销的权限（设备绑定/
# 推送令牌/会话）会随恢复回到目标库——这是备份语义，不是缺陷。
# 开放访问前必须先重放删除编排/墓碑（D-03 user_deletion 编排）并
# 核对撤权不复活；retention-policy.yml 声明 legal_hold: unsupported，
# 不存在 Legal Hold 过滤兜底。对合成 QA 数据的恢复演练 ≠ 生产恢复验收。
warn "⚠️  备份是时点快照：备份点之后已删除的数据/已撤销的权限会随恢复回到本库。"
warn "开放访问前必须先重放删除编排（墓碑）并核对撤权不复活；本脚本不做重放。"
warn "Legal Hold 不支持（retention-policy.yml: legal_hold unsupported）；合成演练 ≠ 生产恢复验收。"
info "L03_RESTORE_NOTE replay_required_before_open=yes legal_hold=unsupported drill_ne_production=yes${NOT_VALID_CONSTRAINTS:+ not_valid_constraints=${NOT_VALID_CONSTRAINTS}}"

info "恢复完成：${TARGET_DB} 共 ${TABLE_COUNT} 张表。请人工核对上面的行数是否符合预期。${NOT_VALID_CONSTRAINTS:+ ⚠️ 以下约束以 NOT VALID 补挂（源库自带违约行）：${NOT_VALID_CONSTRAINTS}}"
