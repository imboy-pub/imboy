#!/usr/bin/env bash
# ============================================================
# PostgreSQL 全量备份 / Full PostgreSQL backup
# ------------------------------------------------------------
# 被 docs/guides/operations/deployment/BACKUP-RESTORE.md 引用。
# 使用 pg_dump 自定义格式（-Fc，支持并行恢复+压缩）。
#
# 用法 / Usage:
#   bash script/backup_pg.sh                 # 用 .env / 环境变量（默认全量）
#   bash script/backup_pg.sh --full          # 全量备份（默认；与无参等价）
#   bash script/backup_pg.sh --schema-only   # 仅 schema（文档 BACKUP-RESTORE.md
#                                            # 承诺的"迁移前 schema 备份"模式）
#   BACKUP_DIR=/data/backups bash script/backup_pg.sh
#   cron 示例（每日 03:00 UTC）:
#     0 3 * * * cd /opt/imboy/deploy && bash ../script/backup_pg.sh >> /var/log/imboy-backup.log 2>&1
# ============================================================
set -euo pipefail

# ---------- 配置（环境变量优先，与 deploy/.env.example 对齐）----------
PG_CONTAINER="${PG_CONTAINER:-imboy_pg18}"          # docker compose 中的 PG 容器名
POSTGRES_USER="${POSTGRES_USER:-imboy_user}"
POSTGRES_DB="${POSTGRES_DB:-imboy_pro}"
BACKUP_DIR="${BACKUP_DIR:-./data/backups/pg}"
RETENTION_DAYS="${RETENTION_DAYS:-7}"               # 全量备份保留天数

# ---------- 彩色输出 ----------
RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[1;33m'; NC='\033[0m'
info()  { echo -e "${GREEN}[backup_pg]${NC} $*"; }
warn()  { echo -e "${YELLOW}[backup_pg]${NC} $*"; }
fail()  { echo -e "${RED}[backup_pg] ERROR:${NC} $*" >&2; exit 1; }

# ---------- 参数解析 ----------
# 文档（docs/guides/operations/deployment/BACKUP-RESTORE.md）写的是
# `backup_pg.sh --full` / `backup_pg.sh --schema-only`；此前脚本不解析参数，
# `--schema-only` 被静默忽略、实际产出全量——按文档操作的运维会拿到语义
# 错位的产物。这里把两个 flag 补齐；无参保持全量（cron 向后兼容）。
MODE="full"
while [ $# -gt 0 ]; do
  case "$1" in
    --full)        MODE="full"; shift ;;
    --schema-only) MODE="schema-only"; shift ;;
    *) fail "未知参数: $1（支持 --full / --schema-only）" ;;
  esac
done

# ---------- 指标推送（闭合 IMBoyBackupNotRunning 告警的产出方）----------
# shellcheck source=scripts/lib/metrics_push.sh
. "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/metrics_push.sh"
# ---------- B-29 加密 + 异地副本 ----------
# shellcheck source=scripts/lib/backup_offsite.sh
. "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/backup_offsite.sh"

START_TS="$(date -u +%s)"
BACKUP_OK=0
OUT=""
# 无论从哪条路径退出（含 fail() 的 exit 1）都上报一次结果，
# 避免失败静默 —— 静默失败正是 absent() 告警要防的场景。
trap 'push_backup_result pg "$BACKUP_OK" "$START_TS" "$OUT"' EXIT

# ---------- 前置检查 ----------
command -v docker >/dev/null 2>&1 || fail "docker 未安装"
docker ps --format '{{.Names}}' | grep -qx "$PG_CONTAINER" \
  || fail "PG 容器 '$PG_CONTAINER' 未运行（设置 PG_CONTAINER 覆盖）"

mkdir -p "$BACKUP_DIR"
# 时间戳由 shell 生成（脚本运行时）
TS="$(date -u +%Y%m%dT%H%M%SZ)"
# schema-only 产物用独立命名（<db>_schema_<ts>.dump）：
#   1) 一眼可分辨；2) 下游（restore_smoke.sh / retention 清理）按
#      `${POSTGRES_DB}_2*.dump` glob 只认全量——schema 备份按文档要永久保留，
#      不参与 7 天轮换，也不能被每日恢复演练当全量用。
DUMP_EXTRA_ARGS=()
case "$MODE" in
  full)         OUT="${BACKUP_DIR}/${POSTGRES_DB}_${TS}.dump" ;;
  schema-only)  OUT="${BACKUP_DIR}/${POSTGRES_DB}_schema_${TS}.dump"
                DUMP_EXTRA_ARGS=(--schema-only) ;;
esac

# ---------- 执行备份 ----------
info "开始备份 mode=${MODE} db=${POSTGRES_DB} → ${OUT}"
if docker exec -i "$PG_CONTAINER" \
      pg_dump -U "$POSTGRES_USER" -d "$POSTGRES_DB" -Fc --no-owner --no-privileges \
      "${DUMP_EXTRA_ARGS[@]+"${DUMP_EXTRA_ARGS[@]}"}" \
      > "$OUT"; then
  SIZE="$(du -h "$OUT" | cut -f1)"
  info "备份完成: ${OUT} (${SIZE})"
else
  rm -f "$OUT"
  fail "pg_dump 失败，已删除不完整备份文件"
fi

# ---------- 完整性校验（pg_restore --list 能解析 = 备份有效）----------
if docker exec -i "$PG_CONTAINER" pg_restore --list < "$OUT" >/dev/null 2>&1; then
  info "完整性校验通过（pg_restore --list 可解析）"
else
  fail "完整性校验失败：备份文件无法被 pg_restore 解析"
fi

# ---------- WAL 归档检查（提示）----------
if docker exec -i "$PG_CONTAINER" psql -U "$POSTGRES_USER" -d "$POSTGRES_DB" -tAc \
     "SELECT CASE WHEN current_setting('archive_mode')='on' THEN 'on' ELSE 'off' END;" 2>/dev/null \
     | grep -qx on; then
  info "WAL archive_mode=on（支持 PITR）"
else
  warn "WAL archive_mode=off：仅有全量备份，无法做时间点恢复（PITR）。生产建议开启。"
fi

# ---------- B-29：加密 + 异地副本 ----------
# 顺序刻意是「先校验完整性 → 再加密 → 再推异地」：
#   - 校验必须在明文上做（pg_restore --list 读不了密文）
#   - 推上去的必须是密文（异地那份我们控制不了访问面）
# 两项都未配置时只警告不失败，但警告写得刺眼 —— 生产上"没有异地副本"
# 等于备份没做（本机故障即全部丢失），不能悄悄跳过。
# 本地保留**明文**：私钥按设计不在服务器上，加密本地那份会让每日恢复演练
# （restore_smoke.sh）解不开、做不成 —— "备份能恢复"必须每天被验证，
# 优先级高于"本地那份也加密"。加密只作用于离开本机的副本。
push_offsite "$OUT" || fail "异地推送失败：本地备份已生成但**没有异地副本**"

# ---------- 清理过期备份 ----------
# glob `${POSTGRES_DB}_2*.dump` 只匹配全量产物（时间戳以 2 开头），
# 不动 _schema_ 备份（文档口径：schema 备份永久保留）。
DELETED="$(find "$BACKUP_DIR" -name "${POSTGRES_DB}_2*.dump" -type f -mtime "+${RETENTION_DAYS}" -print -delete | wc -l | tr -d ' ')"
[ "$DELETED" -gt 0 ] && info "清理 ${DELETED} 个超过 ${RETENTION_DAYS} 天的旧备份"

BACKUP_OK=1
info "全部完成。最新备份: ${OUT}"
