#!/usr/bin/env bash
# ============================================================
# Garage S3 附件数据备份 / Garage S3 attachment backup
# ------------------------------------------------------------
# 用 rclone（S3 兼容）把 Garage bucket 同步到备份目标。
# 被 docs/operations/deployment/BACKUP-RESTORE.md 引用。
#
# 前置 / Prereq:
#   1. 安装 rclone: https://rclone.org/install/
#   2. 配置一个指向 Garage 的 remote，例如 ~/.config/rclone/rclone.conf:
#        [garage]
#        type = s3
#        provider = Other
#        access_key_id = <GARAGE_ACCESS_KEY>
#        secret_access_key = <GARAGE_SECRET_KEY>
#        endpoint = http://127.0.0.1:3900
#        region = garage
#   3. 配置备份目标 remote（另一台机器 / 云 S3 / 本地路径）
#
# 用法 / Usage:
#   bash script/backup_garage.sh                              # 用默认变量
#   GARAGE_REMOTE=garage BUCKET=imboy DEST=/data/backups/garage bash script/backup_garage.sh
#   DEST=offsite:imboy-backup bash script/backup_garage.sh    # 同步到异地 S3
# ============================================================
set -euo pipefail

GARAGE_REMOTE="${GARAGE_REMOTE:-garage}"       # rclone remote 名（指向 Garage）
BUCKET="${BUCKET:-imboy}"                       # Garage bucket 名
DEST="${DEST:-./data/backups/garage}"           # 备份目标（本地路径或 rclone remote）
RETENTION_DAYS="${RETENTION_DAYS:-30}"          # 仅对本地路径目标生效

RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[1;33m'; NC='\033[0m'
info() { echo -e "${GREEN}[backup_garage]${NC} $*"; }
warn() { echo -e "${YELLOW}[backup_garage]${NC} $*"; }
fail() { echo -e "${RED}[backup_garage] ERROR:${NC} $*" >&2; exit 1; }

command -v rclone >/dev/null 2>&1 || fail "rclone 未安装：https://rclone.org/install/"
rclone listremotes 2>/dev/null | grep -qx "${GARAGE_REMOTE}:" \
  || fail "rclone remote '${GARAGE_REMOTE}:' 未配置（见脚本头部说明）"

TS="$(date -u +%Y%m%dT%H%M%SZ)"

# 目标若是本地路径，则按时间戳归档；若是 remote，则增量同步到固定前缀
if [[ "$DEST" == *:* ]]; then
  TARGET="${DEST}/${BUCKET}"
  info "增量同步 ${GARAGE_REMOTE}:${BUCKET} → ${TARGET}（rclone sync）"
  rclone sync "${GARAGE_REMOTE}:${BUCKET}" "$TARGET" --progress --transfers 8 \
    || fail "rclone sync 失败"
else
  mkdir -p "$DEST"
  TARGET="${DEST}/${BUCKET}_${TS}"
  info "全量复制 ${GARAGE_REMOTE}:${BUCKET} → ${TARGET}（rclone copy）"
  rclone copy "${GARAGE_REMOTE}:${BUCKET}" "$TARGET" --progress --transfers 8 \
    || fail "rclone copy 失败"
fi

# ---------- 校验：对比对象数量 ----------
SRC_COUNT="$(rclone size "${GARAGE_REMOTE}:${BUCKET}" --json 2>/dev/null | grep -o '"count":[0-9]*' | head -1 | cut -d: -f2 || echo '?')"
info "源 bucket 对象数: ${SRC_COUNT}"

# ---------- 清理过期（仅本地路径）----------
if [[ "$DEST" != *:* ]]; then
  DELETED="$(find "$DEST" -maxdepth 1 -name "${BUCKET}_*" -type d -mtime "+${RETENTION_DAYS}" -print -exec rm -rf {} + 2>/dev/null | wc -l | tr -d ' ' || echo 0)"
  [ "${DELETED:-0}" -gt 0 ] && info "清理 ${DELETED} 个超过 ${RETENTION_DAYS} 天的旧备份"
fi

info "Garage 备份完成: ${TARGET}"
warn "注意：仅备份 Garage 中的附件对象。数据库（含附件元数据/object_key）请用 backup_pg.sh 一并备份，二者需时间一致。"
