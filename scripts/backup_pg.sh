#!/usr/bin/env bash
#
# IMBoy PostgreSQL 备份脚本
# 用法: bash backup_pg.sh [--full|--schema-only]
#
# 环境变量（可通过 .env 或 export 设置）:
#   PGHOST       - 数据库主机（默认 127.0.0.1）
#   PGPORT       - 数据库端口（默认 5432）
#   PGDATABASE   - 数据库名（默认 imboy）
#   PGUSER       - 数据库用户（默认 imboy）
#   PGPASSWORD   - 数据库密码（必须设置）
#   BACKUP_DIR   - 备份存储目录（默认 /var/backups/imboy/pg）
#   BACKUP_RETAIN_DAYS - 备份保留天数（默认 30）
#
# cron 示例（每日 03:00 执行全量备份）:
#   0 3 * * * /path/to/imboy/scripts/backup_pg.sh --full >> /var/log/imboy_backup.log 2>&1
#

set -euo pipefail

# ===== 配置 =====
PGHOST="${PGHOST:-127.0.0.1}"
PGPORT="${PGPORT:-5432}"
PGDATABASE="${PGDATABASE:-imboy}"
PGUSER="${PGUSER:-imboy}"
BACKUP_DIR="${BACKUP_DIR:-/var/backups/imboy/pg}"
BACKUP_RETAIN_DAYS="${BACKUP_RETAIN_DAYS:-30}"

TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
BACKUP_MODE="${1:---full}"

# ===== 预检 =====
if [ -z "${PGPASSWORD:-}" ]; then
    echo "[ERROR] PGPASSWORD 未设置，请 export PGPASSWORD 或在 .env 中配置"
    exit 1
fi

for cmd in pg_dump pg_restore; do
    if ! command -v "$cmd" &>/dev/null; then
        echo "[ERROR] 未找到 $cmd，请确认 PostgreSQL 客户端工具已安装"
        exit 1
    fi
done

mkdir -p "$BACKUP_DIR"

# ===== 备份 =====
echo "[INFO] $(date '+%Y-%m-%d %H:%M:%S') 开始备份 (mode=$BACKUP_MODE)"
echo "[INFO] 数据库: $PGUSER@$PGHOST:$PGPORT/$PGDATABASE"

case "$BACKUP_MODE" in
    --full)
        BACKUP_FILE="$BACKUP_DIR/${PGDATABASE}_full_${TIMESTAMP}.dump"
        pg_dump \
            -h "$PGHOST" \
            -p "$PGPORT" \
            -U "$PGUSER" \
            -d "$PGDATABASE" \
            -Fc \
            --no-owner \
            --no-privileges \
            -f "$BACKUP_FILE"
        ;;
    --schema-only)
        BACKUP_FILE="$BACKUP_DIR/${PGDATABASE}_schema_${TIMESTAMP}.sql"
        pg_dump \
            -h "$PGHOST" \
            -p "$PGPORT" \
            -U "$PGUSER" \
            -d "$PGDATABASE" \
            --schema-only \
            --no-owner \
            --no-privileges \
            -f "$BACKUP_FILE"
        ;;
    *)
        echo "[ERROR] 未知模式: $BACKUP_MODE (可选: --full, --schema-only)"
        exit 1
        ;;
esac

BACKUP_SIZE=$(du -h "$BACKUP_FILE" | cut -f1)
echo "[INFO] 备份文件: $BACKUP_FILE ($BACKUP_SIZE)"

# ===== 完整性验证 =====
if [ "$BACKUP_MODE" = "--full" ]; then
    echo "[INFO] 验证备份完整性..."
    TABLE_COUNT=$(pg_restore --list "$BACKUP_FILE" 2>/dev/null | grep -c "TABLE " || true)
    if [ "$TABLE_COUNT" -gt 0 ]; then
        echo "[OK] 备份验证通过，包含 $TABLE_COUNT 个表定义"
    else
        echo "[WARN] 备份中未发现表定义，请手动检查"
    fi
fi

# ===== 清理旧备份 =====
DELETED_COUNT=$(find "$BACKUP_DIR" -name "${PGDATABASE}_*" -type f -mtime +"$BACKUP_RETAIN_DAYS" -print -delete | wc -l | tr -d ' ')
if [ "$DELETED_COUNT" -gt 0 ]; then
    echo "[INFO] 已清理 $DELETED_COUNT 个超过 ${BACKUP_RETAIN_DAYS} 天的旧备份"
fi

echo "[INFO] $(date '+%Y-%m-%d %H:%M:%S') 备份完成"

# ===== 恢复指南 =====
# 全量恢复:
#   pg_restore -h $PGHOST -p $PGPORT -U $PGUSER -d $PGDATABASE \
#     --clean --if-exists --no-owner --no-privileges $BACKUP_FILE
#
# 恢复到新数据库:
#   createdb -h $PGHOST -p $PGPORT -U $PGUSER imboy_restored
#   pg_restore -h $PGHOST -p $PGPORT -U $PGUSER -d imboy_restored \
#     --no-owner --no-privileges $BACKUP_FILE
#
# 仅恢复特定表:
#   pg_restore -h $PGHOST -p $PGPORT -U $PGUSER -d $PGDATABASE \
#     --table=user --no-owner --no-privileges $BACKUP_FILE
#
# WAL 归档配置建议 (postgresql.conf):
#   wal_level = replica
#   archive_mode = on
#   archive_command = 'cp %p /var/backups/imboy/wal/%f'
#   archive_timeout = 300
#
# PITR (时间点恢复):
#   1. 停止 PostgreSQL
#   2. 清空 data 目录
#   3. 从全量备份恢复: pg_restore ...
#   4. 创建 recovery.signal 文件
#   5. 配置 restore_command = 'cp /var/backups/imboy/wal/%f %p'
#   6. 配置 recovery_target_time = '2026-04-08 12:00:00'
#   7. 启动 PostgreSQL
