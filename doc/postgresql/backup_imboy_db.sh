#!/bin/bash
# mkdir -p /data /data/backup /data/backup/imboy_db
# chmod +x /usr/local/bin/backup_imboy_db.sh
# bash /usr/local/bin/backup_imboy_db.sh dataonly
# 0 2 * * * /bin/bash /usr/local/bin/backup_imboy_db.sh >> /var/log/backup_imboy_db.log 2>&1

set -euo pipefail
set -x

# ---------- 参数 ----------
TYPE="${1:-full}"  # 默认 full, 可传 dataonly 只备份数据

# ---------- 备份配置 ----------
CONTAINER_NAME="imboy_pg18" # PostgreSQL 容器名称
DB_USER="imboy_user"             # 数据库用户
DB_NAME="imboy_v1"               # 数据库名称
BACKUP_DIR="/data/backup/imboy_db"
PASSWORD_FILE="/etc/imboy_db_password"
DATE=$(date +%Y%m%d_%H%M%S)
BACKUP_FILE="$BACKUP_DIR/imboy_backup_$DATE.sql"
COMPRESSED_FILE="$BACKUP_DIR/imboy_backup_$DATE.tar.gz"
KEEP=10

# 读取密码
DB_PASSWORD=$(cat $PASSWORD_FILE)

# 创建备份目录
mkdir -p $BACKUP_DIR

# 排除表数据但保留结构的表列表
EXCLUDE_TABLES=("database_migrations_history" "fts_user" "verification_code" "msg_c2c" "msg_c2g" "msg_c2g_timeline" "msg_c2s" "msg_s2c")
EXCLUDE_DATA_STRING=""
for table in "${EXCLUDE_TABLES[@]}"; do
    EXCLUDE_DATA_STRING+="--exclude-table-data=$table "
done

echo "[$(date +'%Y-%m-%d %H:%M:%S')] Starting database backup..."

if [ "$TYPE" == "dataonly" ]; then
    # 只备份数据 - 仅备份 public 模式
    docker exec -e PGPASSWORD=$DB_PASSWORD $CONTAINER_NAME \
        pg_dump -U $DB_USER -h localhost --data-only \
        --schema=public \
        $EXCLUDE_DATA_STRING \
        $DB_NAME > $BACKUP_FILE
else
    # 备份结构 + 数据 - 仅备份 public 模式
    docker exec -e PGPASSWORD=$DB_PASSWORD $CONTAINER_NAME \
        pg_dump -U $DB_USER -h localhost --schema-only \
        --schema=public \
        $EXCLUDE_DATA_STRING \
        $DB_NAME > $BACKUP_FILE

    docker exec -e PGPASSWORD=$DB_PASSWORD $CONTAINER_NAME \
        pg_dump -U $DB_USER -h localhost --data-only \
        --schema=public \
        $EXCLUDE_DATA_STRING \
        $DB_NAME >> $BACKUP_FILE
fi

if [ $? -eq 0 ]; then
    echo "[$(date +'%Y-%m-%d %H:%M:%S')] Database backup completed successfully."

    # 压缩备份文件
    echo "[$(date +'%Y-%m-%d %H:%M:%S')] Compressing backup file..."
    tar -czvf $COMPRESSED_FILE -C $BACKUP_DIR $(basename $BACKUP_FILE)
    rm -f $BACKUP_FILE

    # 删除旧的备份文件，只保留最新的10个
    echo "[$(date +'%Y-%m-%d %H:%M:%S')] Cleaning up old backups..."
    ls -t $BACKUP_DIR/imboy_backup_*.tar.gz | tail -n +$(($KEEP+1)) | xargs rm -f

    echo "[$(date +'%Y-%m-%d %H:%M:%S')] Backup process completed. File: $COMPRESSED_FILE"
else
    echo "[$(date +'%Y-%m-%d %H:%M:%S')] Error: Database backup failed."
    rm -f $BACKUP_FILE
    exit 1
fi
