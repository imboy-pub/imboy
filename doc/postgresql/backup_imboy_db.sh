#!/bin/bash

# mkdir -p /data /data/backup /data/backup/imboy_db
# chmod +x /usr/local/bin/backup_imboy_db.sh
# 0 2 * * * /bin/bash /usr/local/bin/backup_imboy_db.sh >> /var/log/backup_imboy_db.log 2>&1

# 备份配置
CONTAINER_NAME="imboy_pg17"  # 替换为您的PostgreSQL容器名称
DB_USER="imboy_user"            # 替换为您的数据库用户
DB_NAME="imboy_v1"                # 替换为您的数据库名称
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
EXCLUDE_TABLES=()

# 生成排除表数据的参数
EXCLUDE_DATA_STRING=""
for table in "${EXCLUDE_TABLES[@]}"; do
    EXCLUDE_DATA_STRING+="--exclude-table-data=$table "
done

# 执行备份（保留所有表结构，只排除指定表的数据）
echo "[$(date +'%Y-%m-%d %H:%M:%S')] Starting database backup..."
docker exec -e PGPASSWORD=$DB_PASSWORD $CONTAINER_NAME \
    pg_dump -U $DB_USER -h localhost \
    --schema-only \
    $EXCLUDE_DATA_STRING \
    $DB_NAME > $BACKUP_FILE

# 追加数据（排除指定表）
docker exec -e PGPASSWORD=$DB_PASSWORD $CONTAINER_NAME \
    pg_dump -U $DB_USER -h localhost \
    --data-only \
    $EXCLUDE_DATA_STRING \
    $DB_NAME >> $BACKUP_FILE

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
