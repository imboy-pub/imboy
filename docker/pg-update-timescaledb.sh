#!/bin/sh

set -e

# Perform all actions as $POSTGRES_USER
export PGUSER="$POSTGRES_USER"

# 确保TIMESCALEDB_VERSION环境变量被设置
if [ -z "$TIMESCALEDB_VERSION" ]; then
  echo "The TIMESCALEDB_VERSION environment variable is not set."
  exit 1
fi

# 确保POSTGRES_DB环境变量被设置
if [ -z "$POSTGRES_DB" ]; then
  echo "The POSTGRES_DB environment variable is not set."
  exit 1
fi

# 判断 timescaledb 是否存在，如果存在就移除它
psql --dbname="$POSTGRES_DB" -c "
DO \$\$
BEGIN
    IF EXISTS (
        SELECT FROM pg_catalog.pg_extension
        WHERE extname = 'timescaledb'
    ) THEN
        RAISE NOTICE 'TimescaleDB extension found. Dropping...';
        EXECUTE 'DROP EXTENSION timescaledb CASCADE';
    ELSE
        RAISE NOTICE 'TimescaleDB extension not found. No action taken.';
    END IF;
END \$\$;
"

# 直接对POSTGRES_DB数据库执行操作
echo "Updating timescaledb extension for database '$POSTGRES_DB' to version $TIMESCALEDB_VERSION"
psql --dbname="$POSTGRES_DB" -c "
    -- Install timescaledb
    CREATE EXTENSION IF NOT EXISTS timescaledb VERSION '$TIMESCALEDB_VERSION';
"

echo "timescaledb extension updated successfully for database '$POSTGRES_DB'."
