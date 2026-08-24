#!/bin/bash
set -euo pipefail
set -x

# ---------- 环境变量 ----------
PGUSER="${POSTGRES_USER:-postgres}"
PGPASSWORD="${POSTGRES_PASSWORD:-}"
PGDATA="${PGDATA:-/var/lib/postgresql/18/docker}"
POSTGRES_DB="${POSTGRES_DB:-$PGUSER}"
# 如果环境变量未设置，使用官方标准版本号
POSTGIS_VERSION="${POSTGIS_VERSION:-3.6.1+dfsg-1.pgdg13+1}"

export PGUSER PGPASSWORD PGDATA

# ---------- 配置 shared_preload_libraries ----------
echo "shared_preload_libraries = 'pgcrypto, pg_jieba, timescaledb, vector, pg_stat_statements'" >> "$PGDATA/postgresql.conf"


# 移除完整版本号的后缀部分，只保留主版本号（例如 3.6.1）
POSTGIS_VERSION="${POSTGIS_VERSION%%+*}"

# 本镜像基于官方 postgres（而不是 postgis/postgis），没有由上游预创建的
# template_postgis。向不存在的模板库执行 psql 会让首次初始化直接退出并触发
# 容器重启；只在实际创建的业务库（以及调用方显式传入的库）安装扩展。
for DB in "$POSTGRES_DB" "${@}"; do
    echo "Updating PostGIS extensions '$DB' to $POSTGIS_VERSION"
    psql --dbname="$DB" -c "
        -- Upgrade PostGIS (includes raster)
        CREATE EXTENSION IF NOT EXISTS postgis VERSION '$POSTGIS_VERSION';
        ALTER EXTENSION postgis UPDATE TO '$POSTGIS_VERSION';

        -- Upgrade Topology
        CREATE EXTENSION IF NOT EXISTS postgis_topology VERSION '$POSTGIS_VERSION';
        ALTER EXTENSION postgis_topology UPDATE TO '$POSTGIS_VERSION';

        -- Install Tiger dependencies in case not already installed
        CREATE EXTENSION IF NOT EXISTS fuzzystrmatch;
        -- Upgrade US Tiger Geocoder
        CREATE EXTENSION IF NOT EXISTS postgis_tiger_geocoder VERSION '$POSTGIS_VERSION';
        ALTER EXTENSION postgis_tiger_geocoder UPDATE TO '$POSTGIS_VERSION';
    "
done
