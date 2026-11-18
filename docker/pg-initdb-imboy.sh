#!/bin/bash
set +x
set +v
set -euo pipefail

# ---------- 环境变量 ----------
PGUSER="${POSTGRES_USER:-postgres}"
PGPASSWORD="${POSTGRES_PASSWORD:-}"
PGDATA="${PGDATA:-/var/lib/postgresql/18/docker}"
POSTGRES_DB="${POSTGRES_DB:-$PGUSER}"
# 如果环境变量未设置，使用官方标准版本号
POSTGIS_VERSION="${POSTGIS_VERSION:-3.7.0+dfsg-1.pgdg13+2}"

export PGUSER PGPASSWORD PGDATA

# ---------- 配置 shared_preload_libraries ----------
echo "shared_preload_libraries = 'pgcrypto, pg_jieba, timescaledb, vector, pg_stat_statements'" >> "$PGDATA/postgresql.conf"


# 移除完整版本号的后缀部分，只保留主版本号（例如 3.6.1）
POSTGIS_VERSION="${POSTGIS_VERSION%%+*}"

# 本镜像基于官方 postgres（而不是 postgis/postgis），没有由上游预创建的
# template_postgis。向不存在的模板库执行 psql 会让首次初始化直接退出并触发
# 容器重启；只在实际创建的业务库安装扩展。
#
# docker-entrypoint-initdb.d 会把当前脚本路径作为位置参数传入；不能使用
# "$@"，否则会尝试连接名为该路径的数据库并导致首次初始化失败。
DB="$POSTGRES_DB"
echo "Updating PostGIS extensions '$DB' to $POSTGIS_VERSION"
psql -v ON_ERROR_STOP=1 --dbname="$DB" -c "
        -- Upgrade PostGIS (includes raster)
        CREATE EXTENSION IF NOT EXISTS postgis VERSION '$POSTGIS_VERSION';
        ALTER EXTENSION postgis UPDATE TO '$POSTGIS_VERSION';

        -- Upgrade Topology
        CREATE EXTENSION IF NOT EXISTS postgis_topology VERSION '$POSTGIS_VERSION';
        ALTER EXTENSION postgis_topology UPDATE TO '$POSTGIS_VERSION';

        -- 字符串相似度/距离函数（soundex、levenshtein 等，通用扩展；
        -- 原为 Tiger Geocoder 前置依赖，Tiger 移除后保留）
        CREATE EXTENSION IF NOT EXISTS fuzzystrmatch;
        -- PostGIS 3.7 移除了 postgis_tiger_geocoder（upstream NEWS #6052），
        -- pgdg 包不再提供该扩展，故不再安装。
"
