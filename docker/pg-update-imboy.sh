#!/bin/bash
set -euo pipefail
set -x

# ---------- 环境变量 ----------
PGUSER="${POSTGRES_USER:-postgres}"
PGPASSWORD="${POSTGRES_PASSWORD:-}"
PGDATA="${PGDATA:-/var/lib/postgresql/18/docker}"
POSTGRES_DB="${POSTGRES_DB:-$PGUSER}"
POSTGIS_VERSION="${POSTGIS_VERSION%%+*}"

export PGUSER PGPASSWORD PGDATA
# ---------- 安装扩展 ----------
# 初始化器的 "$@" 包含当前脚本路径，不是额外数据库名。
DB="$POSTGRES_DB"
echo "Loading extensions into database: $DB"
psql -v ON_ERROR_STOP=1 --username "$PGUSER" --dbname "$DB" <<-EOSQL
        -- 提供如下空间信息服务功能：空间对象、空间索引、空间操作函数和空间操作符
        CREATE EXTENSION IF NOT EXISTS postgis;
        --PgRouting是基于开源空间数据库PostGIS用于网络分析的扩展模块，最初它被称作pgDijkstra，因为它只是利用Dijkstra算法实现最短路径搜索，之后慢慢添加了其他的路径分析算法，如A算法，双向A算法，Dijkstra算法，双向Dijkstra算法，tsp货郎担算法等，然后被更名为pgRouting
        CREATE EXTENSION IF NOT EXISTS pgrouting;
        -- gis 拓扑
        CREATE EXTENSION IF NOT EXISTS postgis_topology;
        -- 提供了几个函数来确定字符串之间的相似性和距离
        CREATE EXTENSION IF NOT EXISTS fuzzystrmatch;
        CREATE EXTENSION IF NOT EXISTS postgis_tiger_geocoder;
        CREATE EXTENSION IF NOT EXISTS address_standardizer;
        -- 中文分词
        CREATE EXTENSION IF NOT EXISTS pg_jieba;
        CREATE EXTENSION IF NOT EXISTS pg_trgm;
        -- 时序数据库
        CREATE EXTENSION IF NOT EXISTS timescaledb;
        --
        CREATE EXTENSION IF NOT EXISTS pg_stat_statements;
        CREATE EXTENSION IF NOT EXISTS pgcrypto;
        CREATE EXTENSION IF NOT EXISTS vector;
        CREATE EXTENSION IF NOT EXISTS roaringbitmap;
EOSQL

echo "Database initialization completed successfully."
