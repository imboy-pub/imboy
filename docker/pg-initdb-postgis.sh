#!/bin/bash

set -xe

# Perform all actions as $POSTGRES_USER
export PGUSER="$POSTGRES_USER"

echo "shared_preload_libraries = 'pgcrypto, pg_jieba, timescaledb, vector, pg_stat_statements'" >> /var/lib/postgresql/data/postgresql.conf

pg_ctl restart

# Create the 'template_postgis' template db
"${psql[@]}" <<- 'EOSQL'
CREATE DATABASE template_postgis IS_TEMPLATE true;
EOSQL

# Load PostGIS into both template_database and $POSTGRES_DB
for DB in template_postgis "$POSTGRES_DB"; do
    echo "Loading PostGIS extensions into $DB"
    "${psql[@]}" --dbname="$DB" <<-'EOSQL'
        -- 提供如下空间信息服务功能：空间对象、空间索引、空间操作函数和空间操作符
        CREATE EXTENSION IF NOT EXISTS postgis;
        --PgRouting是基于开源空间数据库PostGIS用于网络分析的扩展模块，最初它被称作pgDijkstra，因为它只是利用Dijkstra算法实现最短路径搜索，之后慢慢添加了其他的路径分析算法，如A算法，双向A算法，Dijkstra算法，双向Dijkstra算法，tsp货郎担算法等，然后被更名为pgRouting
        -- CREATE EXTENSION IF NOT EXISTS pgrouting;
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
done
