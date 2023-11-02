#!/bin/bash

set -xe

# Perform all actions as $POSTGRES_USER
export PGUSER="$POSTGRES_USER"

echo "shared_preload_libraries = 'pg_jieba'" >> /var/lib/postgresql/data/postgresql.conf
echo "shared_preload_libraries = 'timescaledb'" >> /var/lib/postgresql/data/postgresql.conf

pg_ctl restart

# Create the 'template_postgis' template db
"${psql[@]}" <<- 'EOSQL'
CREATE DATABASE template_postgis IS_TEMPLATE true;
EOSQL

# Load PostGIS into both template_database and $POSTGRES_DB
for DB in template_postgis "$POSTGRES_DB"; do
    echo "Loading PostGIS extensions into $DB"
    "${psql[@]}" --dbname="$DB" <<-'EOSQL'
        CREATE EXTENSION IF NOT EXISTS postgis;
        CREATE EXTENSION IF NOT EXISTS postgis_topology;
        CREATE EXTENSION IF NOT EXISTS fuzzystrmatch;
        CREATE EXTENSION IF NOT EXISTS postgis_tiger_geocoder;
        CREATE EXTENSION IF NOT EXISTS pg_trgm;
        CREATE EXTENSION IF NOT EXISTS pgroonga;
        CREATE EXTENSION IF NOT EXISTS pg_jieba;
        CREATE EXTENSION IF NOT EXISTS timescaledb;
        CREATE EXTENSION IF NOT EXISTS pg_stat_statements;
        CREATE EXTENSION IF NOT EXISTS pgcrypto;
        CREATE EXTENSION IF NOT EXISTS vector;
        CREATE EXTENSION IF NOT EXISTS roaringbitmap;
EOSQL
done
