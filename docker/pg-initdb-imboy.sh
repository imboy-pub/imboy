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

# ---------- 配置 shared_preload_libraries ----------
echo "shared_preload_libraries = 'pgcrypto, pg_jieba, timescaledb, vector, pg_stat_statements'" >> "$PGDATA/postgresql.conf"


# Load PostGIS into both template_database and $POSTGRES_DB
for DB in template_postgis "$POSTGRES_DB" "${@}"; do
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
