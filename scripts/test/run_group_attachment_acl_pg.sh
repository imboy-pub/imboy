#!/usr/bin/env bash
# Run migration 108 and group-attachment ACL tests against a marker scratch DB.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
PGHOST="${PGHOST:-127.0.0.1}"
PGPORT="${PGPORT:-4323}"
PGUSER="${PGUSER:-imboy_user}"
PGPASSWORD="${PGPASSWORD:-abc54321}"
DB="imboy_ga_acl_$(date +%s)_$$"
readonly DB
DB_CREATED=0

case "$PGHOST" in
  127.0.0.1|::1) ;;
  *) echo "refusing non-loopback PostgreSQL host" >&2; exit 2 ;;
esac
case "$DB" in
  imboy_ga_acl_*) ;;
  *) echo "invalid marker database" >&2; exit 2 ;;
esac
if [[ -n "${PGHOSTADDR:-}" || -n "${PGSERVICE:-}" || -n "${PGSERVICEFILE:-}" ]]; then
  echo "refusing libpq connection target override" >&2
  exit 2
fi

export PGHOST PGPORT PGUSER PGPASSWORD

cleanup() {
  if [[ "$DB_CREATED" -eq 1 ]]; then
    psql -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" -d postgres \
      -v ON_ERROR_STOP=1 -q -c "DROP DATABASE IF EXISTS $DB" >/dev/null
  fi
}
trap cleanup EXIT

psql -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" -d postgres \
  -v ON_ERROR_STOP=1 -q -c "CREATE DATABASE $DB" >/dev/null
DB_CREATED=1
for extension in pg_jieba postgis postgis_raster timescaledb pgcrypto uuid-ossp pg_trgm \
  btree_gin btree_gist unaccent pg_stat_statements; do
  psql -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" -d "$DB" \
    -v ON_ERROR_STOP=1 -q -c "CREATE EXTENSION IF NOT EXISTS \"$extension\"" \
      >/dev/null 2>&1 || true
done

make -C "$ROOT" app >/dev/null
PGDATABASE="$DB" IMBOY_DIR="$ROOT" "$ROOT/scripts/drill_migrate.escript" up >/dev/null

IMBOY_GA_TEST_DB="$DB" \
IMBOY_GA_TEST_HOST="$PGHOST" \
IMBOY_GA_TEST_PORT="$PGPORT" \
IMBOY_GA_TEST_USER="$PGUSER" \
IMBOY_GA_TEST_PASSWORD="$PGPASSWORD" \
make -C "$ROOT" eunit t=group_attachment_acl_integration_tests

echo "group attachment migration/ACL PostgreSQL test: PASS"
