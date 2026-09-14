#!/usr/bin/env bash
# Run C2G boundary, attachment/action/member-key ACL, and claim/archive tests against a marker scratch DB.
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

make -C "$ROOT" app
PGDATABASE="$DB" IMBOY_DIR="$ROOT" "$ROOT/scripts/drill_migrate.escript" up

ATTESTATION_PREDICATE="$(
  sed -n 's/^E2EE_ATTESTATION_SCHEMA_PREDICATE="\(.*\)"$/\1/p' "$ROOT/scripts/lib/blue_green_deploy.sh"
)"
[ -n "$ATTESTATION_PREDICATE" ]

schema_predicate() {
  psql -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" -d "$DB" -Atq \
    -v ON_ERROR_STOP=1 -c "SELECT CASE WHEN $ATTESTATION_PREDICATE THEN 1 ELSE 0 END"
}

assert_schema_mutation_rejected() {
  local mutation="$1"
  local result
  result="$(
    psql -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" -d "$DB" -Atq \
      -v ON_ERROR_STOP=1 <<SQL
BEGIN;
$mutation
SELECT CASE WHEN $ATTESTATION_PREDICATE THEN 1 ELSE 0 END;
ROLLBACK;
SQL
  )"
  [ "$result" = 0 ]
}

[ "$(schema_predicate)" = 1 ]
assert_schema_mutation_rejected \
  "ALTER TABLE public.e2ee_group_session_attestation ALTER COLUMN sender_did TYPE varchar(127);"
assert_schema_mutation_rejected \
  "ALTER TABLE public.e2ee_group_session_member DROP CONSTRAINT e2ee_group_session_member_pkey; ALTER TABLE public.e2ee_group_session_member ADD CONSTRAINT e2ee_group_session_member_pkey PRIMARY KEY (group_id, user_id, session_id);"
assert_schema_mutation_rejected \
  "ALTER TABLE public.e2ee_group_session_attestation DROP CONSTRAINT e2ee_group_session_attestation_session_id_key; ALTER TABLE public.e2ee_group_session_attestation ADD CONSTRAINT e2ee_group_session_attestation_session_id_key UNIQUE (room_key_msg_id);"
assert_schema_mutation_rejected \
  "ALTER TABLE public.e2ee_group_session_member DROP CONSTRAINT fk_e2ee_group_session_member_session; ALTER TABLE public.e2ee_group_session_member ADD CONSTRAINT fk_e2ee_group_session_member_session FOREIGN KEY (session_id) REFERENCES public.e2ee_group_session_attestation (session_id);"
assert_schema_mutation_rejected \
  "ALTER TABLE public.e2ee_group_session_member DROP CONSTRAINT chk_e2ee_group_session_member_values; ALTER TABLE public.e2ee_group_session_member ADD CONSTRAINT chk_e2ee_group_session_member_values CHECK (generation_no >= 0);"

IMBOY_GA_TEST_DB="$DB" \
IMBOY_GA_TEST_HOST="$PGHOST" \
IMBOY_GA_TEST_PORT="$PGPORT" \
IMBOY_GA_TEST_USER="$PGUSER" \
IMBOY_GA_TEST_PASSWORD="$PGPASSWORD" \
make -C "$ROOT" eunit t=group_attachment_acl_integration_tests

RUNTIME_SECRET="$(printf 'e2ee-claim:%s' "$DB" | shasum -a 256 | awk '{print $1}')"
for suite in group_history_boundary_tests e2ee_c2g_message_pipeline_integration_tests; do
  IMBOYENV=local HTTP_PORT=0 \
  IMBOY_PG_HOST="$PGHOST" IMBOY_PG_PORT="$PGPORT" \
  IMBOY_PG_USERNAME="$PGUSER" IMBOY_PG_PASSWORD="$PGPASSWORD" \
  IMBOY_PG_DATABASE="$DB" IMBOY_AUTO_MIGRATE=false \
  IMBOY_ADM_COOKIE_SECRET="adm:$RUNTIME_SECRET" \
  IMBOY_POSTGRE_AES_KEY="aes:$RUNTIME_SECRET" \
  IMBOY_JWT_KEY="jwt:$RUNTIME_SECRET" \
  make -C "$ROOT" eunit-local "t=$suite" EUNIT_CONFIG=config/sys.runtime
done

echo "group boundary/attachment/action/member-key/worker-archive PostgreSQL test: PASS"
