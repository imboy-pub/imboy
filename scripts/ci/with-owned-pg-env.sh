#!/usr/bin/env bash
# Explicit local test-server wiring, never infer from daily application config.
set -euo pipefail
[[ ${IMBOY_TEST_OWNED_PG_SOURCE:-} == 1 ]] || { echo 'Explicit owned PG source acknowledgement required' >&2; exit 2; }
for name in PGHOST PGPORT PGUSER PGDATABASE; do
 [[ -v "$name" && -n ${!name} ]] || { echo "Missing owned test input: $name" >&2; exit 2; }
done
[[ -v PGPASSWORD ]] || { echo 'Missing owned test input: PGPASSWORD (explicit empty is valid for trust)' >&2; exit 2; }
case "$PGHOST" in 127.0.0.1|localhost|::1) ;; *) echo 'Owned test source must be loopback' >&2; exit 2 ;; esac
[[ "$PGPORT" =~ ^[0-9]+$ && "$PGPORT" -ge 1 && "$PGPORT" -le 65535 ]] || { echo 'Invalid owned test input: PGPORT' >&2; exit 2; }
[[ $# -gt 0 ]] || { echo 'Usage: with-owned-pg-env.sh COMMAND [ARG...]' >&2; exit 2; }
export HIRDPOC_PG_HOST="$PGHOST" HIRDPOC_PG_PORT="$PGPORT" HIRDPOC_PG_USER="$PGUSER" HIRDPOC_PG_DB="$PGDATABASE" HIRDPOC_PG_PASSWORD="$PGPASSWORD"
export DIRECTORY_SYNC_INTTEST_PG_HOST="$PGHOST" DIRECTORY_SYNC_INTTEST_PG_PORT="$PGPORT" DIRECTORY_SYNC_INTTEST_PG_USER="$PGUSER" DIRECTORY_SYNC_INTTEST_PG_PASSWORD="$PGPASSWORD"
export TG16_PG_HOST="$PGHOST" TG16_PG_PORT="$PGPORT" TG16_PG_USER="$PGUSER" TG16_PG_PASSWORD="$PGPASSWORD"
exec "$@"
