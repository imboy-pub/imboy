#!/usr/bin/env bash
# Exercise the real image entrypoint: three empty clusters, TCP barrier,
# extension idempotency and restart of an initialized cluster.
set -euo pipefail
image=${1:-imboy-pg18:ci}
name="imboy_pg_init_gate_${GITHUB_RUN_ID:-$$}"
tmp=$(mktemp -d)
cleanup() { docker rm -fv "$name" >/dev/null 2>&1 || true; rm -rf "$tmp"; }
trap cleanup EXIT
trap 'docker logs "$name" >&2 || true' ERR
# Keep the temporary socket server alive long enough to check the old probe.
printf '%s\n' 'touch /tmp/imboy-init-started' 'sleep 5' > "$tmp/05_delay.sh"
chmod 755 "$tmp"
chmod 644 "$tmp/05_delay.sh"
wait_tcp() {
  for ((i=0; i<120; i++)); do
    if docker exec "$name" pg_isready -h 127.0.0.1 -U imboy_user -d imboy_v1 >/dev/null 2>&1; then
      return 0
    fi
    if [ "$(docker inspect -f '{{.State.Running}}' "$name")" != true ]; then
      echo 'PostgreSQL exited during initialization' >&2; return 1
    fi
    sleep 1
  done
  echo 'PostgreSQL TCP initialization barrier timed out' >&2; return 1
}
psql_db() {
  docker exec "$name" psql -X -U imboy_user -d "$1" -v ON_ERROR_STOP=1 -At -c "$2"
}
for round in 1 2 3; do
  docker run -d --name "$name" \
    -e POSTGRES_USER=imboy_user -e POSTGRES_PASSWORD=abc54321 \
    -e POSTGRES_DB=imboy_v1 \
    -v "$tmp/05_delay.sh:/docker-entrypoint-initdb.d/05_delay.sh:ro" "$image" >/dev/null
  for ((i=0; i<120; i++)); do
    if docker exec "$name" test -f /tmp/imboy-init-started; then break; fi
    sleep 0.1
  done
  docker exec "$name" test -f /tmp/imboy-init-started
  # Reproduce the false readiness used by the previous workflow.
  docker exec "$name" pg_isready -U imboy_user -d imboy_v1
  if docker exec "$name" pg_isready -h 127.0.0.1 -U imboy_user -d imboy_v1; then
    echo 'TCP probe accepted the temporary init server' >&2; exit 1
  fi
  wait_tcp
  test "$(psql_db imboy_v1 "SELECT count(*) FROM pg_extension WHERE extname IN ('timescaledb','postgis','pg_jieba')")" = 3
  # Repeat every extension required by the migration workflow on both the
  # initialized application DB and a genuinely empty additional database.
  docker exec "$name" createdb -U imboy_user init_empty
  for db in imboy_v1 init_empty; do
    for pass in 1 2; do
      for ext in timescaledb postgis pg_jieba pgcrypto pg_trgm vector citext \
                 btree_gin btree_gist '"uuid-ossp"' hstore unaccent; do
        psql_db "$db" "CREATE EXTENSION IF NOT EXISTS $ext;"
      done
      test "$(psql_db "$db" "SELECT count(*) FROM pg_extension WHERE extname IN ('timescaledb','postgis','pg_jieba','pgcrypto','pg_trgm','vector','citext','btree_gin','btree_gist','uuid-ossp','hstore','unaccent')")" = 12
    done
  done
  docker restart "$name" >/dev/null
  wait_tcp
  test "$(psql_db imboy_v1 "SELECT count(*) FROM pg_extension WHERE extname IN ('timescaledb','postgis','pg_jieba')")" = 3
  docker rm -fv "$name" >/dev/null
  echo "PG initialization round $round: PASS"
done
