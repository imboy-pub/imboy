#!/usr/bin/env bash
# No existing database or object storage is touched.
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
IMAGE="${IMBOY_INTTEST_PG_IMAGE:-imboy/pg18:3.6.1-2}"
TASK_CONTAINER="imboy-legacy-file-probe-$$"
RUN_DIR="$(mktemp -d /tmp/imboy-legacy-file-probe.XXXXXX)"
CREATED=0
cleanup() {
    if [[ "$CREATED" -eq 1 ]]; then docker rm -f -v "$TASK_CONTAINER" >/dev/null; fi
}
trap cleanup EXIT
mkdir "$RUN_DIR/empty-init"
docker image inspect "$IMAGE" --format '{{.Id}}' > "$RUN_DIR/image-id.txt"
docker run -d --name "$TASK_CONTAINER" \
    -e POSTGRES_USER=synthetic -e POSTGRES_PASSWORD=SYNTHETIC-ONLY \
    -e POSTGRES_DB=synthetic_legacy_probe \
    --mount "type=bind,source=$RUN_DIR/empty-init,target=/docker-entrypoint-initdb.d,readonly" \
    --mount "type=bind,source=$ROOT/priv/migrations,target=/probe/priv/migrations,readonly" \
    --mount "type=bind,source=$ROOT/scripts/test/group_file_legacy_migration.sql,target=/probe/probe.sql,readonly" \
    "$IMAGE" postgres -c shared_preload_libraries=timescaledb >/dev/null
CREATED=1
READY=0
for _ in {1..30}; do
    if docker exec "$TASK_CONTAINER" pg_isready -U synthetic -d synthetic_legacy_probe >/dev/null 2>&1; then
        READY=1; break
    fi
    sleep 1
done
[[ "$READY" -eq 1 ]]
docker exec -w /probe "$TASK_CONTAINER" psql -X -U synthetic \
    -d synthetic_legacy_probe -v ON_ERROR_STOP=1 -f probe.sql | tee "$RUN_DIR/result.txt"
printf 'Evidence: %s\n' "$RUN_DIR"
