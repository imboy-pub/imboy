#!/usr/bin/env bash
# Disposable PG + real Internal HTTP; synthetic fixtures, no existing databases.
set -euo pipefail
REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
DEPS_ROOT="${IMBOY_DEPS_ROOT:-$REPO}"
IMAGE="${IMBOY_INTTEST_PG_IMAGE:-imboy/pg18:3.6.1-2}"
RUN_DIR="$(mktemp -d /tmp/imboy-seat-http.XXXXXX)"
CONTAINER="imboy-seat-http-$$"
CREATED=0
cleanup() {
  if [[ "$CREATED" -eq 1 ]]; then docker rm -f -v "$CONTAINER" >/dev/null; fi
}
trap cleanup EXIT
cd "$REPO"
docker image inspect "$IMAGE" --format '{{.Id}}' > "$RUN_DIR/image-id.txt"
mkdir "$RUN_DIR/empty-init" "$RUN_DIR/beams"
# Vendor image init hooks currently fail on template_postgis; the harness itself
# installs every required extension and runs all actual migrations in a marker DB.
docker run -d --name "$CONTAINER" -e POSTGRES_USER=synthetic \
  -e POSTGRES_PASSWORD=SYNTHETIC-ONLY -e POSTGRES_DB=postgres \
  --mount "type=bind,source=$RUN_DIR/empty-init,target=/docker-entrypoint-initdb.d,readonly" \
  -p 127.0.0.1::5432 "$IMAGE" postgres -c shared_preload_libraries=timescaledb >/dev/null
CREATED=1
READY=0
for _ in {1..30}; do
  if docker exec "$CONTAINER" pg_isready -U synthetic -d postgres >/dev/null 2>&1; then READY=1; break; fi
  sleep 1
done
if [[ "$READY" -ne 1 ]]; then docker logs "$CONTAINER" > "$RUN_DIR/postgres.log" 2>&1; exit 1; fi
export INTBE02_INTTEST_PG_HOST=127.0.0.1
export INTBE02_INTTEST_PG_PORT="$(docker port "$CONTAINER" 5432/tcp | sed 's/.*://')"
export INTBE02_INTTEST_PG_USER=synthetic
export INTBE02_INTTEST_PG_PASSWORD=SYNTHETIC-ONLY
export IMBOY_GATE_RUN_DIR="$RUN_DIR" IMBOY_GATE_DEPS_ROOT="$DEPS_ROOT"
python3 - <<'PY'
import os, pathlib, subprocess
out = pathlib.Path(os.environ['IMBOY_GATE_RUN_DIR'])
deps = pathlib.Path(os.environ['IMBOY_GATE_DEPS_ROOT'])
paths = [str(p) for p in (deps / 'deps').glob('*/ebin')]
if not paths:
    raise SystemExit('dependencies absent; build dependencies or set IMBOY_DEPS_ROOT')
# Read application metadata only; compile every product module from current source.
paths.insert(0, str(deps / 'ebin'))
files = [str(p) for p in pathlib.Path('src').rglob('*.erl')]
files += ['test/api/intbe02_http_support.erl', 'test/api/enterprise_internal_wiring_http_tests.erl',
          'test/common/inttest_marker_db.erl', 'test/ds/workspace_creation_pg_tests.erl',
          'test/api/enterprise_workspace_write_http_checks.erl',
          'test/ds/channel_creation_tx_pg_checks.erl',
          'test/api/enterprise_channel_write_http_checks.erl',
          'test/api/enterprise_identity_contract_http_checks.erl',
          'test/api/workspace_channel_limit_http_checks.erl']
files += ['test/lib/organization/organization_invite_race_pg_checks.erl']
cmd = ['erlc', '+debug_info', '+nowarn_unused_function', '+{parse_transform,lager_transform}',
       '-o', str(out / 'beams'), '-I', 'include', '-I', 'src']
for path in paths:
    cmd += ['-pa', path]
with (out / 'compile.log').open('w') as log:
    subprocess.run(cmd + files, stdout=log, stderr=subprocess.STDOUT, check=True)
expr = ('application:set_env(lager,handlers,[{lager_console_backend,[{level,error}]}]), '
        'application:ensure_all_started(lager), '
        'application:load(imboy), application:set_env(imboy,sql_driver,pgsql), '
        'application:set_env(imboy,postgre_aes_key,<<"SYNTHETIC-CONFORMANCE-AES-KEY">>), '
        'application:set_env(imboy,pg_conf,#{start_mfa => {epgsql,connect,[#{}]}}), '
        '{ok,_} = imboy_cache:start_link([]), '
        'case eunit:test([enterprise_internal_wiring_http_tests,workspace_creation_pg_tests],[verbose]) of '
        'ok -> halt(0); _ -> halt(1) end.')
with (out / 'http.log').open('w') as log:
    subprocess.run(['erl', '-noshell', '-pa', *paths, str(out / 'beams'), '-eval', expr],
                   stdout=log, stderr=subprocess.STDOUT, check=True, timeout=180)
report = (out / 'http.log').read_text()
if '[EPGZ04] emit_event_failed crash' in report:
    raise SystemExit('failed-event emission crashed; inspect ' + str(out / 'http.log'))
subprocess.run(['python3', 'scripts/test/check_identity_http_contract.py',
                str(out / 'identity-responses.json')], check=True)
print(report)
PY
printf 'Evidence: %s\n' "$RUN_DIR"
