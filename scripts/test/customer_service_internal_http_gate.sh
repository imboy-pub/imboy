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
import os, pathlib, subprocess, shutil
out = pathlib.Path(os.environ['IMBOY_GATE_RUN_DIR'])
deps = pathlib.Path(os.environ['IMBOY_GATE_DEPS_ROOT'])
os.environ['ERL_CRASH_DUMP'] = str(out / 'erl_crash.dump')
# Freeze metadata before compilation; a shared build can replace ebin meanwhile.
shutil.copyfile(deps / 'ebin' / 'imboy.app', out / 'beams' / 'imboy.app')
paths = [str(p) for p in (deps / 'deps').glob('*/ebin')]
if not paths:
    raise SystemExit('dependencies absent; build dependencies or set IMBOY_DEPS_ROOT')
# Read application metadata only; compile every product module from current source.
paths.insert(0, str(deps / 'ebin'))
files = [str(p) for p in pathlib.Path('src').rglob('*.erl')]
files += ['test/common/meck_helper.erl', 'test/api/qr_login_sse_handler_tests.erl',
          'test/api/intbe02_http_support.erl', 'test/api/enterprise_internal_wiring_http_tests.erl',
          'test/common/inttest_marker_db.erl', 'test/ds/workspace_creation_pg_tests.erl',
          'test/api/enterprise_workspace_write_http_checks.erl',
          'test/ds/channel_creation_tx_pg_checks.erl',
          'test/api/enterprise_channel_write_http_checks.erl',
          'test/api/enterprise_identity_contract_http_checks.erl',
          'test/api/workspace_channel_limit_http_checks.erl',
          'test/api/customer_service_seat_http_checks.erl',
          'test/api/customer_service_browser_fixture.erl',
          'test/features/customer_service/interfaces/cs_route_contract_tests.erl',
          'test/api/enterprise_asset_garage_pg_checks.erl',
          'test/features/enterprise_business/application/eb_ports_tests.erl',
          'test/api/customer_service_seat_sse_checks.erl',
          'test/api/customer_service_widget_expiry_http_checks.erl',
          'test/api/enterprise_oa_expiry_http_checks.erl']
files += ['test/lib/organization/organization_invite_race_pg_checks.erl',
          'test/lib/organization/organization_membership_journey_pg_checks.erl',
          'test/features/customer_service/infrastructure/cs_pg_test_fixture.erl',
          'test/features/customer_service/infrastructure/cs_pg_tests.erl',
          'test/features/customer_service/infrastructure/cs_message_lifecycle_pg_checks.erl',
          'test/features/customer_service/infrastructure/cs_session_open_pg_checks.erl',
          'test/features/customer_service/application/cs_widget_app_tests.erl',
          'test/features/customer_service/application/cs_fake_store.erl',
          'test/features/customer_service/application/cs_fake_id.erl',
          'test/features/customer_service/application/cs_fake_canonical_tx.erl',
          'test/features/enterprise_business/infrastructure/eb_pg_test_fixture.erl']
cmd = ['erlc', '+debug_info', '+nowarn_unused_function', '+{parse_transform,lager_transform}',
       '-o', str(out / 'beams'), '-I', 'include', '-I', 'src']
for path in paths:
    cmd += ['-pa', path]
with (out / 'compile.log').open('w') as log:
    subprocess.run(cmd + files, stdout=log, stderr=subprocess.STDOUT, check=True)
# Only test modules expose case lists for reuse in the disposable native PG run.
asset_tests = [
    'test/features/enterprise_business/infrastructure/eb_asset_store_tests.erl',
    'test/features/enterprise_business/infrastructure/eb_retention_pg_tests.erl',
    'test/features/enterprise_business/infrastructure/eb_purge_orphan_asset_pg_tests.erl',
    'test/features/enterprise_business/application/asset/eb_asset_tests.erl',
    'test/features/enterprise_business/interfaces/eb_tenant_handler_tests.erl',
    'test/features/enterprise_business/e2e/eb_e2e_runner.erl']
with (out / 'compile.log').open('a') as log:
    subprocess.run(cmd + ['+export_all'] + asset_tests, stdout=log, stderr=subprocess.STDOUT, check=True)
expr = ('application:set_env(lager,handlers,[{lager_console_backend,[{level,error}]}]), '
        'application:ensure_all_started(lager), '
        'application:load(imboy), application:set_env(imboy,sql_driver,pgsql), '
        'application:set_env(imboy,postgre_aes_key,<<"SYNTHETIC-CONFORMANCE-AES-KEY">>), '
        'application:set_env(imboy,pg_conf,#{start_mfa => {epgsql,connect,[#{}]}}), '
        '{ok,_} = imboy_cache:start_link([]), '
        'case eunit:test([enterprise_internal_wiring_http_tests,workspace_creation_pg_tests,qr_login_sse_handler_tests],[verbose]) of '
        'ok -> halt(0); _ -> halt(1) end.')
asset_mode = os.environ.get('IMBOY_ASSET_GARAGE_PG_CHECK') == '1'
if asset_mode:
    expr = expr[:expr.index('case eunit:test')] + 'enterprise_asset_garage_pg_checks:run(), halt(0).'
browser_runner = os.environ.get('IMBOY_CS_BROWSER_RUNNER')
assert not (asset_mode and browser_runner), 'choose asset PG or browser mode'
if browser_runner:
    import socket, time
    runner = pathlib.Path(browser_runner).resolve(strict=True)
    with socket.socket() as sock:
        sock.bind(('127.0.0.1', 0))
        host_port = sock.getsockname()[1]
    scheme = 'https' if os.environ.get('IMBOY_CS_BROWSER_ATTACHMENTS') == '1' else 'http'
    os.environ['CSWW_E2E_HOST_ORIGIN'] = f'{scheme}://127.0.0.1:{host_port}'
    os.environ['CSWW_E2E_HOST_PORT'] = str(host_port)
    browser_expr = expr[:expr.index('case eunit:test')] + 'customer_service_browser_fixture:run(), halt(0).'
    with (out / 'http.log').open('w') as log:
        server = subprocess.Popen(['erl', '-noshell', '-pa', *paths, str(out / 'beams'), '-eval', browser_expr],
                                  stdout=log, stderr=subprocess.STDOUT)
        try:
            deadline = time.monotonic() + 45
            while not (out / 'browser-fixture.json').is_file():
                if server.poll() is not None or time.monotonic() > deadline:
                    raise RuntimeError('browser fixture startup failed: ' + str(out / 'http.log'))
                time.sleep(0.1)
            with (out / 'browser.log').open('w') as browser_log:
                result = subprocess.run(['node', str(runner)], cwd=runner.parent.parent.parent,
                                        stdout=browser_log, stderr=subprocess.STDOUT, timeout=150)
            print((out / 'browser.log').read_text())
            (out / 'browser.done').touch()
            assert server.wait(timeout=15) == 0, 'fixture database proof/cleanup failed'
            if result.returncode:
                raise RuntimeError('browser journey failed: ' + str(out))
        finally:
            (out / 'browser.done').touch()
            if server.poll() is None:
                server.terminate()
                try:
                    server.wait(timeout=10)
                except subprocess.TimeoutExpired:
                    server.kill()
                    server.wait()
    print('Browser evidence: ' + str(out))
else:
    with (out / 'http.log').open('w') as log:
        subprocess.run(['erl', '-noshell', '-pa', *paths, str(out / 'beams'), '-eval', expr],
                       stdout=log, stderr=subprocess.STDOUT, check=True, timeout=180)
    report = (out / 'http.log').read_text()
    if '[EPGZ04] emit_event_failed crash' in report:
        raise SystemExit('failed-event emission crashed; inspect ' + str(out / 'http.log'))
    if not asset_mode:
        subprocess.run(['python3', 'scripts/test/check_identity_http_contract.py',
                        str(out / 'identity-responses.json')], check=True)
    print(report)
PY
printf 'Evidence: %s\n' "$RUN_DIR"
