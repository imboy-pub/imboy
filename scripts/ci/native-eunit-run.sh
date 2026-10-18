#!/usr/bin/env bash
# Own every disposable service and private fixture. Final command is unfiltered.
set -euo pipefail
umask 077
[[ ${NATIVE_EPHEMERAL_KEYS_APPROVED:-false} == true ]] || { echo 'Disposable Garage fixture-key approval required; no service or key created'; exit 2; }
[[ ${GITHUB_ACTIONS:-} == true ]] || { echo 'Requires approved GitHub runner'; exit 2; }
: "${NATIVE_CI_ROOT:?}" "${NATIVE_CI_ARTIFACTS:?}" "${GITHUB_RUN_ID:?}" "${GITHUB_RUN_ATTEMPT:?}" "${RUNNER_TEMP:?}"
[[ "$GITHUB_RUN_ID" =~ ^[0-9]+$ && "$GITHUB_RUN_ATTEMPT" =~ ^[0-9]+$ ]] || exit 2
EXPECTED_ROOT="${RUNNER_TEMP%/}/imboy-native-${GITHUB_RUN_ID}-${GITHUB_RUN_ATTEMPT}"
[[ "$NATIVE_CI_ROOT" == "$EXPECTED_ROOT" && -d "$NATIVE_CI_ROOT" && ! -L "$NATIVE_CI_ROOT" ]] || { echo 'Unowned native root path'; exit 2; }
REPO=$(pwd)
source "$NATIVE_CI_ROOT/activate.sh"
PRIVATE="$NATIVE_CI_ROOT/private"
mkdir -m 700 "$PRIVATE" "$PRIVATE/beams" "$PRIVATE/uid-xml"
printf '%s:%s\n' "$GITHUB_RUN_ID" "$GITHUB_RUN_ATTEMPT" > "$PRIVATE/.native-ci-owner"
export PGDATA="$PRIVATE/pgdata" PGHOST=127.0.0.1 PGPORT=55432 PGUSER=imboy_ci PGPASSWORD=''
export GARAGE_CONFIG_FILE="$PRIVATE/garage.toml" RUST_LOG=error
PG_STARTED=0
GPID=''
cleanup() {
  local rc=$?
  trap - EXIT
  set +e # Every disposal step must run even if earlier diagnostics fail.
  if [[ -n "$GPID" ]]; then kill -TERM "$GPID" 2>/dev/null || true; wait "$GPID" 2>/dev/null || true; fi
  if [[ "$PG_STARTED" == 1 ]]; then pg_ctl -D "$PGDATA" -m fast -w stop || rc=1; fi
  # Copy only selected service diagnostics after masking generated credentials.
  python3 - "$PRIVATE" "$NATIVE_CI_ARTIFACTS" <<'PY_CLEAN'
import os,pathlib,sys
private,out=map(pathlib.Path,sys.argv[1:])
secrets=[os.environ.get(n,'') for n in ('W202_GARAGE_ACCESS_KEY','W202_GARAGE_SECRET_KEY','NATIVE_GARAGE_RPC_SECRET')]
for pattern in ('postgres.log','garage-server.log','migrate-*.log'):
 for source in private.glob(pattern):
  text=source.read_text(errors='replace')
  for secret in secrets:
   if secret: text=text.replace(secret,'[REDACTED_SYNTHETIC_CREDENTIAL]')
  (out/source.name).write_text(text)
PY_CLEAN
  [[ $? == 0 ]] || { [[ "$rc" != 0 ]] || rc=1; }
  # Refuse arbitrary recursive deletion: exact job path, non-symlink directory,
  # and marker created by this invocation are all required. sudo handles the
  # narrowly owned root-written EUnit XML without widening permissions.
  private_deleted=false
  if [[ "$PRIVATE" == "$EXPECTED_ROOT/private" && -d "$PRIVATE" && ! -L "$PRIVATE" && -f "$PRIVATE/.native-ci-owner" ]] &&
     [[ "$(cat "$PRIVATE/.native-ci-owner")" == "$GITHUB_RUN_ID:$GITHUB_RUN_ATTEMPT" ]]; then
    if sudo -n rm -rf --one-file-system -- "$PRIVATE" && [[ ! -e "$PRIVATE" ]]; then
      private_deleted=true
    else
      [[ "$rc" != 0 ]] || rc=1
    fi
  else
    echo 'Private resource ownership check failed; refusing deletion' >&2
    [[ "$rc" != 0 ]] || rc=1
  fi
  unset W202_GARAGE_ACCESS_KEY W202_GARAGE_SECRET_KEY NATIVE_GARAGE_RPC_SECRET IMBOY_TEST_ACCESS IMBOY_TEST_SECRET ROOT_ENV
  printf 'cleanup_exit=%s\nprivate_directory_deleted=%s\n' "$rc" "$private_deleted" > "$NATIVE_CI_ARTIFACTS/service-cleanup.txt"
  # Never upload private configs, key files, XML or database directories.
  exit "$rc"
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
initdb -D "$PGDATA" --username="$PGUSER" --encoding=UTF8 --locale=C.UTF-8 --auth-local=trust --auth-host=trust > "$PRIVATE/initdb.log"
PG_STARTED=1
pg_ctl -D "$PGDATA" -l "$PRIVATE/postgres.log" -o "-h 127.0.0.1 -p $PGPORT -c unix_socket_directories='' -c shared_preload_libraries=pg_jieba,timescaledb,pg_stat_statements -c timescaledb.telemetry_level=off -c max_connections=160 -c timezone=UTC -c log_timezone=UTC -c fsync=on -c synchronous_commit=on" -w start
export NATIVE_GARAGE_RPC_SECRET="$(openssl rand -hex 32)"
python3 - "$PRIVATE" <<'PY'
import os,pathlib,sys
p=pathlib.Path(sys.argv[1]); (p/'garage.toml').write_text(f'''metadata_dir = "{p}/meta"
data_dir = "{p}/data"
db_engine = "sqlite"
replication_factor = 1
rpc_bind_addr = "127.0.0.1:3901"
rpc_public_addr = "127.0.0.1:3901"
rpc_secret = "{os.environ['NATIVE_GARAGE_RPC_SECRET']}"
[s3_api]
s3_region = "garage"
api_bind_addr = "127.0.0.1:3900"
root_domain = ".s3.garage.localhost"
''')
PY
garage server --single-node > "$PRIVATE/garage-server.log" 2>&1 & GPID=$!
ready=0
for _ in $(seq 1 100); do
  if garage status > "$PRIVATE/garage-status.log" 2>&1; then ready=1; break; fi
  kill -0 "$GPID" || { echo 'Garage exited; private log retained'; exit 1; }
  sleep 0.1
done
[[ "$ready" == 1 ]] || { echo 'Garage readiness failed'; exit 1; }
export W202_GARAGE_BUCKET=synthetic-enterprise-assets W202_GARAGE_ENDPOINT=http://127.0.0.1:3900 W202_GARAGE_REGION=garage
garage bucket create "$W202_GARAGE_BUCKET" > "$PRIVATE/garage-bucket.log" 2>&1
garage key create native-ci-rw > "$PRIVATE/garage-key.log" 2>&1
python3 - "$PRIVATE" <<'PY'
import pathlib,re,shlex,sys
p=pathlib.Path(sys.argv[1]); text=(p/'garage-key.log').read_text()
a=re.search(r'^Key ID:\s*(\S+)',text,re.M); s=re.search(r'^Secret key:\s*(\S+)',text,re.M)
assert a and s, 'Cannot parse private Garage key output'
(p/'garage-key.env').write_text('export W202_GARAGE_ACCESS_KEY='+shlex.quote(a[1])+'\nexport W202_GARAGE_SECRET_KEY='+shlex.quote(s[1])+'\n')
PY
source "$PRIVATE/garage-key.env"
garage bucket allow "$W202_GARAGE_BUCKET" --read --write --key native-ci-rw > "$PRIVATE/garage-allow.log" 2>&1
export IMBOY_TEST_ENDPOINT="$W202_GARAGE_ENDPOINT" IMBOY_TEST_ACCESS="$W202_GARAGE_ACCESS_KEY" IMBOY_TEST_SECRET="$W202_GARAGE_SECRET_KEY"

prepare_database() {
  export PGDATABASE="attv5_chain_ci_${GITHUB_RUN_ID}_${GITHUB_RUN_ATTEMPT}_$1"
  createdb -T template0 --encoding=UTF8 --locale=C.UTF-8 "$PGDATABASE"
  for ext in pgcrypto pg_jieba timescaledb vector postgis pg_trgm btree_gin btree_gist citext unaccent intarray pg_stat_statements; do
    psql -X -v ON_ERROR_STOP=1 -q -c "CREATE EXTENSION $ext"
  done
  scripts/drill_migrate.escript up > "$PRIVATE/migrate-$1.log" 2>&1
  export W403_CONFIG_PATH="$PRIVATE/sys-$1.config" W403_RESOURCE_MANIFEST="$PRIVATE/manifest-$1.json"
  python3 - "$REPO" <<'PY'
import json,os,pathlib,sys
root=pathlib.Path(sys.argv[1]); text=(root/'config/sys.config.example').read_text()
for original, count in [('username => "imboy_user"',2),('password => "abc54321"',2),('database => "imboy_v1"',2),('port => 4323',2)]:
 assert text.count(original)==count, 'Configuration shape changed: '+original
text=text.replace('username => "imboy_user"','username => "imboy_ci"').replace('password => "abc54321"','password => ""').replace('database => "imboy_v1"','database => '+json.dumps(os.environ['PGDATABASE'])).replace('port => 4323','port => 55432')
pathlib.Path(os.environ['W403_CONFIG_PATH']).write_text(text)
pathlib.Path(os.environ['W403_RESOURCE_MANIFEST']).write_text(json.dumps({'database':os.environ['PGDATABASE']})+'\n')
PY
  export ATTACHMENT_REVIEW_CONFIG="$W403_CONFIG_PATH" ATTACHMENT_REVIEW_MANIFEST="$W403_RESOURCE_MANIFEST"
  export EUNIT_CONFIG="${W403_CONFIG_PATH%.config}" EUNIT_USE_RELAY=0 EUNIT_RELAY_TARGET=55432 EUNIT_OPTS=verbose
  export ERL_LIBS="$REPO/deps" IMBOY_HIRD_BIN="$REPO/deps/hird_runtime/target/release/hird"
  export HIRDPOC_PG_HOST=127.0.0.1 HIRDPOC_PG_PORT=55432 HIRDPOC_PG_USER=imboy_ci HIRDPOC_PG_DB="$PGDATABASE" HIRDPOC_PG_PASSWORD=''
  export DIRECTORY_SYNC_INTTEST_PG_HOST=127.0.0.1 DIRECTORY_SYNC_INTTEST_PG_PORT=55432 DIRECTORY_SYNC_INTTEST_PG_USER=imboy_ci DIRECTORY_SYNC_INTTEST_PG_PASSWORD=fixture-local-trust
  export OA_CLOUD_MARKER_PORT=55432
  psql -X -At -v ON_ERROR_STOP=1 -c 'SELECT extname,extversion FROM pg_extension ORDER BY extname' > "$NATIVE_CI_ARTIFACTS/extensions-$1.txt"
  psql -X -At -v ON_ERROR_STOP=1 -c 'SELECT version FROM schema_migrations_history ORDER BY version' > "$NATIVE_CI_ARTIFACTS/migrations-$1.txt"
  local guards
  guards=$(psql -X -At -v ON_ERROR_STOP=1 -c "SELECT count(*) FROM pg_trigger WHERE NOT tgisinternal AND tgname='trg_00_c2c_order_guard' AND tgenabled IN('O','A') AND tgrelid IN('public.msg_store_staging'::regclass,'public.msg_c2c'::regclass,'public.msg_store'::regclass) AND tgfoid=to_regprocedure('public.fn_c2c_order_write_guard()')")
  [[ "$guards" == 3 ]] || { echo 'Fresh migration guard validation failed'; exit 1; }
}

root_command() {
  # Explicit list avoids inheriting runner access tokens or arbitrary test filters.
  local -a allowed=(PATH LD_LIBRARY_PATH PG_CONFIG ERL_FLAGS ERL_LIBS PGHOST PGPORT PGUSER PGPASSWORD PGDATABASE EUNIT_CONFIG EUNIT_USE_RELAY EUNIT_RELAY_TARGET EUNIT_OPTS W403_CONFIG_PATH W403_RESOURCE_MANIFEST ATTACHMENT_REVIEW_CONFIG ATTACHMENT_REVIEW_MANIFEST W202_GARAGE_ENDPOINT W202_GARAGE_BUCKET W202_GARAGE_REGION W202_GARAGE_ACCESS_KEY W202_GARAGE_SECRET_KEY IMBOY_TEST_ENDPOINT IMBOY_TEST_ACCESS IMBOY_TEST_SECRET IMBOY_HIRD_BIN HIRDPOC_PG_HOST HIRDPOC_PG_PORT HIRDPOC_PG_USER HIRDPOC_PG_DB HIRDPOC_PG_PASSWORD DIRECTORY_SYNC_INTTEST_PG_HOST DIRECTORY_SYNC_INTTEST_PG_PORT DIRECTORY_SYNC_INTTEST_PG_USER DIRECTORY_SYNC_INTTEST_PG_PASSWORD OA_CLOUD_MARKER_PORT NATIVE_UID_XML)
  ROOT_ENV=(sudo -n env -i "HOME=$HOME" USER=root LOGNAME=root "SUDO_UID=$(id -u)" "SUDO_GID=$(id -g)" LC_ALL=C.UTF-8 TMPDIR=/tmp)
  local name
  for name in "${allowed[@]}"; do [[ ! -v "$name" ]] || ROOT_ENV+=("$name=${!name}"); done
}

prepare_database uid
export NATIVE_UID_XML="$PRIVATE/uid-xml"
erlc -o "$PRIVATE/beams" scripts/ci/native_eunit_uid_preflight.erl
[[ ! -e imboy && ! -L imboy ]] || { echo 'Unexpected pre-existing OTP layout path'; exit 1; }
ln -s . imboy
root_command
uid_rc=0
python3 scripts/ci/native-eunit-capture.py "$PRIVATE/uid.raw.log" "$NATIVE_CI_ARTIFACTS/uid.log" \
  "${ROOT_ENV[@]}" erl -noshell -boot no_dot_erlang -config "$EUNIT_CONFIG" \
  -pa "$PRIVATE/beams" ebin test imboy/ebin -s native_eunit_uid_preflight run || uid_rc=$?
rm imboy
python3 scripts/ci/native-eunit-validate.py "$PRIVATE/uid.raw.log" "$NATIVE_CI_ARTIFACTS/uid-summary.json"
[[ "$uid_rc" == 0 ]] || exit "$uid_rc"
# Successful EUnit output is captured by its official XML reporter. Retain only
# exact permission-denial proof rows, never response headers or credentials.
sudo -n python3 scripts/ci/native-eunit-uid-proof.py \
  "$PRIVATE/uid-xml" "$NATIVE_CI_ARTIFACTS/uid-proof.json"
sudo -n chown "$(id -u):$(id -g)" "$NATIVE_CI_ARTIFACTS/uid-proof.json"

# A separate freshly migrated database prevents preflight residue from changing
# full-suite order or results. No module/test filter is passed to this command.
prepare_database full
root_command
printf 'FULL_COMMAND=IMBOYENV=local make eunit\n' | tee "$NATIVE_CI_ARTIFACTS/full-command.txt"
full_rc=0
python3 scripts/ci/native-eunit-capture.py "$PRIVATE/full.raw.log" "$NATIVE_CI_ARTIFACTS/full.log" \
  "${ROOT_ENV[@]}" bash -c 'IMBOYENV=local make eunit' || full_rc=$?
python3 scripts/ci/native-eunit-validate.py "$PRIVATE/full.raw.log" "$NATIVE_CI_ARTIFACTS/full-summary.json"
[[ "$full_rc" == 0 ]] || exit "$full_rc"
echo 'UNFILTERED_FULL_EUNIT_PASS'
