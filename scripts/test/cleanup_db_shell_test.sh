#!/usr/bin/env bash
# Real Shell helper, mocked Docker/psql transport. No database assertions or Python.
set -euo pipefail
ROOT="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." && pwd)"
fail() { printf '[FAIL] %s\n' "$*" >&2; exit 1; }
for tool in cat chmod dirname env grep jq ln mkdir mktemp openssl rm sed tail; do
  command -v "$tool" >/dev/null 2>&1 || fail "Missing test dependency: $tool"
done
umask 077
scratch=$(mktemp -d)
trap 'rm -rf -- "$scratch"' EXIT
mkdir -p "$scratch/bin" "$scratch/tools" "$scratch/pgdata"
for tool in cat dirname grep jq mktemp openssl rm sed tail; do
  ln -s "$(command -v "$tool")" "$scratch/tools/$tool"
done
export TEST_ROOT="$scratch" TEST_ADMIN_PASSWORD=ADMIN_TEST_ONLY_PASSWORD_000000000000
export TEST_PASSWORD="TEST_ONLY_password_0123456789_'\"\\\$ literal=tail"
export TEST_JQ_BIN="$(command -v jq)"
export TEST_BACKEND_ID=bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
jq -n --arg password "$TEST_PASSWORD" --arg admin "$TEST_ADMIN_PASSWORD" '
  {services:{imboy_backend:{environment:{IMBOY_PG_HOST:"imboy_pg18", IMBOY_PG_PORT:"5432",
    IMBOY_PG_USER:"app_user", IMBOY_PG_DB:"app_db", IMBOY_PG_PASSWORD:$admin,
    IMBOY_DIRECT_CLEANUP_PG_USER:"cleanup_user", IMBOY_DIRECT_CLEANUP_PG_PASSWORD:$password}},
    imboy_pg18:{environment:{POSTGRES_USER:"app_user", POSTGRES_DB:"app_db", POSTGRES_PASSWORD:$admin}}}}
' >"$scratch/config.json"
cat >"$scratch/bin/docker" <<'MOCK'
#!/bin/bash
set -euo pipefail
printf '%s\n' "$@" >>"$TEST_ROOT/argv"
[[ ${MOCK_NOISE:-0} != 1 ]] || printf 'mock stderr: %s\n' "$TEST_PASSWORD" >&2
case " $* " in
  ' compose '*' config --format json '*)
    case ${MOCK_JSON:-valid} in
      empty) : ;; malformed) printf '{' ;;
      *) jq "${MOCK_FILTER:-.}" "$TEST_ROOT/config.json"
         [[ ${MOCK_JSON:-valid} != multiple ]] || printf '{}\n' ;;
    esac
    exit "${MOCK_CONFIG_RC:-0}" ;;
  ' compose '*' ps -q imboy_pg18 '*)
    case ${MOCK_IDS:-valid} in
      empty) : ;; invalid) printf 'invalid-id\n' ;;
      multiple) printf '%s\n' "$TEST_BACKEND_ID" "$TEST_BACKEND_ID" ;;
      *) printf 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n' ;;
    esac; exit 0 ;;
  ' compose '*' ps -q imboy_backend '*) printf '%s\n' "$TEST_BACKEND_ID"; exit 0 ;;
  ' inspect --format '*)
    case ${3:-} in
      '{{.Image}}') printf '%s\n' "${MOCK_IMAGE:-sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc}" ;;
      '{{json .Config.Env}}')
        [[ ${4:-} == "$TEST_BACKEND_ID" ]] || exit 89
        case ${MOCK_RUNNING_ENV:-valid} in
          empty) : ;; malformed) printf '[' ;; object) printf '{}' ;; empty_list) printf '[]' ;;
          bad_entry) printf '["entry_without_equals"]' ;;
          *) jq -c ".services.imboy_backend.environment | (${MOCK_RUNNING_FILTER:-.}) | to_entries | map(.key + \"=\" + (.value | tostring)) | if env.MOCK_RUNNING_ENV == \"duplicate\" then . + [.[0]] else . end" "$TEST_ROOT/config.json"
             [[ ${MOCK_RUNNING_ENV:-valid} != multiple ]] || printf '[]\n' ;;
        esac
        exit "${MOCK_RUNNING_RC:-0}" ;;
      *) exit 89 ;;
    esac; exit 0 ;;
  ' exec '*)
    case ${MOCK_VERSIONS:-valid} in
      empty) : ;; invalid) printf 'bad-version\n' ;;
      duplicate) printf '00000179\n00000179\n' ;; failed) exit 1 ;;
      *) printf '%s\n' 00000274 00000179 00000271 00000267 00000270 ;;
    esac; exit 0 ;;
esac
[[ ${1:-} == run ]] || exit 90
network=0; previous=; forwarded=
for arg in "$@"; do
  case "$arg" in -v*|--volume|--volume=*|--mount|--mount=*|--volumes-from*) exit 91 ;; esac
  [[ "$previous" != --network || "$arg" != "container:$TEST_BACKEND_ID" ]] || network=1
  [[ "$previous" != -e ]] || forwarded+=" $arg "
  previous=$arg
done
for key in PGHOST PGPORT PGUSER PGPASSWORD PGDATABASE PGCONNECT_TIMEOUT PGSSLMODE PGGSSENCMODE LC_ALL \
    CLEANUP_VERSIONS CLEANUP_LOGIN CLEANUP_PASSWORD CLEANUP_CREATE; do
  [[ "$forwarded" == *" $key "* ]] || exit 92
done
[[ $network == 1 && "$PGHOST" == imboy_pg18 && "$PGDATABASE" == app_db \
   && "$PGSSLMODE" == disable && "$PGGSSENCMODE" == disable ]] || exit 92
if [[ "$*" == *'SELECT 1'* ]]; then
  printf 'negative\n' >>"$TEST_ROOT/events"
  [[ "$PGUSER" == cleanup_user && "$PGPASSWORD" != "$TEST_PASSWORD" ]] || exit 93
  case ${MOCK_NEGATIVE:-auth} in
    trust) exit 0 ;; network) printf 'connection refused\n' >&2 ;;
    *) printf 'password authentication failed\n' >&2 ;;
  esac; exit 2
fi
sql=$(cat)
[[ "$sql" != *"$TEST_PASSWORD"* && "$sql" != *"$TEST_ADMIN_PASSWORD"* ]] || exit 94
if [[ "$sql" == *CLEANUP_VERIFIED* ]]; then
  printf 'verify\n' >>"$TEST_ROOT/events"
  [[ "$PGUSER" == cleanup_user && "$PGPASSWORD" == "$TEST_PASSWORD" ]] || exit 95
  [[ ${MOCK_VERIFY:-ok} != fail ]] || exit 1
  [[ ${MOCK_VERIFY:-ok} == no_marker ]] || printf 'CLEANUP_VERIFIED\n'
else
  printf 'bootstrap create=%s versions=%s\n' "$CLEANUP_CREATE" "$CLEANUP_VERSIONS" >>"$TEST_ROOT/events"
  [[ "$PGUSER" == app_user && "$PGPASSWORD" == "$TEST_ADMIN_PASSWORD" \
     && "$CLEANUP_PASSWORD" == "$TEST_PASSWORD" ]] || exit 96
  [[ ${MOCK_BOOTSTRAP:-ok} != fail ]] || { printf 'CLEANUP_STAGE=grant\n'; exit 1; }
  if [[ "$CLEANUP_CREATE" == true ]]; then printf 'CLEANUP_CREATED\n'; else printf 'CLEANUP_REUSED\n'; fi
fi
MOCK
cat >"$scratch/bin/jq" <<'MOCK'
#!/bin/bash
for arg in "$@"; do
  if [[ "$arg" == *"$TEST_PASSWORD"* || "$arg" == *"$TEST_ADMIN_PASSWORD"* ]]; then
    : >"$TEST_ROOT/argv-leak"; exit 97
  fi
done
exec "$TEST_JQ_BIN" "$@"
MOCK
cat >"$scratch/bin/psql" <<'MOCK'
#!/bin/bash
set -euo pipefail
[[ "$PGPASSWORD" == "$TEST_ADMIN_PASSWORD" ]] || exit 1
printf 'psql\n' >>"$TEST_ROOT/events"
case " $* " in *' -c '*) ;; *) cat >/dev/null ;; esac
MOCK
chmod +x "$scratch/bin/docker" "$scratch/bin/jq" "$scratch/bin/psql"
test_path="$scratch/bin:$scratch/tools"
passes=0
ok() { passes=$((passes + 1)); printf '[PASS] %s\n' "$1"; }
helper_args=(); bash_args=()
run() {
  : >"$scratch/events"; : >"$scratch/argv"
  rm -f "$scratch/argv-leak"
  local rc=0
  env -i PATH="$test_path" TEST_ROOT="$TEST_ROOT" TEST_PASSWORD="$TEST_PASSWORD" \
    TEST_ADMIN_PASSWORD="$TEST_ADMIN_PASSWORD" TEST_BACKEND_ID="$TEST_BACKEND_ID" TEST_JQ_BIN="$TEST_JQ_BIN" "$@" \
    /bin/bash "${bash_args[@]}" "$ROOT/deploy/init-cleanup-db.sh" "${helper_args[@]}" \
    >"$scratch/output" 2>&1 || rc=$?
  [[ ! -e "$scratch/argv-leak" ]] || fail 'A test credential entered jq argv (value suppressed)'
  for secret in "$TEST_PASSWORD" "$TEST_ADMIN_PASSWORD"; do
    ! grep -Fq -- "$secret" "$scratch/output" "$scratch/argv" || fail 'A test credential leaked (value suppressed)'
  done
  return "$rc"
}
reject() {
  local label=$1 phase=$2; shift 2
  ! run "$@" || fail "$label unexpectedly succeeded"
  case "$phase" in
    config) [[ ! -s "$scratch/events" ]] || fail "$label reached a SQL client" ;;
    bootstrap) grep -q '^bootstrap ' "$scratch/events" || fail "$label never reached bootstrap"
      ! grep -Eq '^(negative|verify)$' "$scratch/events" || fail "$label continued after bootstrap failure" ;;
    negative) grep -qx negative "$scratch/events" || fail "$label never reached authentication probe"
      ! grep -qx verify "$scratch/events" || fail "$label continued after authentication probe failure" ;;
    verify) grep -qx verify "$scratch/events" || fail "$label never reached verification" ;;
  esac
  ok "$label"
}
run || fail 'Default verification failed'
grep -qx 'bootstrap create=false versions=179,267,270,271,274' "$scratch/events" || fail 'Default mode or migration normalization mismatch'
ok 'Default verify, special password bytes, isolated network and normalized release versions'
helper_args=(--create)
run || fail 'Explicit creation failed'
grep -qx 'bootstrap create=true versions=179,267,270,271,274' "$scratch/events" || fail 'Create mode not passed'
ok 'Explicit create uses environment only, without customer volumes or password argv/SQL'
helper_args=()
while IFS='|' read -r label filter; do reject "$label" config "MOCK_FILTER=$filter"; done <<'CASES'
External PG host|.services.imboy_backend.environment.IMBOY_PG_HOST="external.invalid"
Wrong PG port|.services.imboy_backend.environment.IMBOY_PG_PORT="5433"
Mismatched database user|.services.imboy_pg18.environment.POSTGRES_USER="other"
Mismatched database name|.services.imboy_pg18.environment.POSTGRES_DB="other"
Mismatched database password|.services.imboy_pg18.environment.POSTGRES_PASSWORD="other"
Missing cleanup field|del(.services.imboy_backend.environment.IMBOY_DIRECT_CLEANUP_PG_USER)
Missing database field|del(.services.imboy_backend.environment.IMBOY_PG_DB)
Unsafe username characters|.services.imboy_backend.environment.IMBOY_DIRECT_CLEANUP_PG_USER="bad;role"
Reserved pg_ username|.services.imboy_backend.environment.IMBOY_DIRECT_CLEANUP_PG_USER="pg_cleanup"
Worker role reused as LOGIN|.services.imboy_backend.environment.IMBOY_DIRECT_CLEANUP_PG_USER="v5_worker"
Application user reused|.services.imboy_backend.environment.IMBOY_DIRECT_CLEANUP_PG_USER="app_user"
Short password|.services.imboy_backend.environment.IMBOY_DIRECT_CLEANUP_PG_PASSWORD="short"
Multiline password|.services.imboy_backend.environment.IMBOY_DIRECT_CLEANUP_PG_PASSWORD="TEST_ONLY_01234567890123456789012345\nmore"
Conflicting Compose username alias|.services.imboy_backend.environment.IMBOY_PG_USERNAME="other_user"
Conflicting Compose database alias|.services.imboy_backend.environment.IMBOY_PG_DATABASE="other_db"
Compose password-file source even when empty|.services.imboy_backend.environment.IMBOY_DIRECT_CLEANUP_PG_PASSWORD_FILE=""
CASES
for mode in empty malformed multiple; do reject "Invalid Compose JSON: $mode" config "MOCK_JSON=$mode"; done
reject 'Compose producer fails despite valid JSON' config MOCK_CONFIG_RC=23
for mode in empty invalid multiple; do reject "Invalid container identity: $mode" config "MOCK_IDS=$mode"; done
reject 'Invalid PG image identity' config MOCK_IMAGE=latest
while IFS='|' read -r label filter; do reject "$label" config "MOCK_RUNNING_FILTER=$filter"; done <<'CASES'
Stale running cleanup password|.IMBOY_DIRECT_CLEANUP_PG_PASSWORD="STALE_TEST_ONLY_PASSWORD_00000000000"
Stale running database password|.IMBOY_PG_PASSWORD="STALE_TEST_ONLY_PASSWORD_00000000000"
Stale running database host|.IMBOY_PG_HOST="external.invalid"
Missing running database field|del(.IMBOY_PG_DB)
Conflicting username alias|.IMBOY_PG_USERNAME="other_user"
Conflicting database alias|.IMBOY_PG_DATABASE="other_db"
Conflicting password-file source|.IMBOY_DIRECT_CLEANUP_PG_PASSWORD_FILE="/fixture/password"
CASES
for mode in empty malformed object empty_list multiple bad_entry duplicate; do reject "Invalid running environment: $mode" config "MOCK_RUNNING_ENV=$mode"; done
reject 'Runtime environment inspection failure' config MOCK_RUNNING_RC=24
run 'MOCK_RUNNING_FILTER=.IMBOY_PG_USERNAME="app_user" | .IMBOY_PG_DATABASE="app_db"' || fail 'Matching runtime aliases were rejected'
ok 'Matching username/database aliases retain their supported behavior'
for mode in empty invalid duplicate failed; do reject "Invalid release oracle: $mode" config "MOCK_VERSIONS=$mode"; done
reject 'Bootstrap failure stops all authentication probes' bootstrap MOCK_BOOTSTRAP=fail MOCK_NOISE=1
reject 'Wrong password accepted by trust is rejected' negative MOCK_NEGATIVE=trust
reject 'Network failure is not password-rejection evidence' negative MOCK_NEGATIVE=network
reject 'Correct-password or privilege check failure propagates' verify MOCK_VERIFY=fail MOCK_NOISE=1
reject 'Missing verification completion marker is rejected' verify MOCK_VERIFY=no_marker
bash_args=(-x -v)
run MOCK_NOISE=1 || fail 'Trace/noisy-stderr success path failed'
ok 'Explicit bash -x -v and noisy transport stderr do not reveal credentials'
for script in pg-initdb-imboy.sh pg-update-imboy.sh; do
  : >"$scratch/events"
  env -i PATH="$test_path" TEST_ROOT="$TEST_ROOT" TEST_ADMIN_PASSWORD="$TEST_ADMIN_PASSWORD" \
    POSTGRES_USER=app_user POSTGRES_DB=app_db POSTGRES_PASSWORD="$TEST_ADMIN_PASSWORD" \
    PGDATA="$scratch/pgdata" POSTGIS_VERSION=3.6.1+fixture /bin/bash -x -v "$ROOT/docker/$script" \
    </dev/null >"$scratch/output" 2>&1 || fail "$script fixture execution failed"
  grep -qx psql "$scratch/events" || fail "$script did not reach psql"
  ! grep -Fq -- "$TEST_ADMIN_PASSWORD" "$scratch/output" || fail "$script leaked a test credential (value suppressed)"
  ok "$script disables tracing before loading the database password"
done
printf '[PASS] Shell transport checks: %s; PostgreSQL SQL/login/privilege behavior remains a separate gate.\n' "$passes"
