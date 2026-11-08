#!/usr/bin/env bash
# Real PostgreSQL identity/transaction regression, using an isolated Compose project.
# 真实 PG 账号/事务验证：合成迁移台账与四个函数；不代替全量迁移或附件业务验收。
# Usage: bash scripts/test/cleanup_db_postgres_test.sh [postgres:18|LOCAL_PG18_IMAGE]
# Requires Docker + Compose v2, Bash, jq and OpenSSL; no host psql or Python.
set +x
set +v
set -euo pipefail
umask 077

ROOT=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." && pwd)
fail() { printf '[FAIL] %s\n' "$*" >&2; exit 1; }
pass() { count=$((count + 1)); printf '[PASS] %s\n' "$*"; }
[[ $# -le 1 ]] || fail 'Usage: cleanup_db_postgres_test.sh [PG18_IMAGE]'
for tool in docker jq openssl awk grep mktemp; do
    command -v "$tool" >/dev/null 2>&1 || fail "Missing required test tool: $tool (test not run)"
done
docker compose version >/dev/null 2>&1 || fail 'Docker Compose v2 unavailable (test not run)'
docker info >/dev/null 2>&1 || fail 'Docker daemon unavailable (test not run)'
for file in init-cleanup-db.sh cleanup-db/bootstrap.sql cleanup-db/verify.sql; do
    [[ -r "$ROOT/deploy/$file" ]] || fail "Missing production dependency: deploy/$file"
done

work=$(mktemp -d)
fixture="$work/deploy"
mkdir -p "$fixture/cleanup-db"
cp "$ROOT/deploy/init-cleanup-db.sh" "$fixture/"
cp "$ROOT/deploy/cleanup-db/"*.sql "$fixture/cleanup-db/"
: > "$fixture/.env"
count=0
export COMPOSE_PROJECT_NAME="imboy-cleanup-test-${GITHUB_RUN_ID:-$$}-${RANDOM}"
export CLEANUP_TEST_IMAGE=${1:-postgres:18}
export CLEANUP_TEST_ADMIN_USER=cleanup_test_admin
export CLEANUP_TEST_ADMIN_PASSWORD="CleanupTestAdmin_$(openssl rand -hex 24)"
export CLEANUP_TEST_USER=cleanup_test_worker
export CLEANUP_TEST_PASSWORD="CleanupTestWorker_$(openssl rand -hex 24)"
admin_password=$CLEANUP_TEST_ADMIN_PASSWORD
worker_password=$CLEANUP_TEST_PASSWORD
secrets="$work/passwords"
printf '%s\n' "$admin_password" "$worker_password" > "$secrets"
output="$work/helper.log"
compose_file="$fixture/docker-compose.community.yml"
compose() { docker compose -f "$compose_file" "$@"; }
cleanup() {
    status=$?
    trap - EXIT
    compose down --volumes --remove-orphans >/dev/null 2>&1 || true
    rm -rf -- "$work"
    exit "$status"
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

cat > "$compose_file" <<'YAML'
services:
  imboy_pg18:
    image: ${CLEANUP_TEST_IMAGE}
    environment:
      POSTGRES_USER: ${CLEANUP_TEST_ADMIN_USER}
      POSTGRES_PASSWORD: ${CLEANUP_TEST_ADMIN_PASSWORD}
      POSTGRES_DB: cleanup_fixture
      POSTGRES_INITDB_ARGS: --auth-host=scram-sha-256 --auth-local=trust
    command: [postgres, -c, log_statement=all, -c, logging_collector=off]
    volumes:
      - pgdata:/var/lib/postgresql
    healthcheck:
      test: [CMD-SHELL, 'pg_isready -h 127.0.0.1 -U "$$POSTGRES_USER" -d "$$POSTGRES_DB"']
      interval: 1s
      timeout: 3s
      retries: 120
  imboy_backend:
    image: ${CLEANUP_TEST_IMAGE}
    entrypoint:
      - /bin/sh
      - -c
      - |
        mkdir -p /opt/imboy/lib/imboy-test/priv/migrations
        for n in 00000179 00000267 00000270 00000271 00000274; do
          touch "/opt/imboy/lib/imboy-test/priv/migrations/$${n}_fixture.up.sql"
        done
        exec sleep infinity
    depends_on:
      imboy_pg18:
        condition: service_healthy
    environment:
      IMBOY_PG_HOST: imboy_pg18
      IMBOY_PG_PORT: '5432'
      IMBOY_PG_DB: cleanup_fixture
      IMBOY_PG_USER: ${CLEANUP_TEST_ADMIN_USER}
      IMBOY_PG_PASSWORD: ${CLEANUP_TEST_ADMIN_PASSWORD}
      IMBOY_DIRECT_CLEANUP_PG_USER: ${CLEANUP_TEST_USER}
      IMBOY_DIRECT_CLEANUP_PG_PASSWORD: ${CLEANUP_TEST_PASSWORD}
volumes:
  pgdata:
YAML

# Pull only the requested test image; production helper must reuse its local ID.
docker image inspect "$CLEANUP_TEST_IMAGE" >/dev/null 2>&1 \
    || docker pull "$CLEANUP_TEST_IMAGE" >/dev/null
wait_backend_oracle() {
    local attempt
    for attempt in $(seq 1 30); do
        if compose exec -T imboy_backend test -f \
            /opt/imboy/lib/imboy-test/priv/migrations/00000274_fixture.up.sql >/dev/null 2>&1; then
            return 0
        fi
        sleep 1
    done
    return 1
}
sync_backend() {
    # Compose recreates only when effective environment/configuration changed.
    # Never recreate the database while testing alternative administrator roles.
    compose up -d --no-deps imboy_backend >/dev/null 2>&1 || return 1
    wait_backend_oracle
}
compose up -d --wait --wait-timeout 180 >/dev/null
wait_backend_oracle \
    || fail 'Synthetic backend release oracle did not become ready'
pg() {
    compose exec -T imboy_pg18 psql -X -w -U cleanup_test_admin \
        -d cleanup_fixture -v ON_ERROR_STOP=1 -At "$@"
}
sql() { printf '%s\n' "$1" | pg; }
[[ $(sql 'SHOW server_version_num') -ge 180000 ]] \
    || fail 'This gate requires PostgreSQL 18 or newer'
pass 'Real PostgreSQL 18+ server and isolated backend network are running'

check_secret_logs() {
    compose logs --no-color imboy_pg18 > "$work/server.log" 2>&1
    if grep -Fq -f "$secrets" "$work/server.log" "$output"; then
        fail 'A plaintext test password appeared in helper/server logs (output withheld)'
    fi
}
run_helper() {
    if ! sync_backend; then
        printf '[FAIL] Backend fixture did not refresh its environment/oracle\n' > "$output"
        return 1
    fi
    COMPOSE_FILES=docker-compose.community.yml \
        bash "${helper:-$fixture/init-cleanup-db.sh}" "$@" > "$output" 2>&1
}
must_pass() {
    local name=$1; shift
    if ! run_helper "$@"; then
        check_secret_logs
        tail -20 "$output" >&2
        fail "$name"
    fi
    check_secret_logs
    pass "$name"
}
must_fail() {
    local name=$1 expected=$2; shift 2
    if run_helper "$@"; then fail "$name: unexpectedly accepted"; fi
    check_secret_logs
    if ! grep -Eq "$expected" "$output"; then
        tail -20 "$output" >&2
        fail "$name: failed outside the expected validation stage"
    fi
    pass "$name"
}
absent() {
    [[ $(sql "SELECT count(*) FROM pg_roles WHERE rolname='$CLEANUP_TEST_USER'") == 0 ]] \
        || fail 'A rejected initialization left a LOGIN behind'
}
assert_minimum_login() {
    [[ $(sql "SELECT count(*) FROM pg_roles WHERE rolname='$CLEANUP_TEST_USER' AND rolcanlogin AND NOT (rolsuper OR rolcreatedb OR rolcreaterole OR rolreplication OR rolbypassrls OR rolinherit)") == 1 ]] \
        || fail 'Expected exactly one LOGIN with minimum attributes'
    [[ $(sql "SELECT count(*) FROM pg_auth_members m JOIN pg_roles r ON r.oid=m.roleid JOIN pg_roles u ON u.oid=m.member WHERE u.rolname='$CLEANUP_TEST_USER' AND r.rolname='v5_worker' AND NOT m.admin_option AND NOT m.inherit_option AND m.set_option") == 1 ]] \
        || fail 'Expected one membership with ADMIN FALSE / INHERIT FALSE / SET TRUE'
    [[ $(sql "SELECT count(*) FROM pg_auth_members WHERE member=(SELECT oid FROM pg_roles WHERE rolname='$CLEANUP_TEST_USER')") == 1 ]] \
        || fail 'Created LOGIN has additional memberships'
    [[ $(sql "SELECT rolpassword LIKE 'SCRAM-SHA-256%' FROM pg_authid WHERE rolname='$CLEANUP_TEST_USER'") == t ]] \
        || fail 'Created password verifier is not SCRAM-SHA-256'
}
snapshot() {
    # Capture verifier and grants for comparison only. Never print their values.
    sql "SELECT row_to_json(r)::text FROM pg_authid r WHERE rolname='$CLEANUP_TEST_USER';
         SELECT row_to_json(m)::text FROM pg_auth_members m
          WHERE member=(SELECT oid FROM pg_roles WHERE rolname='$CLEANUP_TEST_USER')
             OR roleid=(SELECT oid FROM pg_roles WHERE rolname='$CLEANUP_TEST_USER')
          ORDER BY roleid,member,grantor;
         SELECT json_build_array(oid,datdba,datacl)::text FROM pg_database
          WHERE datname=current_database();
         SELECT json_build_array(oid,nspowner,nspacl)::text FROM pg_namespace
          WHERE nspname='public';
         SELECT json_build_array(c.oid,c.relowner,c.relacl)::text
          FROM pg_class c JOIN pg_namespace n ON n.oid=c.relnamespace
          WHERE n.nspname='public' ORDER BY c.oid;
         SELECT json_build_array(a.attrelid,a.attnum,a.attacl)::text
          FROM pg_attribute a JOIN pg_class c ON c.oid=a.attrelid
          JOIN pg_namespace n ON n.oid=c.relnamespace
          WHERE n.nspname='public' AND a.attnum>0 ORDER BY a.attrelid,a.attnum;
         SELECT json_build_array(p.oid,p.proowner,p.proacl)::text
          FROM pg_proc p JOIN pg_namespace n ON n.oid=p.pronamespace
          WHERE n.nspname='public' ORDER BY p.oid;"
}
unchanged_failure() {
    local name=$1 before; shift
    before=$(snapshot)
    must_fail "$name" "${expected_failure:-正确密码登录或最小权限检查失败}" "$@"
    [[ $(snapshot) == "$before" ]] || fail "$name: existing role or grants were changed"
}
set_password() {
    # Like production, \password uses libpq SCRAM hashing before sending ALTER ROLE.
    # SQL is read with -f so the two password responses remain on standard input.
    local role=$1 password=$2
    printf '%s\n' "$password" >> "$secrets"
    printf '%s\n%s\n' "$password" "$password" | \
        compose exec -T imboy_pg18 sh -c '
            set -eu
            umask 077
            script=$(mktemp)
            trap '\''rm -f "$script"'\'' EXIT
            printf "%s\n" "SET password_encryption = '\''scram-sha-256'\'';" "\\password $1" > "$script"
            psql -X -w -q -U cleanup_test_admin -d cleanup_fixture \
                -v ON_ERROR_STOP=1 -f "$script"
        ' sh "$role" > "$output" 2>&1
    check_secret_logs
}

must_fail 'Missing migration ledger rejects creation' '阶段: migrations' --create
absent
pg >/dev/null <<'SQL'
CREATE TABLE public.schema_migrations (version bigint PRIMARY KEY, dirty boolean NOT NULL);
CREATE TABLE public.schema_migrations_history (version bigint PRIMARY KEY);
INSERT INTO public.schema_migrations VALUES (274, false);
INSERT INTO public.schema_migrations_history VALUES (179),(267),(270),(271),(274);
SQL
must_fail 'Missing v5_worker rejects creation even with clean ledger' '阶段: worker_role' --create
absent

# Synthetic SQL contract only: these functions do not delete objects or implement
# IMBoy migrations. Real migration-head compatibility is a separate delivery gate.
pg >/dev/null <<'SQL'
CREATE ROLE v5_worker NOLOGIN NOINHERIT NOSUPERUSER NOCREATEDB NOCREATEROLE NOREPLICATION NOBYPASSRLS;
CREATE ROLE v5_fn_owner NOLOGIN NOSUPERUSER NOCREATEDB NOCREATEROLE NOREPLICATION NOBYPASSRLS;
CREATE ROLE v5_app_runtime NOLOGIN;
CREATE ROLE v5_proof NOLOGIN;
CREATE ROLE eb_direct_proof_worker NOLOGIN;
REVOKE CREATE ON SCHEMA public FROM PUBLIC;
GRANT USAGE ON SCHEMA public TO v5_worker, v5_fn_owner;
CREATE TABLE public.cleanup_test_business (id bigint PRIMARY KEY);
CREATE FUNCTION public.fn_dv1_tombstone_claim(uuid) RETURNS SETOF bigint
 LANGUAGE sql SECURITY DEFINER SET search_path=pg_catalog,public AS 'SELECT NULL::bigint WHERE false';
CREATE FUNCTION public.fn_dv1_tombstone_ack(bigint,uuid,boolean) RETURNS text
 LANGUAGE sql SECURITY DEFINER SET search_path=pg_catalog,public AS 'SELECT ''done''::text';
CREATE FUNCTION public.fn_cross_rescan_claim(uuid) RETURNS SETOF bigint
 LANGUAGE sql SECURITY DEFINER SET search_path=pg_catalog,public AS 'SELECT NULL::bigint WHERE false';
CREATE FUNCTION public.fn_cross_rescan_ack(bigint,uuid,boolean) RETURNS text
 LANGUAGE sql SECURITY DEFINER SET search_path=pg_catalog,public AS 'SELECT ''done''::text';
ALTER FUNCTION public.fn_dv1_tombstone_claim(uuid) OWNER TO v5_fn_owner;
ALTER FUNCTION public.fn_dv1_tombstone_ack(bigint,uuid,boolean) OWNER TO v5_fn_owner;
ALTER FUNCTION public.fn_cross_rescan_claim(uuid) OWNER TO v5_fn_owner;
ALTER FUNCTION public.fn_cross_rescan_ack(bigint,uuid,boolean) OWNER TO v5_fn_owner;
REVOKE ALL ON ALL FUNCTIONS IN SCHEMA public FROM PUBLIC;
GRANT EXECUTE ON FUNCTION public.fn_dv1_tombstone_claim(uuid),
 public.fn_dv1_tombstone_ack(bigint,uuid,boolean), public.fn_cross_rescan_claim(uuid),
 public.fn_cross_rescan_ack(bigint,uuid,boolean) TO v5_worker;
SQL

sql 'UPDATE public.schema_migrations SET dirty=true' >/dev/null
must_fail 'Dirty migration state rejects creation' '阶段: migrations' --create
absent
sql 'UPDATE public.schema_migrations SET dirty=false,version=270' >/dev/null
must_fail 'Database migration head differs from the running release' '阶段: migrations' --create
absent
sql 'UPDATE public.schema_migrations SET version=274; DELETE FROM public.schema_migrations_history WHERE version=179' >/dev/null
must_fail 'Missing required migration history rejects creation' '阶段: migrations' --create
absent
sql 'INSERT INTO public.schema_migrations_history VALUES (179),(99999999)' >/dev/null
must_fail 'Foreign migration history rejects creation' '阶段: migrations' --create
absent
sql 'DELETE FROM public.schema_migrations_history WHERE version=99999999; INSERT INTO public.schema_migrations VALUES (270,false)' >/dev/null
must_fail 'Multiple schema_migrations rows reject creation' '阶段: migrations' --create
absent
sql 'DELETE FROM public.schema_migrations WHERE version=270; ALTER FUNCTION public.fn_cross_rescan_ack(bigint,uuid,boolean) RENAME TO cleanup_test_hidden_ack' >/dev/null
must_fail 'Missing cleanup function rejects creation' '阶段: worker_role' --create
absent
sql 'ALTER FUNCTION public.cleanup_test_hidden_ack(bigint,uuid,boolean) RENAME TO fn_cross_rescan_ack' >/dev/null
must_fail 'Verification without --create does not create a missing LOGIN' '阶段: create_authority'
absent

# Fault injection lives only in a copied SQL fixture, never in the production API.
mkdir -p "$work/rollback/cleanup-db"
cp "$fixture/init-cleanup-db.sh" "$work/rollback/"
cp "$compose_file" "$work/rollback/docker-compose.community.yml"
cp "$fixture/cleanup-db/"*.sql "$work/rollback/cleanup-db/"
awk '/^[[:space:]]*COMMIT[[:space:]]*;/ {print "SELECT 1/0; -- test-only transaction failure"} {print}' \
    "$fixture/cleanup-db/bootstrap.sql" > "$work/rollback/cleanup-db/bootstrap.sql"
grep -q 'test-only transaction failure' "$work/rollback/cleanup-db/bootstrap.sql" \
    || fail 'Cannot locate bootstrap COMMIT for transaction failure test'
helper="$work/rollback/init-cleanup-db.sh"
must_fail 'SQL failure before COMMIT rolls back the complete initialization' '阶段: create_transaction' --create
absent
unset helper

must_pass 'Explicit --create provisions and authenticates the independent LOGIN' --create
assert_minimum_login
pass 'LOGIN attributes, sole membership and SCRAM verifier match the contract'
before=$(snapshot)
must_pass 'Default mode verifies an existing LOGIN without mutation'
must_pass 'Repeated --create preserves the existing LOGIN and verifier' --create
[[ $(snapshot) == "$before" ]] || fail 'Repeated initialization changed the verifier or role grants'
pass 'Repeated execution leaves role attributes, password hash and memberships unchanged'

# Deliberately change the model without recreating the backend. The helper must
# refuse stale credentials before either creating a new role or changing the old.
stale_before=$(snapshot)
CLEANUP_TEST_USER=cleanup_test_stale_worker
absent
if COMPOSE_FILES=docker-compose.community.yml bash "$fixture/init-cleanup-db.sh" --create \
    > "$output" 2>&1; then
    fail 'Changed Compose credentials were accepted without updating the backend'
fi
check_secret_logs
grep -Fq 'Compose 与运行中后端凭据不一致' "$output" \
    || fail 'Stale backend configuration failed outside the environment consistency gate'
absent
CLEANUP_TEST_USER=cleanup_test_worker
[[ $(snapshot) == "$stale_before" ]] \
    || fail 'Stale backend configuration rejection changed an existing role or ACL'
pass 'Stale Compose/backend credentials are rejected without creating or altering a LOGIN'

# Separate processes and logs exercise the per-LOGIN transaction lock. The two
# calls use one new target, and neither waits for the other before starting.
CLEANUP_TEST_USER=cleanup_test_concurrent_worker
absent
sync_backend || fail 'Concurrent test backend did not refresh its environment/oracle'
COMPOSE_FILES=docker-compose.community.yml bash "$fixture/init-cleanup-db.sh" --create \
    > "$work/concurrent-one.log" 2>&1 &
concurrent_one=$!
COMPOSE_FILES=docker-compose.community.yml bash "$fixture/init-cleanup-db.sh" --create \
    > "$work/concurrent-two.log" 2>&1 &
concurrent_two=$!
concurrent_one_status=0
concurrent_two_status=0
wait "$concurrent_one" || concurrent_one_status=$?
wait "$concurrent_two" || concurrent_two_status=$?
cat "$work/concurrent-one.log" "$work/concurrent-two.log" > "$output"
check_secret_logs
if [[ $concurrent_one_status -ne 0 || $concurrent_two_status -ne 0 ]]; then
    tail -30 "$output" >&2
    fail "Concurrent --create exits: $concurrent_one_status / $concurrent_two_status"
fi
[[ $(grep -c '新 LOGIN 已原子创建' "$output") == 1 \
   && $(grep -c '已有 LOGIN 保持原样' "$output") == 1 ]] \
    || fail 'Concurrent creation did not produce one creator and one reuse'
assert_minimum_login
pass 'Two simultaneous --create processes succeed with one LOGIN and one minimum grant'
CLEANUP_TEST_USER=cleanup_test_worker

CLEANUP_TEST_PASSWORD="CleanupTestWrong_$(openssl rand -hex 24)"
printf '%s\n' "$CLEANUP_TEST_PASSWORD" >> "$secrets"
unchanged_failure 'Incorrect password is rejected in verify mode'
unchanged_failure 'Incorrect password is rejected without reset in --create mode' --create
CLEANUP_TEST_PASSWORD=$worker_password
must_pass 'Restoring the original password recovers verification'

for attribute in SUPERUSER CREATEDB CREATEROLE REPLICATION BYPASSRLS INHERIT; do
    sql "ALTER ROLE $CLEANUP_TEST_USER $attribute" >/dev/null
    unchanged_failure "Existing $attribute LOGIN is refused without alteration" --create
    sql "ALTER ROLE $CLEANUP_TEST_USER NO$attribute" >/dev/null
done
sql "ALTER ROLE $CLEANUP_TEST_USER NOLOGIN" >/dev/null
unchanged_failure 'Existing NOLOGIN role is not silently enabled' --create
sql "ALTER ROLE $CLEANUP_TEST_USER LOGIN; GRANT v5_proof TO $CLEANUP_TEST_USER" >/dev/null
unchanged_failure 'Extra proof-role membership is rejected without revocation' --create
sql "REVOKE v5_proof FROM $CLEANUP_TEST_USER; GRANT v5_worker TO $CLEANUP_TEST_USER WITH ADMIN TRUE" >/dev/null
unchanged_failure 'ADMIN OPTION on v5_worker is rejected'
sql "GRANT v5_worker TO $CLEANUP_TEST_USER WITH ADMIN FALSE, INHERIT TRUE" >/dev/null
unchanged_failure 'Inherited worker privileges are rejected'
sql "GRANT v5_worker TO $CLEANUP_TEST_USER WITH INHERIT FALSE, SET FALSE" >/dev/null
unchanged_failure 'Membership without SET ROLE capability is rejected'
sql "GRANT v5_worker TO $CLEANUP_TEST_USER WITH SET TRUE; GRANT DELETE ON public.cleanup_test_business TO $CLEANUP_TEST_USER" >/dev/null
unchanged_failure 'Direct application-table DML grant is rejected'
sql "REVOKE DELETE ON public.cleanup_test_business FROM $CLEANUP_TEST_USER; GRANT UPDATE ON public.cleanup_test_business TO PUBLIC" >/dev/null
unchanged_failure 'Dangerous DML inherited from PUBLIC is rejected'
sql 'REVOKE UPDATE ON public.cleanup_test_business FROM PUBLIC; GRANT UPDATE (id) ON public.cleanup_test_business TO PUBLIC' >/dev/null
unchanged_failure 'PUBLIC column-level UPDATE is rejected'
sql "REVOKE UPDATE (id) ON public.cleanup_test_business FROM PUBLIC; GRANT UPDATE (id) ON public.cleanup_test_business TO $CLEANUP_TEST_USER" >/dev/null
unchanged_failure 'Direct column-level UPDATE is rejected'
sql "REVOKE UPDATE (id) ON public.cleanup_test_business FROM $CLEANUP_TEST_USER; GRANT DELETE ON public.cleanup_test_business TO v5_worker" >/dev/null
unchanged_failure 'Dangerous worker-role table DML is rejected'
sql 'REVOKE DELETE ON public.cleanup_test_business FROM v5_worker; REVOKE EXECUTE ON FUNCTION public.fn_cross_rescan_ack(bigint,uuid,boolean) FROM v5_worker' >/dev/null
expected_failure='阶段: worker_role' unchanged_failure 'Missing cleanup-function EXECUTE grant is rejected'
sql 'GRANT EXECUTE ON FUNCTION public.fn_cross_rescan_ack(bigint,uuid,boolean) TO v5_worker' >/dev/null
sql "ALTER TABLE public.cleanup_test_business OWNER TO $CLEANUP_TEST_USER" >/dev/null
unchanged_failure 'A cleanup LOGIN owning an application relation is rejected'
sql 'ALTER TABLE public.cleanup_test_business OWNER TO cleanup_test_admin' >/dev/null
must_pass 'DBA correction of permissions restores verification'
sql 'GRANT SELECT ON public.cleanup_test_business TO v5_worker' >/dev/null
must_pass 'Legitimate v5_worker SELECT grants remain compatible with verification'
sql 'REVOKE SELECT ON public.cleanup_test_business FROM v5_worker; GRANT v5_proof TO v5_worker' >/dev/null
expected_failure='阶段: worker_role' unchanged_failure 'v5_worker cannot inherit an additional role'
sql 'REVOKE v5_proof FROM v5_worker' >/dev/null

# Distinguish CREATEROLE and v5_worker ADMIN OPTION. Give fixture admins only the
# other prerequisites so each failure identifies an actual authority boundary.
pg >/dev/null <<'SQL'
CREATE ROLE cleanup_test_no_create LOGIN NOCREATEROLE;
CREATE ROLE cleanup_test_no_admin LOGIN CREATEROLE;
CREATE ROLE cleanup_test_no_connect_grant LOGIN CREATEROLE;
CREATE ROLE cleanup_test_role_admin LOGIN CREATEROLE;
GRANT CONNECT ON DATABASE cleanup_fixture TO cleanup_test_no_create, cleanup_test_no_admin, cleanup_test_role_admin WITH GRANT OPTION;
GRANT USAGE ON SCHEMA public TO cleanup_test_no_create, cleanup_test_no_admin, cleanup_test_no_connect_grant, cleanup_test_role_admin;
GRANT SELECT ON public.schema_migrations,public.schema_migrations_history TO cleanup_test_no_create, cleanup_test_no_admin, cleanup_test_no_connect_grant, cleanup_test_role_admin;
GRANT v5_worker TO cleanup_test_no_create,cleanup_test_no_connect_grant,cleanup_test_role_admin WITH ADMIN TRUE, INHERIT FALSE, SET TRUE;
SQL
limited_password="CleanupTestLimited_$(openssl rand -hex 24)"
for role in cleanup_test_no_create cleanup_test_no_admin cleanup_test_no_connect_grant cleanup_test_role_admin; do
    set_password "$role" "$limited_password"
done
CLEANUP_TEST_USER=cleanup_test_limited_worker
CLEANUP_TEST_ADMIN_PASSWORD=$limited_password
CLEANUP_TEST_ADMIN_USER=cleanup_test_no_create
must_fail 'Initializer without CREATEROLE cannot create the LOGIN' '阶段: create_authority' --create
absent
CLEANUP_TEST_ADMIN_USER=cleanup_test_no_admin
must_fail 'CREATEROLE without worker ADMIN OPTION cannot grant the role' '阶段: create_authority' --create
absent
CLEANUP_TEST_ADMIN_USER=cleanup_test_no_connect_grant
must_fail 'Role administration without database CONNECT grant authority is rejected' '阶段: create_authority' --create
absent
CLEANUP_TEST_ADMIN_USER=cleanup_test_role_admin
must_pass 'Explicitly delegated role administrator creates a minimum-privilege LOGIN' --create
CLEANUP_TEST_ADMIN_USER=cleanup_test_admin
CLEANUP_TEST_ADMIN_PASSWORD=$admin_password
CLEANUP_TEST_USER=cleanup_test_worker
CLEANUP_TEST_ADMIN_PASSWORD="CleanupTestWrongAdmin_$(openssl rand -hex 24)"
printf '%s\n' "$CLEANUP_TEST_ADMIN_PASSWORD" >> "$secrets"
expected_failure='阶段: connection' unchanged_failure 'Incorrect initializer password cannot change an existing LOGIN' --create
CLEANUP_TEST_ADMIN_PASSWORD=$admin_password

# Deliberately weaken authentication only inside this disposable test cluster.
# A successful wrong-password probe must never be mistaken for verified login.
compose exec -T imboy_pg18 sh -c '
    cp "$PGDATA/pg_hba.conf" "$PGDATA/pg_hba.conf.cleanup-test"
    printf "%s\n" "local all all trust" "host all all 0.0.0.0/0 trust" "host all all ::0/0 trust" > "$PGDATA/pg_hba.conf"
' >/dev/null
sql 'SELECT pg_reload_conf()' >/dev/null
sleep 1
expected_failure='错误密码也能登录' unchanged_failure 'trust authentication is rejected because wrong passwords also succeed'
# docker exec runs as root. Keep the original postgres-owned inode: replacing it
# with the root-owned 0600 backup makes PostgreSQL retain the previous trust rules.
compose exec -T imboy_pg18 sh -c '
    cat "$PGDATA/pg_hba.conf.cleanup-test" > "$PGDATA/pg_hba.conf"
    rm -f "$PGDATA/pg_hba.conf.cleanup-test"
' >/dev/null
[[ $(sql 'SELECT count(*) FROM pg_hba_file_rules WHERE error IS NOT NULL') == 0 ]] \
    || fail 'Restored HBA contains unreadable or invalid rules'
sql 'SELECT pg_reload_conf()' >/dev/null
sleep 1
must_pass 'Restoring SCRAM authentication restores verification'
check_secret_logs
pass 'No plaintext password in helper output or PostgreSQL log_statement=all logs'
printf '\n%d checks passed: real PostgreSQL identity/transaction tests; synthetic migration fixture only.\n' "$count"
