#!/usr/bin/env bash
# CI-only: reuse the migrated, disposable E2EE PostgreSQL; never migrate/drop a DB.
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/../.."
[[ ${GITHUB_ACTIONS:-} == true && ${LEE5_CI_DISPOSABLE:-} == 1 ]] || {
  echo 'ERROR: only the explicitly authorized disposable GitHub Actions fixture is supported' >&2
  exit 2
}
out=lee5-evidence
[[ ! -e "$out" ]] || { echo 'ERROR: refusing stale LEE-5 evidence' >&2; exit 2; }
mkdir -p "$out/raw" "$out/beams"
trap 'rc=$?; trap - EXIT; printf "%s\n" "$rc" > lee5-evidence/runner-exit.txt; exit "$rc"' EXIT
git rev-parse HEAD > "$out/commit.txt"
export LEE5_ACCEPTANCE=1 IMBOYENV=local IMBOY_AUTO_MIGRATE=false
printf '%s\n' "$LEE5_ACCEPTANCE" > "$out/acceptance-mode.txt"
# Fixed, synthetic credentials for the in-job container, not caller-supplied targets.
export IMBOY_PG_HOST=127.0.0.1 IMBOY_PG_PORT=4323
export IMBOY_PG_USERNAME=imboy_user IMBOY_PG_PASSWORD=abc54321 IMBOY_PG_DATABASE=imboy_v1
export IMBOY_TEST_CONFIG="$PWD/$out/owned.config"
# Match the TCP endpoint to the actual local container, not just its database name.
LEE5_PG_SYSTEM_ID=$(docker exec imboy_pg_eunit psql -U imboy_user -d imboy_v1 -At \
  -v ON_ERROR_STOP=1 -c 'SELECT system_identifier::text FROM pg_control_system()')
[[ $LEE5_PG_SYSTEM_ID =~ ^[0-9]+$ ]]
export LEE5_PG_SYSTEM_ID
erl -noshell -pa ebin deps/*/ebin -eval '
  {ok, [Terms]} = file:consult("config/sys.config.example"),
  Imboy = proplists:get_value(imboy, Terms),
  #{start_mfa := {epgsql, connect, [Opts]}} = proplists:get_value(pg_conf, Imboy),
  "127.0.0.1" = maps:get(host, Opts), 4323 = maps:get(port, Opts),
  "imboy_v1" = maps:get(database, Opts), "imboy_user" = maps:get(username, Opts),
  {ok, Conn} = epgsql:connect(Opts),
  {ok, _, [{SystemId, Version, <<"imboy_v1">>, true}]} = epgsql:equery(Conn,
    "SELECT system_identifier::text, current_setting('\''server_version_num'\'')::int, "
    "current_database()::text, to_regclass('\''public.qr_login_session'\'') IS NOT NULL "
    "FROM pg_control_system()", []),
  true = SystemId =:= list_to_binary(os:getenv("LEE5_PG_SYSTEM_ID")),
  true = Version >= 180000,
  ok = epgsql:close(Conn),
  %% QR DB fixtures boot the real app; provide valid synthetic storage roots
  %% instead of weakening the production registry validator. No S3 IO is needed.
  Garage = proplists:get_value(garage, Imboy),
  Root = fun(Namespace, BucketKey) ->
    #{driver => <<"garage">>, namespace => Namespace,
      endpoint => maps:get(endpoint, Garage), container => maps:get(BucketKey, Garage)}
  end,
  Registry = #{version => 1, upload_namespaces => #{}, roots => #{
    <<"garage-primary-private">> => Root(<<"garage-primary-private">>, bucket),
    <<"garage-primary-public">> => Root(<<"garage-primary-public">>, public_bucket)}},
  NoMigration = lists:keystore(auto_migrate, 1, Imboy, {auto_migrate, false}),
  SafeImboy = lists:keystore(attachment_storage_registry, 1, NoMigration,
    {attachment_storage_registry, Registry}),
  SafeTerms = lists:keystore(imboy, 1, Terms, {imboy, SafeImboy}),
  ok = file:write_file(os:getenv("IMBOY_TEST_CONFIG"),
    unicode:characters_to_binary(io_lib:format("~tp.~n", [SafeTerms]))),
  io:format("LEE5 fixture identity/version/schema preflight PASS~n"), halt(0).'

# One VM per suite, sequentially: no shared meck state, ports, or concurrent cleanup.
modules=(qr_login_handler_tests qr_login_sse_handler_tests qr_login_uid_contract_tests
  qr_login_pg_failure_tests qr_login_cleanup_worker_tests qr_login_session_repo_tests
  qr_login_claim_acceptance_tests)
for module in "${modules[@]}"; do
  mkdir "$out/raw/$module"
  make eunit-local EUNIT_CONFIG="$PWD/$out/owned" EUNIT_USE_RELAY=0 \
    EUNIT_MODS="$module" \
    EUNIT_OPTS="verbose, {report, {eunit_surefire, [{dir, \"$out/raw/$module\"}]}}" \
    2>&1 | tee "$out/raw/$module/run.log"
done
# Compile the actual existing two-peer entry harness, not a look-alike test.
erlc -I include -o "$out/beams" scripts/cache_v221_qr_entry_dcheck.erl
epmd -daemon
erl -noshell -kernel inet_dist_use_interface "{127,0,0,1}" \
  -pa "$out/beams" ebin test deps/*/ebin \
  -eval 'ok = cache_v221_qr_entry_dcheck:main([
    "lee5-evidence/dcheck.json", "lee5-evidence/owned.config"]), halt(0).' \
  2>&1 | tee "$out/raw/dcheck.log"
