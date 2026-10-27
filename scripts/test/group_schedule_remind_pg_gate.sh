#!/usr/bin/env bash
# Exclusive PostgreSQL gate; invoke from repository root with PG* and the two
# IMBOY_TEST_* identity variables set. The test checks the database ownership
# comment and system identifier before making any changes.
set -euo pipefail
: "${PGHOST:?}" "${PGPORT:?}" "${PGUSER:?}" "${PGPASSWORD:?}" "${PGDATABASE:?}"
: "${IMBOY_TEST_EXCLUSIVE_DATABASE:?}" "${IMBOY_TEST_PG_SYSTEM_ID:?}"
[[ "$PGDATABASE" == "$IMBOY_TEST_EXCLUSIVE_DATABASE" ]] || { echo 'database identity mismatch' >&2; exit 2; }
root=$(git rev-parse --show-toplevel)
cd "$root"
tmp=$(mktemp -d "${TMPDIR:-/tmp}/imboy-schedule-pg.XXXXXXXX")
trap 'rm -rf -- "$tmp"' EXIT
if command -v gmake >/dev/null 2>&1; then make_bin=gmake; else make_bin=make; fi
"$make_bin" compile SKIP_DEPS=1 PROTO_FILES=
# Recompile every directly exercised production module into the isolated path;
# do not trust pre-existing ebin timestamps or cached beams for these modules.
compiler_paths=(-pa ebin)
for dep in deps/*/ebin; do [[ -d "$dep" ]] && compiler_paths+=(-pa "$dep"); done
erlc +debug_info -I include "${compiler_paths[@]}" -o "$tmp" \
  src/ds/group_schedule_ds.erl src/ds/msg_s2c_ds.erl src/logic/group_schedule_logic.erl \
  src/repo/group_schedule_repo.erl src/repo/msg_store_repo.erl \
  src/lib/workspace_guard.erl src/lib/workspace_resolver.erl src/ds/msg_store_worker.erl
erlc +nowarn_deprecated_catch -DTEST -I include -pa 'deps/*/ebin' -o "$tmp" test/integration/group_schedule_remind_pg_tests.erl
paths=()
for dep in deps/*/ebin; do [[ -d "$dep" ]] && paths+=("$dep"); done
paths+=(ebin "$tmp")
export IMBOY_GATE_BEAM_DIR="$tmp"
set +e
erl -noshell -pa "${paths[@]}" -eval 'D = filename:absname(os:getenv("IMBOY_GATE_BEAM_DIR")), Mods = [group_schedule_ds,msg_s2c_ds,group_schedule_logic,group_schedule_repo,msg_store_repo,workspace_guard,workspace_resolver,msg_store_worker], true = lists:all(fun(M) -> filename:dirname(filename:absname(code:which(M))) =:= D end, Mods), case eunit:test(group_schedule_remind_pg_tests,[verbose]) of ok -> halt(0); _ -> halt(1) end.' > "$tmp/result.log" 2>&1
rc=$?
set -e
cat "$tmp/result.log"
[[ $rc -eq 0 ]] || exit "$rc"
grep -Eq 'Test passed\.|All [1-9][0-9]* tests passed\.' "$tmp/result.log" || { echo 'PG gate did not execute a nonzero test count' >&2; exit 1; }
