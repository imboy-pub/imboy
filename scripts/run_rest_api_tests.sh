#!/usr/bin/env bash
# REST API black-box test runner (RTF-01/RTF-03/RTF-05).
#
# Provision order is fixed: connectivity probe -> unique safe db name ->
# create database -> install extensions -> create_hypertable functional
# probe -> (application start runs migrations via auto_migrate) -> run
# Common Test -> cross-check evidence -> cleanup database.
#
# Credentials enter from the environment only (REST_PG_* / IMBOY_PG_*).
# This script must never contain or default a database password (RTF-00-A3).
#
# Exit codes:
#   0  PASS
#   2  environment/usage failure (missing credentials, unsafe db name, ...)
#   3  test/contract/evidence failure
#   75 BLOCKED_SHARED_CT (ct_imboy node busy for the whole wait window)
#   130/143 interrupted by SIGINT/SIGTERM; scratch DB dropped by the trap

set -euo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)

MODE=${1:-run}

die() {
  local code=$1
  shift
  echo "run_rest_api_tests: $*" >&2
  exit "$code"
}

# ---------------------------------------------------------------------------
# Scratch database naming: strictly imboy_rest_<run-id-sanitized>.
# ---------------------------------------------------------------------------

RUN_ID=${REST_RUN_ID:-"$(date -u +%Y%m%dT%H%M%SZ)-$$"}
DB_NAME=${REST_TEST_DB:-"imboy_rest_${RUN_ID//[^a-zA-Z0-9_]/_}"}

# The run id also flows into the report root path and the run_id evidence
# field, so accept only safe identifier characters outright instead of
# sanitizing per consumer (operator-controlled, but cheap to pin down).
# Pure-dot components are rejected explicitly: a component of dots stays
# inside the character whitelist but resolves to a parent directory.
case "$RUN_ID" in
  ''|.|..|*[!a-zA-Z0-9._-]*)
    die 2 "unsafe REST_RUN_ID '$RUN_ID' (allowed: alphanumerics, dot, dash, underscore; not a bare dot component)"
    ;;
esac

valid_db_name() {
  [[ "$1" =~ ^imboy_rest_[a-zA-Z0-9_]+$ ]]
}

require_credentials() {
  if [[ -z "${REST_PG_PASSWORD:-}" && -z "${IMBOY_PG_PASSWORD:-}" ]]; then
    echo "run_rest_api_tests: missing database password" >&2
    echo "  set REST_PG_PASSWORD (or IMBOY_PG_PASSWORD) in the environment;" >&2
    echo "  tracked runners must not carry a default password (RTF-00-A3)" >&2
    return 1
  fi
}

# ---------------------------------------------------------------------------
# Extension inventory: single source of truth is test/common/inttest_marker_db.erl
# (RTF-01 task 2: no second, drifting extension list). The runner parses the
# -define(EXTENSIONS, [...]) block from that module at runtime; it must never
# carry its own copy of the list.
# ---------------------------------------------------------------------------

extension_list() {
  awk '/-define\(EXTENSIONS, \[/,/\]\)\./' "$ROOT/test/common/inttest_marker_db.erl" |
    grep -oE '<<"[^"]+">>' | tr -d '<>"'
}

extension_inventory_ok() {
  [[ $(echo "$(extension_list)" | grep -c .) -ge 12 ]]
}

if [[ "$MODE" == "--check" ]]; then
  # Minimal self-check (RTF-01 task 6): no database is touched here.
  FAIL=0
  if valid_db_name "$DB_NAME"; then
    echo "CHECK OK db name: $DB_NAME"
  else
    echo "CHECK OK db name rejected: $DB_NAME"
  fi
  if valid_db_name "imboy_rest_evil'; drop database x"; then
    echo "CHECK FAIL unsafe name accepted"
    FAIL=1
  else
    echo "CHECK OK unsafe name rejected"
  fi
  if require_credentials; then
    echo "CHECK OK credentials present"
  else
    echo "CHECK OK missing credentials fail-fast verified"
  fi
  # Extension inventory single-source parse (RTF-01 task 2); file read only.
  if extension_inventory_ok; then
    echo "CHECK OK extension inventory parsed from test/common/inttest_marker_db.erl"
  else
    echo "CHECK FAIL extension inventory parse (inttest_marker_db.erl)"
    FAIL=1
  fi
  exit "$FAIL"
fi

[[ "$#" -eq 0 || "${1:-}" == "run" ]] ||
  die 2 "unknown mode: $MODE (expected --check, run, or no arguments)"

valid_db_name "$DB_NAME" || die 2 "refusing unsafe scratch database name: $DB_NAME"
require_credentials || die 2 "missing database credentials"

# ---------------------------------------------------------------------------
# PostgreSQL connection parameters (environment only).
# ---------------------------------------------------------------------------

PG_HOST=${REST_PG_HOST:-${IMBOY_PG_HOST:-127.0.0.1}}
PG_PORT=${REST_PG_PORT:-${IMBOY_PG_PORT:-4323}}
PG_USER=${REST_PG_USER:-${IMBOY_PG_USERNAME:-${IMBOY_PG_USER:-imboy_user}}}
PG_PASSWORD=${REST_PG_PASSWORD:-${IMBOY_PG_PASSWORD}}
PG_MAINT_DB=${REST_PG_MAINT_DB:-postgres}
export PGPASSWORD="$PG_PASSWORD"

REPORT_ROOT=${REST_REPORT_ROOT:-"$ROOT/.reports/rest/$RUN_ID"}
EVIDENCE_DIR="$REPORT_ROOT/evidence"
CT_LOGS_DIR="$REPORT_ROOT/ct"
# A reused report root would mix stale evidence into this run's aggregate
# gate. The runner never deletes (no rm -rf anywhere by design), so instead
# of a sibling-style pre-clean it refuses the rerun outright: fail fast
# with the fix in the message rather than a confusing evidence-count error.
if [[ -e "$REPORT_ROOT" && -n "$(ls -A "$REPORT_ROOT" 2>/dev/null)" ]]; then
  die 2 "report root already exists and is not empty; use a fresh REST_RUN_ID: $REPORT_ROOT"
fi
mkdir -p "$EVIDENCE_DIR" "$CT_LOGS_DIR"

CT_CONFIG=${REST_CT_CONFIG:-}
if [[ -z "$CT_CONFIG" ]]; then
  if [[ -f "$ROOT/config/sys.local.config" ]]; then
    CT_CONFIG=config/sys.local.config
  else
    die 2 "no loadable CT config (config/sys.local.config missing); REST_CT_CONFIG must point at a *.config file"
  fi
fi
# Make relative paths absolute before changing anything.
case "$CT_CONFIG" in
  /*) ;;
  *) CT_CONFIG="$ROOT/$CT_CONFIG" ;;
esac
[[ -f "$CT_CONFIG" ]] || die 2 "CT config not found: $CT_CONFIG"

# eunit_runner:find_project_root/1 locates the project root by looking for
# config/sys.config + Makefile above the CT working directory; a worktree
# only carries the gitignored sys.local.config, so materialize the resolved
# config as config/sys.config (RTF-05: the runner materializes the config,
# same duty the CI job has). It is untracked and never staged by this run —
# but the release tree symlinks its sys.config here, so the file must be
# left exactly as found: snapshot before overwrite, restore on every exit
# path (review round 8, 2026-09-23).
SYS_CONFIG_BACKUP=
SYS_CONFIG_RESTORE_NEEDED=0
sys_config_restore() {
  [[ "$SYS_CONFIG_RESTORE_NEEDED" != "0" ]] || return 0
  if [[ "$SYS_CONFIG_RESTORE_NEEDED" == "1" ]]; then
    if cp "$SYS_CONFIG_BACKUP" "$ROOT/config/sys.config" 2>/dev/null; then
      rm -f "$SYS_CONFIG_BACKUP" 2>/dev/null
    else
      echo "run_rest_api_tests: WARNING: could not restore config/sys.config (backup kept: $SYS_CONFIG_BACKUP)" >&2
      CLEANUP_FAILED=1
    fi
  else
    # The file did not exist before the run: remove the materialized copy.
    rm -f "$ROOT/config/sys.config" 2>/dev/null
  fi
  SYS_CONFIG_RESTORE_NEEDED=0
  return 0
}
if [[ "$CT_CONFIG" != "$ROOT/config/sys.config" ]]; then
  if [[ -e "$ROOT/config/sys.config" ]]; then
    SYS_CONFIG_BACKUP=$(mktemp "${TMPDIR:-/tmp}/sys.config.pre-rest.XXXXXX")
    cp "$ROOT/config/sys.config" "$SYS_CONFIG_BACKUP"
    SYS_CONFIG_RESTORE_NEEDED=1
  else
    SYS_CONFIG_RESTORE_NEEDED=2
  fi
  # Window guard: the full cleanup trap is wired further down; until then
  # this provisional trap is the only restore path if the script dies here.
  # It is replaced by the cleanup trap below.
  trap sys_config_restore EXIT
  cp "$CT_CONFIG" "$ROOT/config/sys.config"
fi
export IMBOY_TEST_CONFIG="$CT_CONFIG"
# The suite boot needs the worktree root for eunit_runner (test build) and
# the alias tree for code:priv_dir(imboy); see rest_fixture helpers.
export REST_PROJECT_ROOT="$ROOT"

# ---------------------------------------------------------------------------
# VM-startup alias + CT_OPTS.
#
# code:priv_dir(imboy) resolves by matching a code-path segment named
# "<...>/imboy/ebin"; the shared main tree matches because its directory is
# named `imboy`, a differently named worktree does not. Build
# .ct/appalias/imboy/{ebin,priv} (symlinks, untracked, runtime-only) and put
# it FIRST on the VM path via CT_OPTS (-pa). Putting the alias ahead of
# test/ also guarantees the real ebin builds (config_ds etc.) win over any
# stale test/common stub copies.
# ---------------------------------------------------------------------------

ALIAS_DIR="$ROOT/.ct/appalias/imboy"
mkdir -p "$ALIAS_DIR"
# `ln -sfn` cannot replace an existing real directory (it nests the link
# inside it), so drop any stale entry first; this scratch area is ours.
for entry in "ebin:$ROOT/ebin" "priv:$ROOT/priv"; do
  link="$ALIAS_DIR/${entry%%:*}"
  rm -rf "$link"
  ln -sfn "${entry#*:}" "$link"
done
ALIAS_EBIN="$ALIAS_DIR/ebin"

# Replicates Makefile's CT_ERL_ARGS (they live behind CT_OPTS += which a
# command-line CT_OPTS would override) and prefixes the alias -pa.
SYS_CONFIG_ABS="$ROOT/config/sys.config"
CT_OPTS="-pa $ALIAS_EBIN -erl_args -config $SYS_CONFIG_ABS -eval 'application:load(imboy)' -eval 'application:set_env(imboy, env, test)' -eval 'application:set_env(imboy, http_port, 0)' -eval 'application:set_env(imboy, dsync_enabled, false)'"
export CT_OPTS

# ---------------------------------------------------------------------------
# Shared Common Test node: erlang.mk hardcodes -sname ct_imboy.
# ---------------------------------------------------------------------------

wait_ct_node_free() {
  local waited=0
  while epmd -names 2>/dev/null | grep -q 'name ct_imboy at port'; do
    if [[ $waited -eq 0 ]]; then
      echo "WAITING_SHARED_CT: ct_imboy busy; waiting up to ${REST_CT_WAIT_SECONDS:-600}s"
    fi
    sleep 10
    waited=$((waited + 10))
    [[ $waited -ge ${REST_CT_WAIT_SECONDS:-600} ]] && return 1
  done
  return 0
}

# ---------------------------------------------------------------------------
# Extension inventory: parsed above from test/common/inttest_marker_db.erl
# (single source of truth, RTF-01 task 2). A parse failure here means the
# module format drifted; refusing to run beats silently provisioning a DB
# with a partial extension set.
# ---------------------------------------------------------------------------

EXTENSIONS=$(extension_list) ||
  die 2 "cannot parse extension inventory from inttest_marker_db.erl (define block format drifted?)"
extension_inventory_ok ||
  die 2 "extension inventory from inttest_marker_db.erl has fewer than 12 entries"

psql_scratch() {
  psql -X -q -h "$PG_HOST" -p "$PG_PORT" -U "$PG_USER" -v ON_ERROR_STOP=1 "$@"
}

# Failure must be distinguishable from an empty result: a scrape that
# cannot run makes residue verification impossible, and that case is a
# non-zero outcome, never a silent pass (reverdict 2026-09-23, finding 3).
DB_SCRAPE_FAILED=0
scratch_databases() {
  local out
  if out=$(psql -X -At -h "$PG_HOST" -p "$PG_PORT" -U "$PG_USER" -d "$PG_MAINT_DB" \
      -tAc "SELECT datname FROM pg_database WHERE datname LIKE 'imboy_rest_%' ORDER BY 1" 2>/dev/null); then
    printf '%s\n' "$out"
  else
    DB_SCRAPE_FAILED=1
  fi
}

DROP_DONE=0
DB_CREATED=0
CLEANUP_DONE=0
CLEANUP_FAILED=0
cleanup() {
  local final=$?
  # Re-entrancy: the INT/TERM traps exit explicitly, which re-enters via the
  # EXIT trap; the second pass must be a no-op.
  [[ $CLEANUP_DONE -eq 1 ]] && return "$final"
  CLEANUP_DONE=1
  if [[ $DB_CREATED -eq 1 && $DROP_DONE -eq 0 && "${REST_KEEP_DB:-0}" != "1" ]]; then
    # RTF-01: cleanup failure must not be swallowed. One grace retry (a
    # terminating backend can hold the drop briefly), then record it loudly:
    # marker file in the report root + CLEANUP_FAILED, which the normal exit
    # path turns into a non-zero run status.
    if ! dropdb --if-exists --force -h "$PG_HOST" -p "$PG_PORT" -U "$PG_USER" "$DB_NAME" >/dev/null 2>&1; then
      sleep 1
      if ! dropdb --if-exists --force -h "$PG_HOST" -p "$PG_PORT" -U "$PG_USER" "$DB_NAME" >/dev/null 2>&1; then
        CLEANUP_FAILED=1
        echo "run_rest_api_tests: CLEANUP FAILED: scratch database could not be dropped: $DB_NAME" >&2
        [[ -n "${REPORT_ROOT:-}" && -d "$REPORT_ROOT" ]] && : >"$REPORT_ROOT/CLEANUP_FAILED" 2>/dev/null
      fi
    fi
    DROP_DONE=1
  elif [[ "${REST_KEEP_DB:-0}" == "1" && $DB_CREATED -eq 1 ]]; then
    echo "run_rest_api_tests: REST_KEEP_DB=1; scratch database kept: $DB_NAME" >&2
    DROP_DONE=1
  fi
  # Leave config/sys.config exactly as found on every exit path (round 8).
  sys_config_restore
  return $final
}
trap cleanup EXIT
# RTF-01 task 4: INT/TERM must drop the scratch DB and stop immediately; a
# bare `trap cleanup INT TERM` would resume the run afterwards (suites would
# keep failing against an already-dropped database). 130/143 are the shell
# conventions for death-by-SIGINT/SIGTERM.
trap 'cleanup; exit 130' INT
trap 'cleanup; exit 143' TERM

# ---------------------------------------------------------------------------
# Provision.
# ---------------------------------------------------------------------------

pg_isready -h "$PG_HOST" -p "$PG_PORT" -U "$PG_USER" >/dev/null ||
  die 2 "postgres not ready at $PG_HOST:$PG_PORT"

PRE_DBS=$(scratch_databases)
if [[ "$DB_SCRAPE_FAILED" == "1" ]]; then
  die 2 "cannot inventory scratch databases; pre-state unverifiable"
fi
if grep -qx "$DB_NAME" <<<"$PRE_DBS"; then
  die 2 "scratch database already exists (rerun with a fresh REST_RUN_ID): $DB_NAME"
fi

createdb -h "$PG_HOST" -p "$PG_PORT" -U "$PG_USER" "$DB_NAME" ||
  die 2 "createdb failed for $DB_NAME (target: $PG_USER@$PG_HOST:$PG_PORT — the scratch provider default is 127.0.0.1:4323; check REST_PG_PORT/IMBOY_PG_PORT if this server looks wrong)"
# Only a database this run actually created may be dropped (or reported as
# kept) by cleanup; an earlier death (e.g. postgres not ready) must not
# report a false cleanup outcome for a database that never existed
# (review round 8, 2026-09-23).
DB_CREATED=1

for ext in $EXTENSIONS; do
  # Defense in depth: the names are interpolated into SQL, so only allow
  # plain identifier characters even though they come from tracked source.
  [[ "$ext" =~ ^[a-zA-Z0-9_]+$ ]] ||
    die 2 "unsafe extension name parsed from inttest_marker_db.erl: $ext"
  psql_scratch -d "$DB_NAME" -c "CREATE EXTENSION IF NOT EXISTS $ext" >/dev/null ||
    die 3 "extension install failed: $ext"
done

# RTF-01-A4: prove create_hypertable resolves AND executes before migrations
# run (migrations call public.create_hypertable on msg/user_log tables). A
# pg_proc lookup alone would miss a loaded-but-broken TimescaleDB. The three
# statements run as one implicit transaction, so the probe table is rolled
# back automatically if create_hypertable fails.
psql_scratch -d "$DB_NAME" \
  -c "CREATE TABLE rtf01_ts_probe(ts timestamptz NOT NULL);
      SELECT create_hypertable('rtf01_ts_probe', 'ts');
      DROP TABLE rtf01_ts_probe;" >/dev/null ||
  die 3 "create_hypertable probe failed (TimescaleDB not operational before migration)"

wait_ct_node_free || {
  echo '{"result":"BLOCKED_SHARED_CT","run_id":"'"$RUN_ID"'"}' >"$REPORT_ROOT/result.json"
  die 75 "ct_imboy busy; CT not executed (WAITING_SHARED_CT)"
}

# ---------------------------------------------------------------------------
# Environment for the Common Test node and the application under test.
# ---------------------------------------------------------------------------

export IMBOYENV=test
export IMBOY_PG_HOST="$PG_HOST"
export IMBOY_PG_PORT="$PG_PORT"
export IMBOY_PG_USERNAME="$PG_USER"
export IMBOY_PG_PASSWORD="$PG_PASSWORD"
export IMBOY_PG_DATABASE="$DB_NAME"
export TEST_HTTP_PORT=0
export REST_COMMIT_SHA
REST_COMMIT_SHA=$(git -C "$ROOT" rev-parse HEAD)
export REST_EVIDENCE_DIR="$EVIDENCE_DIR"
export REST_RUN_ID

echo "REST run: $RUN_ID"
echo "Scratch DB: $DB_NAME @ $PG_HOST:$PG_PORT"
echo "Report: $REPORT_ROOT"

STARTED_AT=$(date -u +%Y-%m-%dT%H:%M:%SZ)
RUNNER_LOG="$REPORT_ROOT/runner.log"
RUN_STATUS=0

bash "$ROOT/scripts/check_rest_contract_coverage.sh" 2>&1 | tee -a "$RUNNER_LOG" || RUN_STATUS=3

# RTF-02 unit gate (review P2): the redaction unit tests must EXECUTE on
# every round, not merely compile. Pure in-memory eunit, no database.
if [[ $RUN_STATUS -eq 0 ]]; then
  make -C "$ROOT" test-build >>"$RUNNER_LOG" 2>&1 || RUN_STATUS=3
  erl -pa "$ROOT/ebin" -pa "$ROOT/test" -pa "$ROOT"/deps/*/ebin -noshell -eval \
    'case eunit:test([rest_assert_tests, rest_evidence_tests]) of ok -> halt(0); _ -> halt(3) end' \
    >>"$RUNNER_LOG" 2>&1 \
    && echo "evidence cross-check: REST support unit tests PASS" | tee -a "$RUNNER_LOG" \
    || { echo "evidence cross-check: REST support unit tests FAIL" | tee -a "$RUNNER_LOG"; RUN_STATUS=3; }
fi

if [[ $RUN_STATUS -eq 0 ]]; then
  # One make invocation per suite so a failing suite does not stop the
  # remaining ones; any failure keeps the overall run non-zero.
  for SUITE in api_v1_login api_v1_auth api_v1_user api_v1_friend \
               api_v1_group api_v1_conversation api_v1_msg api_v1_channel; do
    echo "=== REST suite: $SUITE ===" | tee -a "$RUNNER_LOG"
    make -C "$ROOT" "ct-$SUITE" \
      CT_CONFIG="$CT_CONFIG" \
      TEST_HTTP_PORT=0 \
      CT_LOGS_DIR="$CT_LOGS_DIR" 2>&1 | tee -a "$RUNNER_LOG" || RUN_STATUS=3
  done
fi
FINISHED_AT=$(date -u +%Y-%m-%dT%H:%M:%SZ)

# ---------------------------------------------------------------------------
# Post-run: migration state evidence + evidence cross-check (RTF-03 task 8).
# ---------------------------------------------------------------------------

TABLES_COUNT=$(psql_scratch -d "$DB_NAME" -Atc \
  "SELECT count(*) FROM pg_tables WHERE schemaname='public'" || echo 0)
MIGRATION_TABLE=$(psql_scratch -d "$DB_NAME" -Atc \
  "SELECT table_name FROM information_schema.tables WHERE table_name LIKE '%migration%' AND table_schema='public' LIMIT 1" || true)

# Cross-check: the login golden suite must always deliver its five cases,
# and the aggregate gate derives the expected evidence total from each
# suite's all()/0 so the machine-readable totals track every case (review
# P2: case_total used to be golden-suite-only, diverging from the case
# truth in the CT logs).
EXPECTED_CASES="login-001 login-002 login-003 login-004 login-005"
EVIDENCE_STATUS=0
EXPECTED_TOTAL=0
for SUITE in api_v1_login api_v1_auth api_v1_user api_v1_friend \
             api_v1_group api_v1_conversation api_v1_msg api_v1_channel; do
  N=$(erl -pa "$ROOT/ebin" -pa "$ROOT/test" -noshell -eval \
    "io:format('~p', [length(${SUITE}_SUITE:all())]), halt(0)." 2>/dev/null || echo 0)
  EXPECTED_TOTAL=$((EXPECTED_TOTAL + N))
done
EVIDENCE_TOTAL=$(find "$EVIDENCE_DIR" -maxdepth 1 -name '*.json' | wc -l | tr -d ' ')
if [[ "$EVIDENCE_TOTAL" -ne "$EXPECTED_TOTAL" ]]; then
  echo "evidence cross-check: expected $EXPECTED_TOTAL evidence files, found $EVIDENCE_TOTAL" | tee -a "$RUNNER_LOG"
  EVIDENCE_STATUS=3
fi
if [[ "$EXPECTED_TOTAL" -eq 0 ]]; then
  echo "evidence cross-check: aggregate derivation got 0 cases (suite beams missing?)" | tee -a "$RUNNER_LOG"
  EVIDENCE_STATUS=3
fi
for case in $EXPECTED_CASES; do
  f="$EVIDENCE_DIR/$case.json"
  if [[ ! -f "$f" ]]; then
    echo "evidence cross-check: missing evidence for $case" | tee -a "$RUNNER_LOG"
    EVIDENCE_STATUS=3
  elif ! jq -e '.result == "PASS"' "$f" >/dev/null 2>&1; then
    echo "evidence cross-check: $case result is not PASS" | tee -a "$RUNNER_LOG"
    EVIDENCE_STATUS=3
  fi
done

# Skipped cases never reach the evidence writer, and init failures skip the
# whole suite; both are caught above. Belt-and-braces: CT prints a per-suite
# "K skipped" segment on its TEST COMPLETE line only when cases actually
# skipped, so scan those authoritative lines. A recursive scan of the CT dir
# false-positives forever: Common Test's own HTML templates (index.html
# column headers) and the cumulative all_runs.html history contain the word
# regardless of outcomes.
if grep -E 'TEST COMPLETE,' "$RUNNER_LOG" 2>/dev/null | grep -q 'skipped'; then
  echo "evidence cross-check: skip marker found under CT logs" | tee -a "$RUNNER_LOG"
  EVIDENCE_STATUS=3
fi

# Belt-and-braces (review P1): no JWT-shaped material may surface in the CT
# logs, the evidence dir, or the runner log. Suites keep credential fields
# out of anything CT logs (sanitized session handles); evidence redacts by key.
if grep -rqE 'eyJ[A-Za-z0-9_-]{20,}' "$CT_LOGS_DIR" "$EVIDENCE_DIR" "$RUNNER_LOG" 2>/dev/null; then
  echo "evidence cross-check: JWT-shaped material found in CT logs/evidence" | tee -a "$RUNNER_LOG"
  EVIDENCE_STATUS=3
fi

# Credential-echo gate (review round 4): the same init-return mechanism that
# once echoed JWTs into every suite log page also echoed the login suite's
# fixture password key. The eyJ shape above cannot see passwords, so scan
# for credential-key names directly. Source-listing pages legitimately
# contain the identifiers, so they are excluded (--exclude form is portable
# across BSD/GNU grep; a find|xargs form would hang on GNU xargs with empty
# input).
if grep -rq --exclude='*.src.html' 'plain_password' "$CT_LOGS_DIR" "$RUNNER_LOG" 2>/dev/null; then
  echo "evidence cross-check: credential keys echoed into CT log pages" | tee -a "$RUNNER_LOG"
  EVIDENCE_STATUS=3
fi

[[ $EVIDENCE_STATUS -ne 0 ]] && RUN_STATUS=3

# ---------------------------------------------------------------------------
# Aggregated result + environment manifests (RTF-05).
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# Redaction canaries (RTF-02-A4): dynamic values must not appear anywhere
# under the report root.
# ---------------------------------------------------------------------------

for canary_var in REST_REDACTION_CANARY_PASSWORD REST_REDACTION_CANARY_TOKEN; do
  canary=${!canary_var:-}
  if [[ -n "$canary" ]] && grep -rF -- "$canary" "$REPORT_ROOT" >/dev/null 2>&1; then
    echo "run_rest_api_tests: redaction canary $canary_var leaked into $REPORT_ROOT" >&2
    RUN_STATUS=3
  fi
done
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------

OTP_RELEASE=$(erl -noshell -noinput -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt(0).' 2>/dev/null || echo unknown)
PG_VERSION=$(psql_scratch -d "$DB_NAME" -Atc "SHOW server_version" || echo unknown)
EXTENSION_INVENTORY=$(psql_scratch -d "$DB_NAME" -Atc \
  "SELECT string_agg(extname || '=' || extversion, ',' ORDER BY extname) FROM pg_extension" || echo unknown)

CASE_TOTAL=${EVIDENCE_TOTAL}
if [[ "$CASE_TOTAL" -gt 0 ]]; then
  # A malformed evidence file must degrade to a totals mismatch (exit 3
  # with a full result.json), not abort the script before the remaining
  # gates run.
  CASE_PASS=$(jq -s 'map(select(.result == "PASS")) | length' "$EVIDENCE_DIR"/*.json | head -1) || CASE_PASS=0
else
  CASE_PASS=0
fi

# Residue assertion: this run leaves no scratch database behind.
# ---------------------------------------------------------------------------

cleanup
trap - EXIT INT TERM
DB_SCRAPE_FAILED=0
POST_DBS=$(scratch_databases)
if [[ "${REST_KEEP_DB:-0}" == "1" ]]; then
  : # operator-owned diagnostic residue: the kept scratch DB is intentional
elif [[ "$DB_SCRAPE_FAILED" == "1" || "${CLEANUP_FAILED:-0}" == "1" ]]; then
  # A scrape that cannot run makes residue unverifiable; a failed drop was
  # already reported loudly by cleanup. Both are non-zero outcomes
  # (reverdict 2026-09-23, finding 3) — never a silent pass.
  echo "run_rest_api_tests: residue verification unavailable (scrape/cleanup failure)" >&2
  RUN_STATUS=3
elif grep -qx "$DB_NAME" <<<"$POST_DBS"; then
  # The invariant is scoped to THIS run's scratch database only: a
  # concurrent or prior run's imboy_rest_% entry appearing/disappearing
  # between PRE_DBS and POST_DBS is not residue we own (foreign resources
  # are recorded, never policed), and a transient psql failure during a
  # scrape must not fail the run either.
  echo "run_rest_api_tests: residue detected (own scratch DB still present): $DB_NAME" >&2
  RUN_STATUS=3
fi

# result.json is written ONCE, after every gate, so result/exit_code are the
# single machine truth of the whole run (reverdict 2026-09-23, finding 2).
jq -n \
  --arg run_id "$RUN_ID" \
  --arg db "$DB_NAME" \
  --arg base_sha "$REST_COMMIT_SHA" \
  --arg otp "$OTP_RELEASE" \
  --arg pg "$PG_VERSION" \
  --arg ext "$EXTENSION_INVENTORY" \
  --arg started "$STARTED_AT" \
  --arg finished "$FINISHED_AT" \
  --argjson case_total "${CASE_TOTAL:-0}" \
  --argjson case_pass "${CASE_PASS:-0}" \
  --argjson exit_code "$RUN_STATUS" \
  --argjson tables "${TABLES_COUNT:-0}" \
  --arg migtable "${MIGRATION_TABLE:-none}" \
  '{
    schema_version: 1,
    run_id: $run_id,
    base_sha: $base_sha,
    otp_release: $otp,
    postgres_version: $pg,
    extensions: ($ext | if . == "unknown" then {} else (split(",") | map(split("=") | {(.[0]): .[1]}) | add) end),
    command: "make rest-api-test",
    exit_code: $exit_code,
    case_total: $case_total,
    case_pass: $case_pass,
    case_fail: ($case_total - $case_pass),
    case_skip: 0,
    result: (if $exit_code == 0 and $case_pass == $case_total and $case_total > 0 then "PASS" elif $exit_code == 75 then "BLOCKED_SHARED_CT" else "FAIL" end),
    scratch_database: $db,
    public_tables: $tables,
    migration_table: $migtable,
    started_at: $started,
    finished_at: $finished
  }' >"$REPORT_ROOT/result.json"

jq -n \
  --arg run_id "$RUN_ID" \
  --arg base_sha "$REST_COMMIT_SHA" \
  --arg otp "$OTP_RELEASE" \
  --arg pg "$PG_VERSION" \
  --arg ext "$EXTENSION_INVENTORY" \
  --arg ct_config "$CT_CONFIG" \
  '{
    run_id: $run_id,
    base_sha: $base_sha,
    otp_release: $otp,
    postgres_version: $pg,
    extensions: ($ext | if . == "unknown" then {} else (split(",") | map(split("=") | {(.[0]): .[1]}) | add) end),
    ct_config: $ct_config,
    credentials: "environment only (REST_PG_*/IMBOY_PG_*)"
  }' >"$REPORT_ROOT/environment.json"

echo "REST run $RUN_ID => $(jq -r '.result' "$REPORT_ROOT/result.json") (case_pass=$(jq -r '.case_pass' "$REPORT_ROOT/result.json")/$(jq -r '.case_total' "$REPORT_ROOT/result.json"))"
exit "$RUN_STATUS"
