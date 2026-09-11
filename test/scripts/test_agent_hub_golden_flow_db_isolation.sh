#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT/scripts/agent_hub_golden_flow.sh"
EVIDENCE_WRITER="$ROOT/scripts/write_agent_hub_e2e_evidence.py"
CHANNEL_CLIENT="$ROOT/scripts/agent_hub_channel_webhook_smoke.py"
BOT_FIXTURE="$ROOT/scripts/agent_hub_bot_webhook_fixture.py"
WS_CLIENT="$ROOT/scripts/smoke/ws_c2g_send.py"
FAKE_LLM="$ROOT/test/fixtures/agent_hub/agent_hub_fake_llm.erl"
TEST_TMPDIR="${TEST_TMPDIR:?TEST_TMPDIR is required}"
STUB_BIN="$TEST_TMPDIR/bin"
mkdir -p "$STUB_BIN"

cat > "$STUB_BIN/psql" <<'EOF'
#!/usr/bin/env bash
touch "$PSQL_CALLED_MARKER"
exit 99
EOF
chmod +x "$STUB_BIN/psql"

# shellcheck disable=SC2016 # Match literal variables in the target script.
grep -Fq 'PGDATABASE="$DB" IMBOY_DIR="$ROOT" "$ROOT/scripts/drill_migrate.escript" up' "$SCRIPT" || {
  echo "strict migration driver does not target the scratch DB" >&2
  exit 1
}

awk '
  /^export IMBOY_PG_DATABASE="\$DB"$/ { bound = 1 }
  bound && /EUNIT_CONFIG="\$EUNIT_CONFIG" make -C "\$ROOT" eunit-local/ { eunit = 1 }
  END { exit !(bound && eunit) }
' "$SCRIPT" || {
  echo "eunit-local is not bound to the scratch DB and ephemeral config" >&2
  exit 1
}

# shellcheck disable=SC2016 # Match literal variables in the target script.
grep -Fq 'cp "$ROOT/config/sys.config.example" "$CONFIG_TMP_DIR/sys.eunit.config"' "$SCRIPT" || {
  echo "golden flow does not create an isolated EUnit config" >&2
  exit 1
}
# shellcheck disable=SC2016 # Match a literal variable in the target script.
grep -Fq 'imboy-ah-e2e-config.*) rm -rf -- "$CONFIG_TMP_DIR"' "$SCRIPT" || {
  echo "golden flow does not clean its isolated EUnit config" >&2
  exit 1
}

REQUIRED_SUITES=(
  channel_webhook_logic_tests
  ai_agent_reply_tests
  ai_agent_group_reply_tests
  ai_agent_tool_loop_tests
  mcp_authz_gate_tests
  mcp_governance_logic_tests
  mcp_client_repo_tests
  imboy_mcp_task_tools_tests
  agent_task_repo_tests
  agent_task_logic_tests
  bot_group_mention_tests
  bot_logic_tests
  bot_webhook_logic_tests
  bot_webhook_delivery_repo_tests
  bot_webhook_delivery_worker_tests
  bot_webhook_delivery_sender_tests
  bot_webhook_guard_tests
  bot_handler_tests
  bot_e2e_tests
  agent_hub_runtime_trace_tests
)
for suite in "${REQUIRED_SUITES[@]}"; do
  grep -Fq "$suite" "$SCRIPT" || {
    echo "golden flow missing required suite: $suite" >&2
    exit 1
  }
done

grep -Fq 'bearer[[:space:]]+' "$SCRIPT" || {
  echo "golden flow does not scan credential-shaped log content" >&2
  exit 1
}
grep -Fq '1[3-9][0-9]{9}' "$SCRIPT" || {
  echo "golden flow does not scan mobile-shaped log content" >&2
  exit 1
}
grep -Fq '(^|[^0-9])1[3-9][0-9]{9}([^0-9]|$)' "$SCRIPT" || {
  echo "mobile scan can false-positive on longer TSID/timestamp values" >&2
  exit 1
}

grep -Fq 'FINAL_STATUS="PARTIAL"' "$SCRIPT" || {
  echo "golden flow does not settle honest PARTIAL evidence" >&2
  exit 1
}
grep -Fq -- '--final-integrated-base' "$SCRIPT" || {
  echo "golden flow has no explicit final integrated Base mode" >&2
  exit 1
}
grep -Fq 'CURRENT_BRANCH" == "main"' "$SCRIPT" || {
  echo "final integrated Base mode is not pinned to main" >&2
  exit 1
}
grep -Fq 'agent_hub_ext01_mcp_client_smoke.py' "$SCRIPT" || {
  echo "golden flow does not run the real HTTP MCP client" >&2
  exit 1
}
grep -Fq 'agent_hub_http_status_fixture.py' "$SCRIPT" || {
  echo "golden flow does not run real webhook status fixtures" >&2
  exit 1
}
grep -Fq 'agent_hub_delivery_replay_smoke.py' "$SCRIPT" || {
  echo "golden flow does not run the admin replay endpoint" >&2
  exit 1
}
grep -Fq -- '--protocol-negatives-passed' "$SCRIPT" || {
  echo "golden flow does not bind protocol negative evidence" >&2
  exit 1
}
grep -Fq 'agent_hub_channel_webhook_smoke.py' "$SCRIPT" || {
  echo "golden flow does not run the real channel webhook HTTP client" >&2
  exit 1
}
grep -Fq 'channel-webhook-a02-db.json' "$SCRIPT" || {
  echo "golden flow does not verify channel webhook persistence" >&2
  exit 1
}
grep -Fq -- '--channel-webhook-passed' "$SCRIPT" || {
  echo "golden flow does not bind channel webhook evidence" >&2
  exit 1
}
grep -Fq 'WS_MENTIONS=' "$SCRIPT" || {
  echo "golden flow does not run a real WebSocket Agent mention" >&2
  exit 1
}
grep -Fq 'agent-dialog-a02-db.json' "$SCRIPT" || {
  echo "golden flow does not verify Agent reply persistence" >&2
  exit 1
}
grep -Fq -- '--agent-dialog-passed' "$SCRIPT" || {
  echo "golden flow does not bind Agent dialog evidence" >&2
  exit 1
}
grep -Fq 'agent_hub_bot_webhook_fixture.py' "$SCRIPT" || {
  echo "golden flow does not run the real Bot webhook fixture" >&2
  exit 1
}
grep -Fq 'bot-dialog-a02-db.json' "$SCRIPT" || {
  echo "golden flow does not verify Bot delivery and reply persistence" >&2
  exit 1
}
grep -Fq -- '--bot-dialog-passed' "$SCRIPT" || {
  echo "golden flow does not bind Bot dialog evidence" >&2
  exit 1
}
grep -Fq 'stop_backend' "$SCRIPT" || {
  echo "golden flow does not stop and restart the real backend" >&2
  exit 1
}
grep -Fq 'connect_ex(("127.0.0.1"' "$SCRIPT" || {
  echo "restart port check can confuse TIME_WAIT with a live listener" >&2
  exit 1
}
grep -Fq 'restart-before.json' "$SCRIPT" || {
  echo "golden flow does not compare restart persistence snapshots" >&2
  exit 1
}
grep -Fq -- '--restart-passed' "$SCRIPT" || {
  echo "golden flow does not bind restart evidence into evidence.json" >&2
  exit 1
}
grep -Fq 'verify_agent_hub_correlation_trace.py' "$SCRIPT" || {
  echo "golden flow does not verify a runtime correlation export" >&2
  exit 1
}
grep -Fq 'export_agent_hub_correlation_trace.sql' "$SCRIPT" || {
  echo "golden flow does not export persisted runtime correlation records" >&2
  exit 1
}
grep -Fq 'A01-A07 passed on the final integrated main Base' "$SCRIPT" || {
  echo "golden flow cannot report final integrated PASS" >&2
  exit 1
}
test -f "$EVIDENCE_WRITER" || {
  echo "golden flow evidence writer is missing" >&2
  exit 1
}
test -f "$CHANNEL_CLIENT" || {
  echo "channel webhook HTTP client is missing" >&2
  exit 1
}
test -f "$BOT_FIXTURE" || {
  echo "Bot webhook fixture is missing" >&2
  exit 1
}
test -f "$WS_CLIENT" || {
  echo "WebSocket C2G client is missing" >&2
  exit 1
}
test -f "$FAKE_LLM" || {
  echo "local fake LLM fixture is missing" >&2
  exit 1
}

assert_rejected_before_psql() {
  name="$1"
  shift
  marker="$TEST_TMPDIR/$name.psql-called"
  evidence="$TEST_TMPDIR/$name-evidence/E2E-01"
  output="$TEST_TMPDIR/$name.log"
  set +e
  env -u PGHOSTADDR -u PGSERVICE -u PGSERVICEFILE \
    PATH="$STUB_BIN:$PATH" PSQL_CALLED_MARKER="$marker" \
    PGHOST=127.0.0.1 "$@" \
    bash "$SCRIPT" --evidence-dir "$evidence" > "$output" 2>&1
  code=$?
  set -e
  [ "$code" -eq 2 ] || {
    echo "$name did not fail closed with exit 2 (got $code)" >&2
    exit 1
  }
  [ ! -e "$marker" ] || {
    echo "$name reached psql" >&2
    exit 1
  }
  [ ! -e "$evidence" ] || {
    echo "$name created evidence before validation" >&2
    exit 1
  }
}

assert_rejected_before_psql remote-hostaddr PGHOSTADDR=192.0.2.10
assert_rejected_before_psql service PGSERVICE=remote-service
assert_rejected_before_psql service-file PGSERVICEFILE="$TEST_TMPDIR/pg_service.conf"
assert_rejected_before_psql localhost PGHOST=localhost

workspace_marker="$TEST_TMPDIR/workspace-evidence.psql-called"
workspace_evidence="$ROOT/test/.agent-hub-evidence-should-not-exist"
set +e
env -u PGHOSTADDR -u PGSERVICE -u PGSERVICEFILE \
  PATH="$STUB_BIN:$PATH" PSQL_CALLED_MARKER="$workspace_marker" \
  PGHOST=127.0.0.1 bash "$SCRIPT" --evidence-dir "$workspace_evidence" \
  > "$TEST_TMPDIR/workspace-evidence.log" 2>&1
code=$?
set -e
[ "$code" -eq 2 ] || {
  echo "workspace evidence path did not fail closed with exit 2 (got $code)" >&2
  exit 1
}
[ ! -e "$workspace_marker" ] || {
  echo "workspace evidence rejection reached psql" >&2
  exit 1
}
[ ! -e "$workspace_evidence" ] || {
  echo "workspace evidence path was created before rejection" >&2
  exit 1
}

# Once a valid local run starts, PostgreSQL failure must produce valid FAIL
# evidence and must not claim cleanup succeeded when the residual query failed.
failure_marker="$TEST_TMPDIR/failure.psql-called"
failure_evidence="$TEST_TMPDIR/failure/E2E-01"
set +e
env -u PGHOSTADDR -u PGSERVICE -u PGSERVICEFILE \
  PATH="$STUB_BIN:$PATH" PSQL_CALLED_MARKER="$failure_marker" \
  PGHOST=127.0.0.1 bash "$SCRIPT" --evidence-dir "$failure_evidence" \
  > "$TEST_TMPDIR/failure.log" 2>&1
code=$?
set -e
[ "$code" -eq 99 ] || {
  echo "PostgreSQL failure exit code was not preserved (got $code)" >&2
  exit 1
}
[ -e "$failure_marker" ] || {
  echo "PostgreSQL failure fixture did not reach psql" >&2
  exit 1
}
set +e
python3 "$ROOT/scripts/verify_agent_hub_task_evidence.py" \
  --task "$failure_evidence/evidence.json" > "$TEST_TMPDIR/failure-verdict.json"
verifier_code=$?
set -e
[ "$verifier_code" -eq 1 ] || {
  echo "FAIL evidence was not structurally valid (got $verifier_code)" >&2
  exit 1
}
grep -Fq '"decision": "FAIL"' "$TEST_TMPDIR/failure-verdict.json" || {
  echo "PostgreSQL failure evidence was not classified FAIL" >&2
  exit 1
}
grep -Fq 'cleanup_passed=0' "$failure_evidence/run-summary.txt" || {
  echo "failed cleanup query was treated as successful" >&2
  exit 1
}

echo "golden flow DB isolation contract PASS"
