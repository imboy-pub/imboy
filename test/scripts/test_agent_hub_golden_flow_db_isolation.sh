#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT/scripts/agent_hub_golden_flow.sh"
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
  ai_agent_tool_loop_tests
  mcp_authz_gate_tests
  imboy_mcp_task_tools_tests
  agent_task_repo_tests
  agent_task_logic_tests
  bot_group_mention_tests
  bot_logic_tests
  bot_webhook_logic_tests
  bot_webhook_delivery_repo_tests
  bot_webhook_delivery_worker_tests
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

assert_rejected_before_psql() {
  name="$1"
  shift
  marker="$TEST_TMPDIR/$name.psql-called"
  evidence="$TEST_TMPDIR/$name-evidence"
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

echo "golden flow DB isolation contract PASS"
