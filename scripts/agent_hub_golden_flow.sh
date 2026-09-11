#!/usr/bin/env bash
# E2E-01：Agent Hub 本地 Golden Flow harness。
# 从空 scratch 环境复现：建库 → 迁移 → 核心套件 → 真实 HTTP/重启 → 证据 → 清理。
# 所有资源带 marker（库名前缀 imboy_ah_e2e_），清理只删除带 marker 的对象。
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
WORKSPACE_ROOT="$(cd "$ROOT/.." && pwd -P)"
PROFILE="local-fixture"
FINAL_INTEGRATED_BASE=0
EVIDENCE_DIR="${IMBOY_EVIDENCE_ROOT:-${TMPDIR:-/tmp}/imboy-agent-hub}/E2E-01"
MARKER_DB_PREFIX="imboy_ah_e2e_"
CONFIG_TMP_DIR=""
CURRENT_STEP="initialization"
RUN_FINISHED=0
SUITES_PASSED=0
TRACE_EXIT=2
CLEANUP_PASSED=0
CLEANUP_DONE=0
SENSITIVE_SCAN_PASSED=0
HTTP_SMOKE_PASSED=0
CHANNEL_WEBHOOK_PASSED=0
AGENT_DIALOG_PASSED=0
BOT_DIALOG_PASSED=0
PROTOCOL_NEGATIVES_PASSED=0
RESTART_PASSED=0
BACKEND_PID=""
BOT_FIXTURE_PID=""
STATUS_FIXTURE_PID=""
BACKEND_NODE="imboy_ah_e2e_$$_runtime"
BACKEND_COOKIE=""
BACKEND_DIST_PORT=""
RUNTIME_HTTP_PORT="${IMBOY_AGENT_HUB_HTTP_PORT:-19862}"
ERL_CALL=""
FIXTURE_EBIN=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --profile) PROFILE="$2"; shift 2 ;;
    --profile=*) PROFILE="${1#*=}"; shift ;;
    --final-integrated-base) FINAL_INTEGRATED_BASE=1; shift ;;
    --evidence-dir) EVIDENCE_DIR="$2"; shift 2 ;;
    --evidence-dir=*) EVIDENCE_DIR="${1#*=}"; shift ;;
    *) echo "unknown arg: $1" >&2; exit 2 ;;
  esac
done

EVIDENCE_DIR="$(python3 -c 'import os, sys; print(os.path.realpath(sys.argv[1]))' "$EVIDENCE_DIR")"
if [[ "${EVIDENCE_DIR##*/}" != "E2E-01" ]]; then
  echo "[golden] evidence directory basename must be E2E-01" >&2
  exit 2
fi
case "$EVIDENCE_DIR/" in
  "$WORKSPACE_ROOT/"*)
    echo "[golden] refusing evidence directory inside workspace" >&2
    exit 2
    ;;
esac

FINAL_INTEGRATED_PATHS=(
  priv/migrations/00000107_agent_hub_runtime_audit.down.sql
  priv/migrations/00000107_agent_hub_runtime_audit.up.sql
  docs/operations/agent-hub-local-golden-flow.md
  scripts/agent_hub_delivery_replay_smoke.py
  scripts/agent_hub_ext01_mcp_client_smoke.py
  scripts/agent_hub_golden_flow.sh
  scripts/agent_hub_http_status_fixture.py
  scripts/export_agent_hub_correlation_trace.sql
  scripts/write_agent_hub_e2e_evidence.py
  src/lib/bot_webhook_delivery_sender.erl
  src/logic/bot_webhook_delivery_worker.erl
  src/logic/agent_task_logic.erl
  src/logic/mcp_governance_logic.erl
  src/mcp/barrel_mcp_registry.erl
  src/mcp/imboy_mcp_tools.erl
  src/mcp/mcp_authz_gate.erl
  src/repo/agent_hub_audit_repo.erl
  src/repo/agent_task_repo.erl
  src/repo/bot_webhook_delivery_repo.erl
  test/api/mcp_handler_auth_tests.erl
  test/integration/agent_hub_runtime_trace_tests.erl
  test/logic/agent_task_logic_tests.erl
  test/logic/mcp_governance_logic_tests.erl
  test/mcp/barrel_mcp_protocol_tests.erl
  test/mcp/imboy_mcp_task_tools_tests.erl
  test/mcp/mcp_authz_gate_tests.erl
  test/repo/bot_webhook_delivery_repo_tests.erl
  test/scripts/test_agent_hub_golden_flow_db_isolation.sh
  test/scripts/test_agent_hub_http_status_fixture.py
  test/scripts/test_write_agent_hub_e2e_evidence.py
)
if [[ "$FINAL_INTEGRATED_BASE" -eq 1 ]]; then
  CURRENT_BRANCH="$(git -C "$ROOT" symbolic-ref --quiet --short HEAD || true)"
  [[ "$CURRENT_BRANCH" == "main" ]] || {
    echo "[golden] final integrated Base must run on main" >&2
    exit 2
  }
  [[ -z "$(git -C "$ROOT" status --porcelain -- "${FINAL_INTEGRATED_PATHS[@]}")" ]] || {
    echo "[golden] final integrated Base candidate paths differ from HEAD" >&2
    exit 2
  }
fi

DB="${MARKER_DB_PREFIX}$(date +%s)_$$"
CHANNEL_FIXTURE_ID=$((91100000000000000 + $$))
CHANNEL_WEBHOOK_TEXT="agent-hub-channel-webhook-$DB"
AGENT_HUMAN_UID=$((91200000000000000 + $$))
MCP_OTHER_UID=$((91250000000000000 + $$))
AGENT_GROUP_ID=$((91300000000000000 + $$))
AGENT_PROMPT="agent-hub-local-prompt-$$_$(date +%s)"
AGENT_REPLY="agent-hub-local-reply-$$_$(date +%s)"
AGENT_MSG_ID="ah-agent-$$_$(date +%s)"
BOT_MSG_ID="ah-bot-$$_$(date +%s)"
BOT_PLAIN_MSG_ID="ah-bot-plain-$$_$(date +%s)"
BOT_DISABLED_MSG_ID="ah-bot-disabled-$$_$(date +%s)"
BOT_NONMEMBER_MSG_ID="ah-bot-nonmember-$$_$(date +%s)"
BOT_E2EE_MSG_ID="ah-bot-e2ee-$$_$(date +%s)"
NEGATIVE_TAG="$$_$(date +%s)"
NEGATIVE_CORR="corr-negative-$NEGATIVE_TAG"
NEGATIVE_TASK_ID="task-negative-$NEGATIVE_TAG"
NEGATIVE_EVENT_ID="event-negative-$NEGATIVE_TAG"
WEBHOOK_5XX_DELIVERY_ID="dlv-5xx-$NEGATIVE_TAG"
WEBHOOK_4XX_DELIVERY_ID="dlv-4xx-$NEGATIVE_TAG"
E2EE_TASK_ID="task-e2ee-$NEGATIVE_TAG"
BOT_NONMEMBER_GROUP_ID=$((91400000000000000 + $$))
BOT_PROMPT="agent-hub-local-bot-prompt-$$_$(date +%s)"
BOT_REPLY="agent-hub-local-bot-reply-$$_$(date +%s)"
PGHOST="${PGHOST:-127.0.0.1}"
PGPORT="${PGPORT:-4323}"
PGUSER="${PGUSER:-imboy_user}"
export PGPASSWORD="${PGPASSWORD:-abc54321}"

case "$PGHOST" in
  127.0.0.1|::1) ;;
  *) echo "[golden] refusing non-loopback PostgreSQL host" >&2; exit 2 ;;
esac
if [[ -n "${PGHOSTADDR:-}" || -n "${PGSERVICE:-}" || -n "${PGSERVICEFILE:-}" ]]; then
  echo "[golden] refusing libpq connection target override" >&2
  exit 2
fi
unset PGHOSTADDR PGSERVICE PGSERVICEFILE

if [[ ! "$RUNTIME_HTTP_PORT" =~ ^[0-9]+$ ]] ||
   (( RUNTIME_HTTP_PORT < 1024 || RUNTIME_HTTP_PORT > 65535 )); then
  echo "[golden] invalid private runtime HTTP port" >&2
  exit 2
fi

PSQL="psql -h $PGHOST -p $PGPORT -U $PGUSER -v ON_ERROR_STOP=1 -q"
BASE_IMBOY="$(git -C "$ROOT" rev-parse HEAD)"
BASE_IMBOYAPP="$(git -C "$WORKSPACE_ROOT/imboyapp" rev-parse HEAD)"
BASE_IMBOYADMIN="$(git -C "$WORKSPACE_ROOT/imboyadmin" rev-parse HEAD)"

stop_backend() {
  local failed=0
  if [[ -z "$BACKEND_PID" ]]; then
    return 0
  fi
  if kill -0 "$BACKEND_PID" 2>/dev/null; then
    if [[ -n "$BACKEND_DIST_PORT" && -x "$ERL_CALL" ]]; then
      "$ERL_CALL" -address "127.0.0.1:$BACKEND_DIST_PORT" \
        -c "$BACKEND_COOKIE" -q -timeout 10 >/dev/null 2>&1 || failed=1
    fi
    for _ in $(seq 1 40); do
      kill -0 "$BACKEND_PID" 2>/dev/null || break
      sleep 0.25
    done
    if kill -0 "$BACKEND_PID" 2>/dev/null; then
      kill "$BACKEND_PID" 2>/dev/null || true
      failed=1
    fi
  else
    failed=1
  fi
  wait "$BACKEND_PID" 2>/dev/null || failed=1
  BACKEND_PID=""
  BACKEND_DIST_PORT=""
  [[ "$failed" -eq 0 ]]
}

stop_bot_fixture() {
  if [[ -z "$BOT_FIXTURE_PID" ]]; then
    return 0
  fi
  if kill -0 "$BOT_FIXTURE_PID" 2>/dev/null; then
    kill "$BOT_FIXTURE_PID" 2>/dev/null || true
  fi
  wait "$BOT_FIXTURE_PID" 2>/dev/null || true
  BOT_FIXTURE_PID=""
}

stop_status_fixture() {
  if [[ -z "$STATUS_FIXTURE_PID" ]]; then
    return 0
  fi
  if kill -0 "$STATUS_FIXTURE_PID" 2>/dev/null; then
    kill "$STATUS_FIXTURE_PID" 2>/dev/null || true
  fi
  wait "$STATUS_FIXTURE_PID" 2>/dev/null || true
  STATUS_FIXTURE_PID=""
}

start_backend() {
  local log_path="$1"
  local runtime_secret
  local pa_args=(-pa "$ROOT/ebin")
  if [[ -n "$FIXTURE_EBIN" ]]; then
    pa_args+=(-pa "$FIXTURE_EBIN")
  fi
  for ebin_dir in "$ROOT"/deps/*/ebin; do
    pa_args+=(-pa "$ebin_dir")
  done
  python3 -c \
    'import socket,sys; s=socket.socket(); rc=s.connect_ex(("127.0.0.1", int(sys.argv[1]))); s.close(); raise SystemExit(rc == 0)' \
    "$RUNTIME_HTTP_PORT"
  runtime_secret="$(printf 'agent-hub-local:%s' "$DB" | shasum -a 256 | awk '{print $1}')"
  BACKEND_COOKIE="ah_${runtime_secret:0:30}"
  IMBOYENV=local HTTP_PORT="$RUNTIME_HTTP_PORT" \
    IMBOY_PG_HOST="$PGHOST" IMBOY_PG_PORT="$PGPORT" \
    IMBOY_PG_USERNAME="$PGUSER" IMBOY_PG_PASSWORD="$PGPASSWORD" \
    IMBOY_PG_DATABASE="$DB" IMBOY_AUTO_MIGRATE=false \
    IMBOY_PRODUCT_PROFILE=enterprise \
    IMBOY_ADM_COOKIE_SECRET="adm:$runtime_secret" \
    IMBOY_POSTGRE_AES_KEY="aes:$runtime_secret" \
    IMBOY_JWT_KEY="jwt:$runtime_secret" \
    erl -noshell -sname "$BACKEND_NODE" -setcookie "$BACKEND_COOKIE" \
      -config "$EUNIT_CONFIG" "${pa_args[@]}" \
      -eval 'case application:ensure_all_started(imboy) of {ok, _} -> ok; Error -> io:format("START_FAILED ~p~n", [Error]), halt(2) end, timer:sleep(infinity).' \
      > "$log_path" 2>&1 &
  BACKEND_PID=$!
  for _ in $(seq 1 60); do
    if curl -fsS "http://127.0.0.1:$RUNTIME_HTTP_PORT/healthz" >/dev/null 2>&1; then
      break
    fi
    kill -0 "$BACKEND_PID" 2>/dev/null || return 1
    sleep 0.5
  done
  curl -fsS "http://127.0.0.1:$RUNTIME_HTTP_PORT/healthz" >/dev/null
  for _ in $(seq 1 20); do
    BACKEND_DIST_PORT="$(epmd -names | awk -v node="$BACKEND_NODE" \
      '$2 == node {print $5}')"
    [[ -n "$BACKEND_DIST_PORT" ]] && break
    sleep 0.25
  done
  [[ -n "$BACKEND_DIST_PORT" ]]
}

issue_admin_cookie() {
  printf '%s\n' \
    'io:format("~s", [adm_auth_middleware:sign_admin_cookie(<<"700001">>)]).' \
    | "$ERL_CALL" -address "127.0.0.1:$BACKEND_DIST_PORT" \
        -c "$BACKEND_COOKIE" -e -fetch_stdout -no_result_term -timeout 5
}

create_channel_webhook() {
  local name="$1"
  printf 'case channel_webhook_ds:create(%s, <<"%s">>, 1) of {ok, #{<<"id">> := Id, <<"token">> := Token, <<"bot_uid">> := BotUid}} -> io:format("~s ~B ~B", [Token, Id, BotUid]); Error -> io:format("ERROR ~p", [Error]) end.\n' \
    "$CHANNEL_FIXTURE_ID" "$name" \
    | "$ERL_CALL" -address "127.0.0.1:$BACKEND_DIST_PORT" \
        -c "$BACKEND_COOKIE" -e -fetch_stdout -no_result_term -timeout 10
}

issue_user_token() {
  local uid="$1"
  printf 'io:format("~s", [token_ds:encrypt_token(%s)]).\n' "$uid" \
    | "$ERL_CALL" -address "127.0.0.1:$BACKEND_DIST_PORT" \
        -c "$BACKEND_COOKIE" -e -fetch_stdout -no_result_term -timeout 5
}

create_fake_agent() {
  printf 'application:set_env(imboy, llm_providers, [#{name => <<"agent_hub_fake">>, module => agent_hub_fake_llm, expected_prompt => <<"%s">>, reply => <<"%s">>}]), case ai_agent_ds:create(#{<<"nickname">> => <<"Agent Hub Fake Agent">>, <<"account">> => <<"agent-hub-fake-%s">>, <<"provider">> => <<"agent_hub_fake">>, <<"owner_uid">> => %s, <<"trigger_policy">> => #{<<"mention">> => true}, <<"capabilities">> => #{<<"group_reply">> => true}}) of {ok, #{<<"user_id">> := Uid}} -> io:format("~B", [Uid]); Error -> io:format("ERROR ~p", [Error]) end.\n' \
    "$AGENT_PROMPT" "$AGENT_REPLY" "$DB" "$AGENT_HUMAN_UID" \
    | "$ERL_CALL" -address "127.0.0.1:$BACKEND_DIST_PORT" \
        -c "$BACKEND_COOKIE" -e -fetch_stdout -no_result_term -timeout 10
}

create_bot() {
  local webhook_url="$1"
  printf 'case bot_logic:register(#{name => <<"Agent Hub Local Bot">>, username => <<"agent-hub-local-%s">>, owner_uid => %s, webhook_url => <<"%s">>, events => jsone:encode([<<"message.c2g_mention">>])}) of {ok, #{<<"user_id">> := Uid, <<"api_token">> := Api, <<"verify_token">> := Verify}} -> io:format("~B ~s ~s", [Uid, Api, Verify]); Error -> io:format("ERROR ~p", [Error]) end.\n' \
    "$DB" "$AGENT_HUMAN_UID" "$webhook_url" \
    | "$ERL_CALL" -address "127.0.0.1:$BACKEND_DIST_PORT" \
        -c "$BACKEND_COOKIE" -e -fetch_stdout -no_result_term -timeout 10
}

insert_runtime_delivery() {
  local delivery_id="$1"
  local correlation_id="$2"
  local webhook_url="$3"
  printf 'io:format("~p", [bot_webhook_delivery_repo:insert(#{delivery_id => <<"%s">>, bot_id => %s, event_type => <<"agent_task.completed">>, payload => <<"{}">>, correlation_id => <<"%s">>, idempotency_key => <<"negative:%s">>, webhook_url => <<"%s">>, webhook_host => <<"127.0.0.1">>, pinned_ip => <<"127.0.0.1">>})]).\n' \
    "$delivery_id" "$BOT_UID" "$correlation_id" "$delivery_id" "$webhook_url" \
    | "$ERL_CALL" -address "127.0.0.1:$BACKEND_DIST_PORT" \
        -c "$BACKEND_COOKIE" -e -fetch_stdout -no_result_term -timeout 10
}

execute_runtime_delivery() {
  local delivery_id="$1"
  local count="$2"
  printf 'lists:foreach(fun(_) -> {ok, Delivery} = bot_webhook_delivery_repo:get_delivery(<<"%s">>), _ = bot_webhook_delivery_worker:execute(Delivery) end, lists:seq(1, %s)), io:format("ok").\n' \
    "$delivery_id" "$count" \
    | "$ERL_CALL" -address "127.0.0.1:$BACKEND_DIST_PORT" \
        -c "$BACKEND_COOKIE" -e -fetch_stdout -no_result_term -timeout 20
}

snapshot_restart_state() {
  $PSQL -d "$DB" -At -v correlation_id="$TRACE_CORR" <<'SQL'
SELECT jsonb_pretty(jsonb_build_object(
  'task', (
    SELECT jsonb_build_object('id', id, 'status', status, 'correlation_id', correlation_id)
    FROM public.agent_task WHERE correlation_id = :'correlation_id'
  ),
  'decision', (
    SELECT jsonb_build_object('task_id', task_id, 'decision', decision, 'correlation_id', correlation_id)
    FROM public.agent_task_decision WHERE correlation_id = :'correlation_id'
  ),
  'delivery', (
    SELECT jsonb_build_object('delivery_id', delivery_id, 'status', status, 'correlation_id', correlation_id)
    FROM public.bot_delivery WHERE correlation_id = :'correlation_id'
  )
));
SQL
}

cleanup() {
  local failed=0
  local remaining=""
  if [[ "$CLEANUP_DONE" -eq 1 && "$CLEANUP_PASSED" -eq 1 ]]; then
    return
  fi
  stop_bot_fixture
  stop_status_fixture
  stop_backend || failed=1
  echo "[golden] cleanup: drop $DB"
  psql -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" -d postgres \
    -c "DROP DATABASE IF EXISTS $DB" > /dev/null 2>&1 || failed=1
  case "$CONFIG_TMP_DIR" in
    "${TMPDIR:-/tmp}"/imboy-ah-e2e-config.*) rm -rf -- "$CONFIG_TMP_DIR" ;;
  esac
  if ! remaining="$(psql -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" -d postgres -tAc \
      "SELECT 1 FROM pg_database WHERE datname='$DB'" 2>/dev/null)"; then
    failed=1
  elif [[ "$remaining" == *1* ]]; then
    failed=1
  fi
  if [[ -n "$CONFIG_TMP_DIR" && -e "$CONFIG_TMP_DIR" ]]; then
    failed=1
  fi
  [[ "$failed" -eq 0 ]] && CLEANUP_PASSED=1 || CLEANUP_PASSED=0
  CLEANUP_DONE=1
}

write_manifest() {
  (
    cd "$EVIDENCE_DIR"
    find . -type f ! -name 'manifest.sha256*' -exec shasum -a 256 {} \; \
      > manifest.sha256.tmp
    mv manifest.sha256.tmp manifest.sha256
  )
}

write_evidence() {
  local status="$1"
  local failed_code="${2:-0}"
  python3 "$ROOT/scripts/write_agent_hub_e2e_evidence.py" \
    --evidence-dir "$EVIDENCE_DIR" --repo-root "$ROOT" --status "$status" \
    --imboy-sha "$BASE_IMBOY" --imboyapp-sha "$BASE_IMBOYAPP" \
    --imboyadmin-sha "$BASE_IMBOYADMIN" --suites-passed "$SUITES_PASSED" \
    --trace-exit "$TRACE_EXIT" --cleanup-passed "$CLEANUP_PASSED" \
    --sensitive-scan-passed "$SENSITIVE_SCAN_PASSED" \
    --http-smoke-passed "$HTTP_SMOKE_PASSED" \
    --channel-webhook-passed "$CHANNEL_WEBHOOK_PASSED" \
    --agent-dialog-passed "$AGENT_DIALOG_PASSED" \
    --bot-dialog-passed "$BOT_DIALOG_PASSED" \
    --protocol-negatives-passed "$PROTOCOL_NEGATIVES_PASSED" \
    --restart-passed "$RESTART_PASSED" \
    --final-integrated-base "$FINAL_INTEGRATED_BASE" \
    --failed-step "$CURRENT_STEP" --failed-code "$failed_code"
}

# shellcheck disable=SC2329 # Invoked indirectly by the EXIT trap below.
on_exit() {
  local code="$?"
  trap - EXIT
  set +e
  cleanup
  if [[ "$RUN_FINISHED" -eq 0 ]]; then
    write_evidence FAIL "$code"
    python3 "$ROOT/scripts/verify_agent_hub_task_evidence.py" \
      --task "$EVIDENCE_DIR/evidence.json" > "$EVIDENCE_DIR/evidence-verifier.json"
    write_manifest
  fi
  exit "$code"
}
trap on_exit EXIT

echo "[golden] profile=$PROFILE db=$DB evidence=$EVIDENCE_DIR"
if [[ -d "$EVIDENCE_DIR" ]]; then
  mv "$EVIDENCE_DIR" "${EVIDENCE_DIR}.superseded.$(date +%s).$$"
fi
mkdir -p "$EVIDENCE_DIR"
CONFIG_TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/imboy-ah-e2e-config.XXXXXX")"
cp "$ROOT/config/sys.config.example" "$CONFIG_TMP_DIR/sys.eunit.config"
EUNIT_CONFIG="$CONFIG_TMP_DIR/sys.eunit"

# 1) scratch 库 + 扩展
CURRENT_STEP="create marker scratch database"
psql -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" -d postgres \
  -c "DROP DATABASE IF EXISTS $DB" -c "CREATE DATABASE $DB" > /dev/null
for E in pg_jieba postgis postgis_raster timescaledb pgcrypto uuid-ossp pg_trgm \
         btree_gin btree_gist unaccent pg_stat_statements; do
  $PSQL -d "$DB" -c "CREATE EXTENSION IF NOT EXISTS $E" > /dev/null 2>&1 || true
done

# 2) 全链迁移 up（1→93+），由生产同口径 strict 迁移器维护 tracking。
CURRENT_STEP="make app"
make -C "$ROOT" app > "$EVIDENCE_DIR/make-app.log" 2>&1
CURRENT_STEP="apply migrations to marker scratch database"
PGHOST="$PGHOST" PGPORT="$PGPORT" PGUSER="$PGUSER" \
  PGDATABASE="$DB" IMBOY_DIR="$ROOT" "$ROOT/scripts/drill_migrate.escript" up \
  > "$EVIDENCE_DIR/migration-up.log" 2>&1
echo "[golden] migrations up OK"

# 3) 核心套件（golden flow 的可自动化子集；测试内含正负例）
export IMBOYENV=local
export IMBOY_PG_HOST="$PGHOST"
export IMBOY_PG_PORT="$PGPORT"
export IMBOY_PG_USERNAME="$PGUSER"
export IMBOY_PG_PASSWORD="$PGPASSWORD"
export IMBOY_PG_DATABASE="$DB"
SUITES=(
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
for S in "${SUITES[@]}"; do
  CURRENT_STEP="make eunit-local t=$S"
  echo "[golden] eunit $S"
  EUNIT_CONFIG="$EUNIT_CONFIG" make -C "$ROOT" eunit-local "t=$S" \
    > "$EVIDENCE_DIR/eunit-$S.log" 2>&1 || {
    echo "[golden] SUITE_FAIL $S"; exit 1; }
  SUITES_PASSED=$((SUITES_PASSED + 1))
done

# 日志扫描在真实 HTTP/重启链完成后统一执行，避免只扫描模块测试。
SENSITIVE_PATTERN='bearer[[:space:]]+[A-Za-z0-9._-]{16,}|api[_-]?key['"'"'"[:space:]:=]+[A-Za-z0-9._-]{12,}|(verify|api|access|refresh)[_-]?token['"'"'"[:space:]:=]+[A-Za-z0-9._-]{12,}|[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}|(^|[^0-9])1[3-9][0-9]{9}([^0-9]|$)'

# 4) 负例断言：E2EE/禁用路径由套件内覆盖（A03/A04 语义），
#    此处追加跨层断言：迁移后的表约束存在性
$PSQL -d "$DB" -tAc "SELECT 1 FROM information_schema.table_constraints
  WHERE constraint_name='bot_delivery_status_check'" | grep -q 1 \
  || { echo "[golden] bot_delivery status check missing"; exit 1; }
$PSQL -d "$DB" -tAc "SELECT 1 FROM information_schema.table_constraints
  WHERE constraint_name='agent_task_status_check'" | grep -q 1 \
  || { echo "[golden] agent_task status check missing"; exit 1; }

# 5) 从真实持久化 fixture 行导出 correlation 链，再由冻结 verifier 复核。
CURRENT_STEP="verify runtime correlation trace"
TRACE_CORR="$($PSQL -d "$DB" -tAc \
  "SELECT correlation_id FROM public.agent_task
   WHERE tool='agent_hub.trace.v1' ORDER BY created_at DESC LIMIT 1")"
if [[ ! "$TRACE_CORR" =~ ^corr-[A-Za-z0-9_-]{16,59}$ ]]; then
  echo "[golden] runtime trace seed correlation missing" >&2
  exit 1
fi
set +e
$PSQL -d "$DB" -At -v correlation_id="$TRACE_CORR" \
  -f "$ROOT/scripts/export_agent_hub_correlation_trace.sql" \
  > "$EVIDENCE_DIR/runtime-correlation-trace.json"
EXPORT_EXIT=$?
if [[ "$EXPORT_EXIT" -eq 0 ]]; then
  python3 "$ROOT/scripts/verify_agent_hub_correlation_trace.py" \
  "$EVIDENCE_DIR/runtime-correlation-trace.json" > "$EVIDENCE_DIR/trace-verifier.json"
  TRACE_EXIT=$?
else
  TRACE_EXIT="$EXPORT_EXIT"
  printf '{"decision":"VIOLATION","errors":["export.command_failed"]}\n' \
    > "$EVIDENCE_DIR/trace-verifier.json"
fi
set -e

# 6) 启动真实后端，运行独立 HTTP MCP 客户端，再做一次完整进程 stop/start。
CURRENT_STEP="compile runtime beams after EUnit"
make -C "$ROOT" app > "$EVIDENCE_DIR/runtime-make-app.log" 2>&1
FIXTURE_EBIN="$CONFIG_TMP_DIR/fixture-ebin"
mkdir -p "$FIXTURE_EBIN"
erlc -o "$FIXTURE_EBIN" \
  "$ROOT/test/fixtures/agent_hub/agent_hub_fake_llm.erl" \
  > "$EVIDENCE_DIR/fake-llm-compile.log" 2>&1
ERL_ROOT="$(erl -noshell -eval 'io:format("~s", [code:root_dir()]), halt().')"
ERL_CALL="$ERL_ROOT/bin/erl_call"
[[ -x "$ERL_CALL" ]] || { echo "[golden] erl_call missing" >&2; exit 1; }

CURRENT_STEP="seed local admin for HTTP smoke"
$PSQL -d "$DB" -c "
  INSERT INTO public.adm_user
    (id, account, nickname, password, role_id, status)
  VALUES
    (700001, 'agent-hub-local-admin', 'Agent Hub local admin',
     'not-used-for-login', ARRAY[1]::bigint[], 1)
  ON CONFLICT (id) DO NOTHING" >/dev/null
$PSQL -d "$DB" -v human_uid="$AGENT_HUMAN_UID" -v other_uid="$MCP_OTHER_UID" \
  -v group_id="$AGENT_GROUP_ID" <<'SQL' >/dev/null
INSERT INTO public."user"
  (id, nickname, password, account, reg_ip, reg_cosv, account_type)
VALUES
  (:human_uid, 'Agent Hub local human', 'not-used-for-login',
   'agent-hub-local-human-' || :human_uid, '127.0.0.1', 'local-fixture', 0),
  (:other_uid, 'Agent Hub local other owner', 'not-used-for-login',
   'agent-hub-local-other-' || :other_uid, '127.0.0.1', 'local-fixture', 0);
INSERT INTO public."group"
  (id, owner_uid, creator_uid, title, member_count, e2ee_mode, scope)
VALUES
  (:group_id, :human_uid, :human_uid, 'Agent Hub local group', 0, 0, 'personal');
SQL

CURRENT_STEP="start local backend for HTTP smoke"
start_backend "$EVIDENCE_DIR/runtime-backend-before-restart.log"
ADM_SIG="$(issue_admin_cookie)"
[[ -n "$ADM_SIG" ]] || { echo "[golden] admin cookie issue failed" >&2; exit 1; }
MCP_MEMBER_RESULT="$(
  printf 'io:format("~p", [{group_member_ds:add_member(%s, %s), group_member_ds:add_member(%s, %s)}]).\n' \
    "$AGENT_GROUP_ID" "$AGENT_HUMAN_UID" "$AGENT_GROUP_ID" "$MCP_OTHER_UID" \
    | "$ERL_CALL" -address "127.0.0.1:$BACKEND_DIST_PORT" \
        -c "$BACKEND_COOKIE" -e -fetch_stdout -no_result_term -timeout 10
)"
[[ "$MCP_MEMBER_RESULT" == "{ok,ok}" ]]
CURRENT_STEP="run real HTTP MCP credential lifecycle"
MCP_ENFORCE_RESULT="$(
  printf 'io:format("~p", [mcp_governance_logic:enforce()]).\n' \
    | "$ERL_CALL" -address "127.0.0.1:$BACKEND_DIST_PORT" \
        -c "$BACKEND_COOKIE" -e -fetch_stdout -no_result_term -timeout 10
)"
[[ "$MCP_ENFORCE_RESULT" == "true" ]]
EXPIRED_MCP_SECRET="$(
  printf 'case mcp_client_repo:create_client(900001, #{name => <<"expired-local">>, expires_at => <<"2020-01-01T00:00:00Z">>}) of {ok, #{<<"secret">> := Secret}} -> io:format("~s", [Secret]); Error -> io:format("ERROR ~p", [Error]) end.\n' \
    | "$ERL_CALL" -address "127.0.0.1:$BACKEND_DIST_PORT" \
        -c "$BACKEND_COOKIE" -e -fetch_stdout -no_result_term -timeout 10
)"
[[ "$EXPIRED_MCP_SECRET" =~ ^[0-9a-f]{64}$ ]]
IMBOY_BASE_URL="http://127.0.0.1:$RUNTIME_HTTP_PORT" \
  ADM_UID=700001 ADM_SIG="$ADM_SIG" EXT01_OWNER_UID="$AGENT_HUMAN_UID" \
  EXT01_OTHER_OWNER_UID="$MCP_OTHER_UID" EXT01_GROUP_ID="$AGENT_GROUP_ID" \
  EXT01_STRICT_NEGATIVES=yes EXPIRED_MCP_SECRET="$EXPIRED_MCP_SECRET" \
  python3 "$ROOT/scripts/agent_hub_ext01_mcp_client_smoke.py" \
    --out "$EVIDENCE_DIR/ext01-a02-runtime.json" \
    > "$EVIDENCE_DIR/ext01-a02-runtime.log" 2>&1
unset EXPIRED_MCP_SECRET
python3 -c \
  'import json,sys; d=json.load(open(sys.argv[1], encoding="utf-8")); assert d.get("passed") == 25 and d.get("failed") == 0' \
  "$EVIDENCE_DIR/ext01-a02-runtime.json"
HTTP_SMOKE_PASSED=1

CURRENT_STEP="seed real channel incoming webhook fixtures"
$PSQL -d "$DB" -v channel_id="$CHANNEL_FIXTURE_ID" <<'SQL' >/dev/null
INSERT INTO public.channel (id, name, creator_uid, status)
VALUES (:channel_id, 'Agent Hub local incoming fixture', 1, 1);
SQL
read -r CHANNEL_WEBHOOK_TOKEN CHANNEL_WEBHOOK_ID CHANNEL_WEBHOOK_BOT_UID <<< \
  "$(create_channel_webhook active-local-hook)"
read -r CHANNEL_WEBHOOK_DISABLED_TOKEN CHANNEL_WEBHOOK_DISABLED_ID _ <<< \
  "$(create_channel_webhook disabled-local-hook)"
[[ "$CHANNEL_WEBHOOK_TOKEN" =~ ^[0-9a-f]{64}$ ]]
[[ "$CHANNEL_WEBHOOK_DISABLED_TOKEN" =~ ^[0-9a-f]{64}$ ]]
[[ "$CHANNEL_WEBHOOK_ID" =~ ^[0-9]+$ && "$CHANNEL_WEBHOOK_BOT_UID" =~ ^[0-9]+$ ]]
[[ "$CHANNEL_WEBHOOK_DISABLED_ID" =~ ^[0-9]+$ ]]
DISABLE_RESULT="$(
  printf 'io:format("~p", [channel_webhook_ds:disable(%s, %s)]).\n' \
    "$CHANNEL_FIXTURE_ID" "$CHANNEL_WEBHOOK_DISABLED_ID" \
    | "$ERL_CALL" -address "127.0.0.1:$BACKEND_DIST_PORT" \
        -c "$BACKEND_COOKIE" -e -fetch_stdout -no_result_term -timeout 10
)"
[[ "$DISABLE_RESULT" == "ok" ]]

CURRENT_STEP="run real channel incoming webhook HTTP checks"
IMBOY_BASE_URL="http://127.0.0.1:$RUNTIME_HTTP_PORT" \
  CHANNEL_WEBHOOK_TOKEN="$CHANNEL_WEBHOOK_TOKEN" \
  CHANNEL_WEBHOOK_DISABLED_TOKEN="$CHANNEL_WEBHOOK_DISABLED_TOKEN" \
  CHANNEL_WEBHOOK_TEXT="$CHANNEL_WEBHOOK_TEXT" \
  python3 "$ROOT/scripts/agent_hub_channel_webhook_smoke.py" \
    --out "$EVIDENCE_DIR/channel-webhook-a02-runtime.json" \
    > "$EVIDENCE_DIR/channel-webhook-a02-runtime.log" 2>&1
$PSQL -d "$DB" -At \
  -v channel_id="$CHANNEL_FIXTURE_ID" \
  -v message_text="$CHANNEL_WEBHOOK_TEXT" \
  -v active_webhook_id="$CHANNEL_WEBHOOK_ID" \
  -v disabled_webhook_id="$CHANNEL_WEBHOOK_DISABLED_ID" \
  -v active_bot_uid="$CHANNEL_WEBHOOK_BOT_UID" <<'SQL' \
  > "$EVIDENCE_DIR/channel-webhook-a02-db.json"
SELECT jsonb_pretty(jsonb_build_object(
  'message_count', (
    SELECT count(*) FROM public.channel_message
    WHERE channel_id = :channel_id AND content = :'message_text'
  ),
  'message', (
    SELECT jsonb_build_object(
      'author_id', author_id, 'content', content, 'payload', payload
    ) FROM public.channel_message
    WHERE channel_id = :channel_id AND content = :'message_text'
    ORDER BY created_at DESC LIMIT 1
  ),
  'active_webhook', (
    SELECT jsonb_build_object(
      'status', status, 'bot_uid', bot_uid, 'plaintext_token_empty', token = '',
      'last_used', last_used_at IS NOT NULL
    ) FROM public.channel_webhook WHERE id = :active_webhook_id
  ),
  'disabled_webhook', (
    SELECT jsonb_build_object(
      'status', status, 'plaintext_token_empty', token = '',
      'last_used', last_used_at IS NOT NULL
    ) FROM public.channel_webhook WHERE id = :disabled_webhook_id
  ),
  'active_bot_account_type', (
    SELECT account_type FROM public."user" WHERE id = :active_bot_uid
  )
));
SQL
python3 - "$EVIDENCE_DIR/channel-webhook-a02-runtime.json" \
  "$EVIDENCE_DIR/channel-webhook-a02-db.json" "$CHANNEL_WEBHOOK_BOT_UID" <<'PY'
import json
import sys

runtime = json.load(open(sys.argv[1], encoding="utf-8"))
database = json.load(open(sys.argv[2], encoding="utf-8"))
bot_uid = int(sys.argv[3])
assert runtime["passed"] == 4 and runtime["failed"] == 0
assert database["message_count"] == 1
assert database["message"]["author_id"] == bot_uid
assert database["message"]["payload"]["is_bot"] is True
assert database["active_webhook"] == {
    "status": 1,
    "bot_uid": bot_uid,
    "plaintext_token_empty": True,
    "last_used": True,
}
assert database["disabled_webhook"] == {
    "status": 2,
    "plaintext_token_empty": True,
    "last_used": True,
}
assert database["active_bot_account_type"] == 2
PY
CHANNEL_WEBHOOK_PASSED=1

CURRENT_STEP="run built-in Agent group dialog with local fake LLM"
AGENT_UID="$(create_fake_agent)"
[[ "$AGENT_UID" =~ ^[0-9]+$ ]] || {
  echo "[golden] fake Agent creation failed" >&2; exit 1; }
MEMBER_RESULT="$(
  printf 'io:format("~p", [group_member_ds:add_member(%s, %s)]).\n' \
    "$AGENT_GROUP_ID" "$AGENT_UID" \
    | "$ERL_CALL" -address "127.0.0.1:$BACKEND_DIST_PORT" \
        -c "$BACKEND_COOKIE" -e -fetch_stdout -no_result_term -timeout 10
)"
[[ "$MEMBER_RESULT" == "ok" ]]
AGENT_HUMAN_TOKEN="$(issue_user_token "$AGENT_HUMAN_UID")"
[[ -n "$AGENT_HUMAN_TOKEN" ]]
WS_URL="ws://127.0.0.1:$RUNTIME_HTTP_PORT/api/v1/ws" \
  WS_TOKEN="$AGENT_HUMAN_TOKEN" WS_GID="$AGENT_GROUP_ID" \
  WS_FROM_UID="$AGENT_HUMAN_UID" \
  WS_MSG_ID="$AGENT_MSG_ID" WS_TEXT="$AGENT_PROMPT" \
  WS_MENTIONS="[\"$AGENT_UID\"]" WS_WAIT_SEC=5 \
  python3 "$ROOT/scripts/smoke/ws_c2g_send.py" \
    > "$EVIDENCE_DIR/agent-dialog-a02-runtime.log" 2>&1
for _ in $(seq 1 80); do
  AGENT_ARCHIVE_COUNT="$($PSQL -d "$DB" -tAc \
    "SELECT count(*) FROM public.msg_store WHERE group_id=$AGENT_GROUP_ID AND
       ((msg_id='$AGENT_MSG_ID' AND from_id=$AGENT_HUMAN_UID AND
         payload::jsonb #>> '{payload,text}'='$AGENT_PROMPT') OR
        (from_id=$AGENT_UID AND payload::jsonb #>> '{payload,text}'='$AGENT_REPLY'))")"
  [[ "$AGENT_ARCHIVE_COUNT" == "2" ]] && break
  sleep 0.25
done
$PSQL -d "$DB" -At \
  -v group_id="$AGENT_GROUP_ID" -v human_uid="$AGENT_HUMAN_UID" \
  -v agent_uid="$AGENT_UID" -v human_msg_id="$AGENT_MSG_ID" \
  -v prompt="$AGENT_PROMPT" -v reply="$AGENT_REPLY" <<'SQL' \
  > "$EVIDENCE_DIR/agent-dialog-a02-db.json"
SELECT jsonb_pretty(jsonb_build_object(
  'group', (
    SELECT jsonb_build_object('id', id, 'e2ee_mode', e2ee_mode, 'scope', scope)
    FROM public."group" WHERE id = :group_id
  ),
  'members', (
    SELECT jsonb_agg(jsonb_build_object(
      'user_id', gm.user_id, 'account_type', u.account_type
    ) ORDER BY gm.user_id)
    FROM public.group_member gm
    JOIN public."user" u ON u.id = gm.user_id
    WHERE gm.group_id = :group_id AND gm.status = 1
  ),
  'formal_messages', (
    SELECT jsonb_agg(jsonb_build_object(
      'msg_id', msg_id, 'from_id', from_id,
      'text', payload #>> '{payload,text}',
      'mentions', payload #> '{payload,mentions}'
    ) ORDER BY created_at)
    FROM public.msg_c2g
    WHERE to_id = :group_id
  ),
  'archived_messages', (
    SELECT jsonb_agg(jsonb_build_object(
      'msg_id', msg_id, 'from_id', from_id, 'group_id', group_id,
      'text', payload::jsonb #>> '{payload,text}'
    ) ORDER BY conv_seq)
    FROM public.msg_store
    WHERE group_id = :group_id
  ),
  'human_match', (
    SELECT count(*) FROM public.msg_store
    WHERE group_id = :group_id AND msg_id = :'human_msg_id'
      AND from_id = :human_uid AND payload::jsonb #>> '{payload,text}' = :'prompt'
  ),
  'agent_match', (
    SELECT count(*) FROM public.msg_store
    WHERE group_id = :group_id AND from_id = :agent_uid
      AND payload::jsonb #>> '{payload,text}' = :'reply'
  )
));
SQL
python3 - "$EVIDENCE_DIR/agent-dialog-a02-db.json" \
  "$AGENT_HUMAN_UID" "$MCP_OTHER_UID" "$AGENT_UID" "$AGENT_GROUP_ID" \
  "$AGENT_MSG_ID" "$AGENT_PROMPT" "$AGENT_REPLY" <<'PY'
import json
import sys

database = json.load(open(sys.argv[1], encoding="utf-8"))
human_uid, other_uid, agent_uid, group_id = map(int, sys.argv[2:6])
human_msg_id, prompt, reply = sys.argv[6:9]
assert database["group"] == {"id": group_id, "e2ee_mode": 0, "scope": "personal"}
expected_members = sorted([
    {"user_id": human_uid, "account_type": 0},
    {"user_id": other_uid, "account_type": 0},
    {"user_id": agent_uid, "account_type": 1},
], key=lambda row: row["user_id"])
assert database["members"] == expected_members
assert database["human_match"] == 1
assert database["agent_match"] == 1
assert len(database["formal_messages"]) == 2
assert len(database["archived_messages"]) == 2
human = next(row for row in database["formal_messages"] if row["msg_id"] == human_msg_id)
agent = next(row for row in database["formal_messages"] if row["from_id"] == agent_uid)
assert human["from_id"] == human_uid and human["text"] == prompt
assert human["mentions"] == [str(agent_uid)]
assert agent["text"] == reply and agent["mentions"] is None
PY
AGENT_DIALOG_PASSED=1

CURRENT_STEP="run developer Bot group dialog with signed loopback webhook"
BOT_FIXTURE_PORT="$(python3 -c \
  'import socket; s=socket.socket(); s.bind(("127.0.0.1", 0)); print(s.getsockname()[1]); s.close()')"
BOT_WEBHOOK_URL="http://127.0.0.1:$BOT_FIXTURE_PORT/hook"
read -r BOT_UID BOT_API_TOKEN BOT_VERIFY_TOKEN <<< "$(create_bot "$BOT_WEBHOOK_URL")"
[[ "$BOT_UID" =~ ^[0-9]+$ && "$BOT_API_TOKEN" =~ ^[0-9a-f]{48}$ \
  && "$BOT_VERIFY_TOKEN" =~ ^[0-9a-f]{48}$ ]]
BOT_MEMBER_RESULT="$(
  printf 'io:format("~p", [group_member_ds:add_member(%s, %s)]).\n' \
    "$AGENT_GROUP_ID" "$BOT_UID" \
    | "$ERL_CALL" -address "127.0.0.1:$BACKEND_DIST_PORT" \
        -c "$BACKEND_COOKIE" -e -fetch_stdout -no_result_term -timeout 10
)"
[[ "$BOT_MEMBER_RESULT" == "ok" ]]
BOT_FIXTURE_READY="$CONFIG_TMP_DIR/bot-fixture-ready.json"
BOT_API_TOKEN="$BOT_API_TOKEN" BOT_VERIFY_TOKEN="$BOT_VERIFY_TOKEN" \
  IMBOY_BASE_URL="http://127.0.0.1:$RUNTIME_HTTP_PORT" \
  python3 "$ROOT/scripts/agent_hub_bot_webhook_fixture.py" \
    --port "$BOT_FIXTURE_PORT" --ready "$BOT_FIXTURE_READY" \
    --out "$EVIDENCE_DIR/bot-dialog-a02-runtime.json" \
    --expected-group-id "$AGENT_GROUP_ID" \
    --expected-trigger-msg-id "$BOT_MSG_ID" \
    --expected-from-uid "$AGENT_HUMAN_UID" --reply-text "$BOT_REPLY" \
    > "$EVIDENCE_DIR/bot-dialog-a02-runtime.log" 2>&1 &
BOT_FIXTURE_PID=$!
for _ in $(seq 1 40); do
  [[ -f "$BOT_FIXTURE_READY" ]] && break
  kill -0 "$BOT_FIXTURE_PID" 2>/dev/null || break
  sleep 0.25
done
[[ -f "$BOT_FIXTURE_READY" ]] && kill -0 "$BOT_FIXTURE_PID" 2>/dev/null
WS_URL="ws://127.0.0.1:$RUNTIME_HTTP_PORT/api/v1/ws" \
  WS_TOKEN="$AGENT_HUMAN_TOKEN" WS_GID="$AGENT_GROUP_ID" \
  WS_FROM_UID="$AGENT_HUMAN_UID" WS_MSG_ID="$BOT_MSG_ID" \
  WS_TEXT="$BOT_PROMPT" WS_MENTIONS="[\"$BOT_UID\"]" WS_WAIT_SEC=5 \
  python3 "$ROOT/scripts/smoke/ws_c2g_send.py" \
    > "$EVIDENCE_DIR/bot-dialog-a02-ws.log" 2>&1
for _ in $(seq 1 80); do
  kill -0 "$BOT_FIXTURE_PID" 2>/dev/null || break
  sleep 0.25
done
if kill -0 "$BOT_FIXTURE_PID" 2>/dev/null; then
  echo "[golden] Bot webhook fixture timed out" >&2
  exit 1
fi
wait "$BOT_FIXTURE_PID"
BOT_FIXTURE_PID=""
unset BOT_API_TOKEN BOT_VERIFY_TOKEN
python3 -c \
  'import json,sys; d=json.load(open(sys.argv[1], encoding="utf-8")); assert d.get("passed") == 12 and d.get("failed") == 0' \
  "$EVIDENCE_DIR/bot-dialog-a02-runtime.json"

for _ in $(seq 1 80); do
  BOT_ARCHIVE_COUNT="$($PSQL -d "$DB" -tAc \
    "SELECT count(*) FROM public.msg_store WHERE group_id=$AGENT_GROUP_ID AND
       from_id=$BOT_UID AND payload::jsonb #>> '{payload,text}'='$BOT_REPLY'")"
  [[ "$BOT_ARCHIVE_COUNT" == "1" ]] && break
  sleep 0.25
done

CURRENT_STEP="run developer Bot group mention negative checks"
WS_URL="ws://127.0.0.1:$RUNTIME_HTTP_PORT/api/v1/ws" \
  WS_TOKEN="$AGENT_HUMAN_TOKEN" WS_GID="$AGENT_GROUP_ID" \
  WS_FROM_UID="$AGENT_HUMAN_UID" WS_MSG_ID="$BOT_PLAIN_MSG_ID" \
  WS_TEXT="plain message without bot mention" WS_WAIT_SEC=2 \
  python3 "$ROOT/scripts/smoke/ws_c2g_send.py" \
    > "$EVIDENCE_DIR/bot-dialog-a02-plain.log" 2>&1
BOT_DISABLE_RESULT="$(
  printf 'io:format("~p", [bot_repo:set_status(%s, 0)]).\n' "$BOT_UID" \
    | "$ERL_CALL" -address "127.0.0.1:$BACKEND_DIST_PORT" \
        -c "$BACKEND_COOKIE" -e -fetch_stdout -no_result_term -timeout 10
)"
[[ "$BOT_DISABLE_RESULT" == "{ok,1}" ]]
WS_URL="ws://127.0.0.1:$RUNTIME_HTTP_PORT/api/v1/ws" \
  WS_TOKEN="$AGENT_HUMAN_TOKEN" WS_GID="$AGENT_GROUP_ID" \
  WS_FROM_UID="$AGENT_HUMAN_UID" WS_MSG_ID="$BOT_DISABLED_MSG_ID" \
  WS_TEXT="disabled bot mention" WS_MENTIONS="[\"$BOT_UID\"]" WS_WAIT_SEC=2 \
  python3 "$ROOT/scripts/smoke/ws_c2g_send.py" \
    > "$EVIDENCE_DIR/bot-dialog-a02-disabled.log" 2>&1
BOT_ENABLE_RESULT="$(
  printf 'io:format("~p", [bot_repo:set_status(%s, 1)]).\n' "$BOT_UID" \
    | "$ERL_CALL" -address "127.0.0.1:$BACKEND_DIST_PORT" \
        -c "$BACKEND_COOKIE" -e -fetch_stdout -no_result_term -timeout 10
)"
[[ "$BOT_ENABLE_RESULT" == "{ok,1}" ]]
$PSQL -d "$DB" -v group_id="$BOT_NONMEMBER_GROUP_ID" \
  -v human_uid="$AGENT_HUMAN_UID" <<'SQL' >/dev/null
INSERT INTO public."group"
  (id, owner_uid, creator_uid, title, member_count, e2ee_mode, scope)
VALUES
  (:group_id, :human_uid, :human_uid, 'Agent Hub Bot nonmember group', 0, 0, 'personal');
SQL
NONMEMBER_HUMAN_RESULT="$(
  printf 'io:format("~p", [group_member_ds:add_member(%s, %s)]).\n' \
    "$BOT_NONMEMBER_GROUP_ID" "$AGENT_HUMAN_UID" \
    | "$ERL_CALL" -address "127.0.0.1:$BACKEND_DIST_PORT" \
        -c "$BACKEND_COOKIE" -e -fetch_stdout -no_result_term -timeout 10
)"
[[ "$NONMEMBER_HUMAN_RESULT" == "ok" ]]
WS_URL="ws://127.0.0.1:$RUNTIME_HTTP_PORT/api/v1/ws" \
  WS_TOKEN="$AGENT_HUMAN_TOKEN" WS_GID="$BOT_NONMEMBER_GROUP_ID" \
  WS_FROM_UID="$AGENT_HUMAN_UID" WS_MSG_ID="$BOT_NONMEMBER_MSG_ID" \
  WS_TEXT="nonmember bot mention" WS_MENTIONS="[\"$BOT_UID\"]" WS_WAIT_SEC=2 \
  python3 "$ROOT/scripts/smoke/ws_c2g_send.py" \
    > "$EVIDENCE_DIR/bot-dialog-a02-nonmember.log" 2>&1
E2EE_DISPATCH_RESULT="$(
  printf 'io:format("~p", [bot_webhook_logic:dispatch_group_mention(%s, %s, #{<<"id">> => <<"%s">>, <<"e2ee">> => 1}, #{<<"mentions">> => [<<"%s">>]}, [%s])]).\n' \
    "$AGENT_HUMAN_UID" "$AGENT_GROUP_ID" "$BOT_E2EE_MSG_ID" "$BOT_UID" "$BOT_UID" \
    | "$ERL_CALL" -address "127.0.0.1:$BACKEND_DIST_PORT" \
        -c "$BACKEND_COOKIE" -e -fetch_stdout -no_result_term -timeout 10
)"
[[ "$E2EE_DISPATCH_RESULT" == "ok" ]]

$PSQL -d "$DB" -At \
  -v group_id="$AGENT_GROUP_ID" -v human_uid="$AGENT_HUMAN_UID" \
  -v bot_uid="$BOT_UID" -v trigger_msg_id="$BOT_MSG_ID" \
  -v prompt="$BOT_PROMPT" -v reply="$BOT_REPLY" \
  -v plain_msg_id="$BOT_PLAIN_MSG_ID" -v disabled_msg_id="$BOT_DISABLED_MSG_ID" \
  -v nonmember_msg_id="$BOT_NONMEMBER_MSG_ID" -v e2ee_msg_id="$BOT_E2EE_MSG_ID" <<'SQL' \
  > "$EVIDENCE_DIR/bot-dialog-a02-db.json"
SELECT jsonb_pretty(jsonb_build_object(
  'bot', (
    SELECT jsonb_build_object('user_id', b.user_id, 'status', b.status,
      'account_type', u.account_type, 'plaintext_api_token_empty', b.api_token = '',
      'plaintext_verify_token_empty', b.verify_token = '')
    FROM public.bot b JOIN public."user" u ON u.id = b.user_id
    WHERE b.user_id = :bot_uid
  ),
  'delivery', (
    SELECT jsonb_build_object(
      'delivery_id', delivery_id, 'event_type', event_type, 'status', status,
      'attempt_count', attempt_count, 'reply_context_empty', reply_context = '',
      'correlation_matches_payload', correlation_id = payload->>'correlation_id')
    FROM public.bot_delivery
    WHERE idempotency_key = 'bwd-mention:' || :bot_uid || ':' || :'trigger_msg_id'
  ),
  'human_message_count', (
    SELECT count(*) FROM public.msg_store WHERE group_id = :group_id
      AND msg_id = :'trigger_msg_id' AND from_id = :human_uid
      AND payload::jsonb #>> '{payload,text}' = :'prompt'
  ),
  'bot_reply_count', (
    SELECT count(*) FROM public.msg_store WHERE group_id = :group_id
      AND from_id = :bot_uid AND payload::jsonb #>> '{payload,text}' = :'reply'
  ),
  'negative_delivery_count', (
    SELECT count(*) FROM public.bot_delivery
    WHERE idempotency_key IN (
      'bwd-mention:' || :bot_uid || ':' || :'plain_msg_id',
      'bwd-mention:' || :bot_uid || ':' || :'disabled_msg_id',
      'bwd-mention:' || :bot_uid || ':' || :'nonmember_msg_id',
      'bwd-mention:' || :bot_uid || ':' || :'e2ee_msg_id')
  )
));
SQL
python3 - "$EVIDENCE_DIR/bot-dialog-a02-runtime.json" \
  "$EVIDENCE_DIR/bot-dialog-a02-db.json" "$BOT_UID" <<'PY'
import json
import sys

runtime = json.load(open(sys.argv[1], encoding="utf-8"))
database = json.load(open(sys.argv[2], encoding="utf-8"))
bot_uid = int(sys.argv[3])
assert runtime["passed"] == 12 and runtime["failed"] == 0
assert runtime["bot_id"] == str(bot_uid)
assert database["bot"] == {
    "user_id": bot_uid,
    "status": 1,
    "account_type": 3,
    "plaintext_api_token_empty": True,
    "plaintext_verify_token_empty": True,
}
assert database["delivery"]["delivery_id"] == runtime["delivery_id"]
assert database["delivery"]["event_type"] == "message.c2g_mention"
assert database["delivery"]["status"] == "success"
assert database["delivery"]["attempt_count"] == 1
assert database["delivery"]["reply_context_empty"] is True
assert database["delivery"]["correlation_matches_payload"] is True
assert database["human_message_count"] == 1
assert database["bot_reply_count"] == 1
assert database["negative_delivery_count"] == 0
PY
BOT_DIALOG_PASSED=1

CURRENT_STEP="run webhook retry, dead-letter, replay, and E2EE task negatives"
WORKER_SUSPEND_RESULT="$(
  printf 'io:format("~p", [sys:suspend(bot_webhook_delivery_worker)]).\n' \
    | "$ERL_CALL" -address "127.0.0.1:$BACKEND_DIST_PORT" \
        -c "$BACKEND_COOKIE" -e -fetch_stdout -no_result_term -timeout 10
)"
[[ "$WORKER_SUSPEND_RESULT" == "ok" ]]
NEGATIVE_CHAIN_RESULT="$(
  printf 'io:format("~p", [elib_pg:with_tx(fun(Conn) -> ok = agent_hub_audit_repo:record_task_start_tx(Conn, <<"%s">>, <<"%s">>), ok = agent_hub_audit_repo:record_transition_tx(Conn, <<"%s">>, <<"%s">>, <<"%s">>, <<"working">>, <<"submitted">>), ok end)]).\n' \
    "$NEGATIVE_CORR" "$NEGATIVE_TASK_ID" "$NEGATIVE_CORR" \
    "$NEGATIVE_TASK_ID" "$NEGATIVE_EVENT_ID" \
    | "$ERL_CALL" -address "127.0.0.1:$BACKEND_DIST_PORT" \
        -c "$BACKEND_COOKIE" -e -fetch_stdout -no_result_term -timeout 10
)"
[[ "$NEGATIVE_CHAIN_RESULT" == "ok" ]]

WEBHOOK_5XX_PORT="$(python3 -c \
  'import socket; s=socket.socket(); s.bind(("127.0.0.1", 0)); print(s.getsockname()[1]); s.close()')"
WEBHOOK_5XX_URL="http://127.0.0.1:$WEBHOOK_5XX_PORT/hook"
WEBHOOK_5XX_READY="$CONFIG_TMP_DIR/webhook-5xx-ready.json"
python3 "$ROOT/scripts/agent_hub_http_status_fixture.py" \
  --port "$WEBHOOK_5XX_PORT" --status 503 --requests 4 \
  --ready "$WEBHOOK_5XX_READY" \
  --out "$EVIDENCE_DIR/webhook-5xx-a02-runtime.json" \
  > "$EVIDENCE_DIR/webhook-5xx-a02-runtime.log" 2>&1 &
STATUS_FIXTURE_PID=$!
for _ in $(seq 1 40); do
  [[ -f "$WEBHOOK_5XX_READY" ]] && break
  kill -0 "$STATUS_FIXTURE_PID" 2>/dev/null || break
  sleep 0.25
done
[[ -f "$WEBHOOK_5XX_READY" ]] && kill -0 "$STATUS_FIXTURE_PID" 2>/dev/null
[[ "$(insert_runtime_delivery "$WEBHOOK_5XX_DELIVERY_ID" "$NEGATIVE_CORR" "$WEBHOOK_5XX_URL")" == "{ok,inserted}" ]]
[[ "$(execute_runtime_delivery "$WEBHOOK_5XX_DELIVERY_ID" 4)" == "ok" ]]
wait "$STATUS_FIXTURE_PID"
STATUS_FIXTURE_PID=""
$PSQL -d "$DB" -At -v delivery_id="$WEBHOOK_5XX_DELIVERY_ID" <<'SQL' \
  > "$EVIDENCE_DIR/webhook-5xx-a02-db.json"
SELECT jsonb_pretty(jsonb_build_object(
  'delivery_id', d.delivery_id,
  'status', d.status,
  'attempt_count', d.attempt_count,
  'row_count', (SELECT count(*) FROM public.bot_delivery WHERE delivery_id = :'delivery_id'),
  'attempt_rows', (SELECT count(*) FROM public.bot_delivery_attempt WHERE delivery_id = :'delivery_id'),
  'audit_status', (SELECT status FROM public.agent_hub_audit
                   WHERE entity_type = 'delivery' AND entity_id = :'delivery_id')
)) FROM public.bot_delivery d WHERE d.delivery_id = :'delivery_id';
SQL
python3 - "$EVIDENCE_DIR/webhook-5xx-a02-runtime.json" \
  "$EVIDENCE_DIR/webhook-5xx-a02-db.json" "$WEBHOOK_5XX_DELIVERY_ID" <<'PY'
import json
import sys

runtime = json.load(open(sys.argv[1], encoding="utf-8"))
database = json.load(open(sys.argv[2], encoding="utf-8"))
assert runtime == {"status": 503, "expected_requests": 4, "requests": 4, "passed": True}
assert database == {
    "delivery_id": sys.argv[3], "status": "dead", "attempt_count": 4,
    "row_count": 1, "attempt_rows": 4, "audit_status": "failed",
}
PY

WEBHOOK_4XX_PORT="$(python3 -c \
  'import socket; s=socket.socket(); s.bind(("127.0.0.1", 0)); print(s.getsockname()[1]); s.close()')"
WEBHOOK_4XX_URL="http://127.0.0.1:$WEBHOOK_4XX_PORT/hook"
WEBHOOK_4XX_READY="$CONFIG_TMP_DIR/webhook-4xx-ready.json"
python3 "$ROOT/scripts/agent_hub_http_status_fixture.py" \
  --port "$WEBHOOK_4XX_PORT" --status 400 --requests 1 \
  --ready "$WEBHOOK_4XX_READY" \
  --out "$EVIDENCE_DIR/webhook-4xx-a02-runtime.json" \
  > "$EVIDENCE_DIR/webhook-4xx-a02-runtime.log" 2>&1 &
STATUS_FIXTURE_PID=$!
for _ in $(seq 1 40); do
  [[ -f "$WEBHOOK_4XX_READY" ]] && break
  kill -0 "$STATUS_FIXTURE_PID" 2>/dev/null || break
  sleep 0.25
done
[[ -f "$WEBHOOK_4XX_READY" ]] && kill -0 "$STATUS_FIXTURE_PID" 2>/dev/null
[[ "$(insert_runtime_delivery "$WEBHOOK_4XX_DELIVERY_ID" "$NEGATIVE_CORR" "$WEBHOOK_4XX_URL")" == "{ok,inserted}" ]]
[[ "$(execute_runtime_delivery "$WEBHOOK_4XX_DELIVERY_ID" 1)" == "ok" ]]
wait "$STATUS_FIXTURE_PID"
STATUS_FIXTURE_PID=""
$PSQL -d "$DB" -At -v delivery_id="$WEBHOOK_4XX_DELIVERY_ID" <<'SQL' \
  > "$EVIDENCE_DIR/webhook-4xx-a02-db.json"
SELECT jsonb_pretty(jsonb_build_object(
  'delivery_id', d.delivery_id,
  'status', d.status,
  'attempt_count', d.attempt_count,
  'row_count', (SELECT count(*) FROM public.bot_delivery WHERE delivery_id = :'delivery_id'),
  'attempt_rows', (SELECT count(*) FROM public.bot_delivery_attempt WHERE delivery_id = :'delivery_id'),
  'audit_status', (SELECT status FROM public.agent_hub_audit
                   WHERE entity_type = 'delivery' AND entity_id = :'delivery_id')
)) FROM public.bot_delivery d WHERE d.delivery_id = :'delivery_id';
SQL
python3 - "$EVIDENCE_DIR/webhook-4xx-a02-runtime.json" \
  "$EVIDENCE_DIR/webhook-4xx-a02-db.json" "$WEBHOOK_4XX_DELIVERY_ID" <<'PY'
import json
import sys

runtime = json.load(open(sys.argv[1], encoding="utf-8"))
database = json.load(open(sys.argv[2], encoding="utf-8"))
assert runtime == {"status": 400, "expected_requests": 1, "requests": 1, "passed": True}
assert database == {
    "delivery_id": sys.argv[3], "status": "dead", "attempt_count": 1,
    "row_count": 1, "attempt_rows": 1, "audit_status": "failed",
}
PY

IMBOY_BASE_URL="http://127.0.0.1:$RUNTIME_HTTP_PORT" \
  ADM_UID=700001 ADM_SIG="$ADM_SIG" \
  python3 "$ROOT/scripts/agent_hub_delivery_replay_smoke.py" \
    --delivery-id "$WEBHOOK_4XX_DELIVERY_ID" \
    --out "$EVIDENCE_DIR/delivery-replay-a02-runtime.json" \
    > "$EVIDENCE_DIR/delivery-replay-a02-runtime.log" 2>&1
$PSQL -d "$DB" -At -v delivery_id="$WEBHOOK_4XX_DELIVERY_ID" <<'SQL' \
  > "$EVIDENCE_DIR/delivery-replay-a02-db.json"
SELECT jsonb_pretty(jsonb_build_object(
  'delivery_id', d.delivery_id,
  'status', d.status,
  'attempt_count', d.attempt_count,
  'row_count', (SELECT count(*) FROM public.bot_delivery WHERE delivery_id = :'delivery_id'),
  'audit_status', (SELECT status FROM public.agent_hub_audit
                   WHERE entity_type = 'delivery' AND entity_id = :'delivery_id')
)) FROM public.bot_delivery d WHERE d.delivery_id = :'delivery_id';
SQL
python3 - "$EVIDENCE_DIR/delivery-replay-a02-runtime.json" \
  "$EVIDENCE_DIR/delivery-replay-a02-db.json" "$WEBHOOK_4XX_DELIVERY_ID" <<'PY'
import json
import sys

runtime = json.load(open(sys.argv[1], encoding="utf-8"))
database = json.load(open(sys.argv[2], encoding="utf-8"))
assert runtime == {
    "delivery_id": sys.argv[3], "http_status": 200, "code": 0,
    "status": "pending", "passed": True,
}
assert database == {
    "delivery_id": sys.argv[3], "status": "pending", "attempt_count": 1,
    "row_count": 1, "audit_status": "pending",
}
PY

printf 'io:format("~p", [agent_task_observer:emit(#{task_id => <<"%s">>, agent_uid => %s, group_id => %s, status => working, member_uids => [%s], text => <<"ignored encrypted task">>, e2ee => true})]).\n' \
  "$E2EE_TASK_ID" "$BOT_UID" "$AGENT_GROUP_ID" "$AGENT_HUMAN_UID" \
  | "$ERL_CALL" -address "127.0.0.1:$BACKEND_DIST_PORT" \
      -c "$BACKEND_COOKIE" -e -fetch_stdout -no_result_term -timeout 10 \
      > "$EVIDENCE_DIR/agent-task-e2ee-a02-runtime.txt"
grep -Fxq 'ok' "$EVIDENCE_DIR/agent-task-e2ee-a02-runtime.txt"
$PSQL -d "$DB" -At -v task_id="$E2EE_TASK_ID" <<'SQL' \
  > "$EVIDENCE_DIR/agent-task-e2ee-a02-db.json"
SELECT jsonb_pretty(jsonb_build_object(
  'task_count', (SELECT count(*) FROM public.agent_task WHERE id = :'task_id'),
  'audit_count', (SELECT count(*) FROM public.agent_hub_audit WHERE entity_id = :'task_id'),
  'msg_c2g_count', (SELECT count(*) FROM public.msg_c2g
                    WHERE payload #>> '{payload,agent_task,task_id}' = :'task_id'),
  'msg_store_count', (SELECT count(*) FROM public.msg_store
                      WHERE payload::jsonb #>> '{payload,agent_task,task_id}' = :'task_id')
));
SQL
python3 - "$EVIDENCE_DIR/agent-task-e2ee-a02-db.json" <<'PY'
import json
import sys

database = json.load(open(sys.argv[1], encoding="utf-8"))
assert database == {
    "task_count": 0, "audit_count": 0, "msg_c2g_count": 0, "msg_store_count": 0,
}
PY

$PSQL -d "$DB" -v correlation_id="$NEGATIVE_CORR" \
  -v delivery_5xx="$WEBHOOK_5XX_DELIVERY_ID" \
  -v delivery_4xx="$WEBHOOK_4XX_DELIVERY_ID" <<'SQL' >/dev/null
DELETE FROM public.bot_delivery_attempt
WHERE delivery_id IN (:'delivery_5xx', :'delivery_4xx');
DELETE FROM public.bot_delivery
WHERE delivery_id IN (:'delivery_5xx', :'delivery_4xx');
DELETE FROM public.agent_hub_audit WHERE correlation_id = :'correlation_id';
SQL
WORKER_RESUME_RESULT="$(
  printf 'io:format("~p", [sys:resume(bot_webhook_delivery_worker)]).\n' \
    | "$ERL_CALL" -address "127.0.0.1:$BACKEND_DIST_PORT" \
        -c "$BACKEND_COOKIE" -e -fetch_stdout -no_result_term -timeout 10
)"
[[ "$WORKER_RESUME_RESULT" == "ok" ]]
PROTOCOL_NEGATIVES_PASSED=1

TRACE_TASK_ID="$($PSQL -d "$DB" -tAc \
  "SELECT id FROM public.agent_task WHERE correlation_id='$TRACE_CORR'")"
[[ "$TRACE_TASK_ID" =~ ^task-[A-Za-z0-9_-]{11,59}$ ]] || {
  echo "[golden] restart task seed missing" >&2; exit 1; }
snapshot_restart_state > "$EVIDENCE_DIR/restart-before.json"
python3 -c '
import json, sys
d = json.load(open(sys.argv[1], encoding="utf-8"))
assert d["task"]["status"] == "completed"
assert d["decision"]["decision"] == "approved"
assert d["delivery"]["status"] == "success"
assert d["task"]["correlation_id"] == d["decision"]["correlation_id"]
assert d["task"]["correlation_id"] == d["delivery"]["correlation_id"]
' "$EVIDENCE_DIR/restart-before.json"

CURRENT_STEP="stop local backend"
stop_backend
if curl -fsS "http://127.0.0.1:$RUNTIME_HTTP_PORT/healthz" >/dev/null 2>&1; then
  echo "[golden] backend port remained live after stop" >&2
  exit 1
fi
CURRENT_STEP="restart local backend"
start_backend "$EVIDENCE_DIR/runtime-backend-after-restart.log"
snapshot_restart_state > "$EVIDENCE_DIR/restart-after.json"
cmp "$EVIDENCE_DIR/restart-before.json" "$EVIDENCE_DIR/restart-after.json" \
  > "$EVIDENCE_DIR/restart-snapshot-cmp.log"
printf 'io:format("~p", [agent_task_logic:lookup(<<"%s">>)]).\n' "$TRACE_TASK_ID" \
  | "$ERL_CALL" -address "127.0.0.1:$BACKEND_DIST_PORT" \
      -c "$BACKEND_COOKIE" -e -fetch_stdout -no_result_term -timeout 5 \
      > "$EVIDENCE_DIR/restart-logic-read.txt"
grep -Fq 'completed' "$EVIDENCE_DIR/restart-logic-read.txt"
stop_backend
RESTART_PASSED=1

CURRENT_STEP="scan all generated logs for secrets and PII"
if grep -qiE "$SENSITIVE_PATTERN" "$EVIDENCE_DIR"/*.log 2>/dev/null; then
  echo "[golden] SECRET OR PII LEAK in generated logs" >&2
  exit 1
fi
SENSITIVE_SCAN_PASSED=1

# 7) 先清理并核验，再结算证据；仅最终 main Base 可闭合 A07。
CURRENT_STEP="cleanup marker scratch resources"
cleanup
FINAL_STATUS="PARTIAL"
EXPECTED_EVIDENCE_EXIT=1
if [[ "$FINAL_INTEGRATED_BASE" -eq 1 ]]; then
  FINAL_STATUS="PASS"
  EXPECTED_EVIDENCE_EXIT=0
fi
write_evidence "$FINAL_STATUS"
set +e
python3 "$ROOT/scripts/verify_agent_hub_task_evidence.py" \
  --task "$EVIDENCE_DIR/evidence.json" > "$EVIDENCE_DIR/evidence-self-verifier.json"
EVIDENCE_EXIT=$?
set -e
if [[ "$EVIDENCE_EXIT" -ne "$EXPECTED_EVIDENCE_EXIT" ]]; then
  echo "[golden] evidence verifier expected $FINAL_STATUS (exit $EXPECTED_EVIDENCE_EXIT), got $EVIDENCE_EXIT" >&2
  exit 2
fi
write_manifest
RUN_FINISHED=1
echo "[golden] evidence written to $EVIDENCE_DIR"
if [[ "$FINAL_INTEGRATED_BASE" -eq 1 ]]; then
  echo "[golden] PASS: A01-A07 passed on the final integrated main Base"
  exit 0
fi
echo "[golden] PARTIAL: A01-A06 PASS; A07 waits for the final integrated Base rerun"
exit 1
