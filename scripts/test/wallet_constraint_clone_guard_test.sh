#!/usr/bin/env bash
# 只测隔离克隆验收脚本的本地输入守卫；不连接任何数据库。
set -uo pipefail

cd "$(dirname "$0")/../.."

SCRIPT="scripts/verify_wallet_constraint_clone.sh"
TMP_ROOT="$(mktemp -d /tmp/wallet_clone_guard.XXXXXX)"
MOCK_BIN="$TMP_ROOT/bin"
MOCK_CALLS="$TMP_ROOT/psql.calls"
mkdir -p "$MOCK_BIN"

cleanup() {
  rm -rf -- "$TMP_ROOT"
}
trap cleanup EXIT

cat >"$MOCK_BIN/psql" <<'MOCK'
#!/usr/bin/env bash
printf '%s\n' "$*" >>"$MOCK_CALLS"
[ "${MOCK_PSQL_MODE:-reject}" = "respond" ] || exit 99

if [ "${MOCK_REQUIRE_GUARD:-0}" = "1" ] \
   && [[ "$*" == *" -f "* ]] \
   && [[ "$*" != *"wallet acceptance target identity changed"* ]]; then
  echo "mutation file loaded without same-session identity guard" >&2
  exit 96
fi

if [ "${MOCK_IDENTITY_DRIFT:-0}" = "1" ] \
   && [[ "$*" == *"wallet_acceptance_gate"* ]]; then
  echo "ERROR:  55000: wallet acceptance target identity changed" >&2
  exit 1
fi

case "$*" in
  *"LOCK TABLE public.wallet IN ACCESS EXCLUSIVE MODE"*"wallet_constraint_cleanup"*)
    if [[ "$*" == *"current_oid IS DISTINCT FROM"* ]] \
       && [[ "$*" == *"DROP CONSTRAINT chk_wallet_frozen_le_balance"* ]]; then
      printf '%s\n' seen >"$MOCK_ATOMIC_CLEANUP_SEEN"
    else
      exit 96
    fi
    if [ "${MOCK_CLEANUP_OID_MISMATCH:-0}" = "1" ]; then
      echo "ERROR:  55000: wallet acceptance cleanup ownership changed" >&2
      exit 1
    fi
    : >"$MOCK_OID_STATE"
    ;;
  *"LOCK TABLE public.wallet IN ROW EXCLUSIVE MODE"*) sleep 30 ;;
  *"pg_terminate_backend"*) printf '%s\n' t ;;
  *"coalesce(min(a.pid), 0)"*) printf '%s\n' 1:4242 ;;
  *"SELECT count(*)"*"FROM pg_locks"*) printf '%s\n' 1 ;;
  *"00000065_wallet_available_balance_constraint.up.sql"*)
    if [ "${MOCK_REQUIRE_ATOMIC_OWNERSHIP:-0}" = "1" ] \
       && { [[ "$*" != *"BEGIN;"* ]] \
            || [[ "$*" != *"SELECT oid::text || ':' || CASE WHEN"* ]] \
            || [[ "$*" != *"COMMIT;"* ]]; }; then
      exit 96
    fi
    count=0
    [ ! -s "$MOCK_M65_COUNT_STATE" ] || count="$(cat "$MOCK_M65_COUNT_STATE")"
    count=$((count + 1))
    printf '%s\n' "$count" >"$MOCK_M65_COUNT_STATE"
    if [ "$count" -eq 1 ]; then
      echo "ERROR:  55P03: canceling statement due to lock timeout" >&2
      exit 1
    fi
    printf '%s\n' 12345 >"$MOCK_OID_STATE"
    printf '%s\n' f >"$MOCK_VALIDATED_STATE"
    printf '%s\n' 12345:f
    ;;
  *"00000066_validate_wallet_available_balance_constraint.up.sql"*)
    if [ "${MOCK_REQUIRE_ATOMIC_OWNERSHIP:-0}" = "1" ] \
       && { [[ "$*" != *"wallet_constraint_owner"* ]] \
            || [[ "$*" != *"LOCK TABLE public.wallet IN SHARE UPDATE EXCLUSIVE MODE"* ]]; }; then
      exit 96
    fi
    if [ "${MOCK_FAIL_AFTER_65:-0}" = "1" ]; then
      exit 97
    fi
    printf '%s\n' t >"$MOCK_VALIDATED_STATE"
    printf '%s\n' 12345:t
    ;;
  *"00000066_validate_wallet_available_balance_constraint.down.sql"*)
    if [ "${MOCK_REQUIRE_ATOMIC_OWNERSHIP:-0}" = "1" ] \
       && { [[ "$*" != *"wallet_constraint_owner"* ]] \
            || [[ "$*" != *"LOCK TABLE public.wallet IN ACCESS EXCLUSIVE MODE"* ]]; }; then
      exit 96
    fi
    printf '%s\n' 12346 >"$MOCK_OID_STATE"
    printf '%s\n' f >"$MOCK_VALIDATED_STATE"
    printf '%s\n' 12346:f
    ;;
  *"00000065_wallet_available_balance_constraint.down.sql"*)
    if [ "${MOCK_REQUIRE_ATOMIC_OWNERSHIP:-0}" = "1" ] \
       && { [[ "$*" != *"wallet_constraint_owner"* ]] \
            || [[ "$*" != *"LOCK TABLE public.wallet IN ACCESS EXCLUSIVE MODE"* ]]; }; then
      exit 96
    fi
    : >"$MOCK_OID_STATE"
    printf '%s\n' 0
    ;;
  *"frozen=balance+1"*)
    echo "ERROR:  23514: check constraint violation" >&2
    exit 1
    ;;
  *"SELECT coalesce("*"oid::text"*) cat "$MOCK_OID_STATE" ;;
  *"SELECT convalidated"*) cat "$MOCK_VALIDATED_STATE" ;;
  *"pid<>pg_backend_pid()"*) printf '%s\n' "${MOCK_OTHER_SESSIONS:-0}" ;;
  *"count(*)"*"conname='chk_wallet_frozen_le_balance'"*)
    if [ -n "${MOCK_CONSTRAINT_COUNT+x}" ]; then
      printf '%s\n' "$MOCK_CONSTRAINT_COUNT"
    elif [ -s "$MOCK_OID_STATE" ]; then
      printf '%s\n' 1
    else
      printf '%s\n' 0
    fi
    ;;
  *"pg_total_relation_size"*) printf '%s\n' 16777216 ;;
  *"frozen>balance"*) printf '%s\n' "${MOCK_VIOLATIONS:-0}" ;;
  *"balance<9223372036854775807"*) printf '%s\n' "${MOCK_PROBE_ROWS:-100000}" ;;
  *"count(*) FROM public.wallet"*) printf '%s\n' "${MOCK_ROW_COUNT:-100000}" ;;
  *"sum(balance::numeric)"*) printf '%s\n' 100000:1000000:100000:0:100000 ;;
  *"wallet_acceptance_gate"*) printf '%s\n' "${MOCK_MIGRATION_STATE:-64:false}" ;;
  *"SELECT current_setting('server_version_num')"*"pg_control_system()"*)
    printf '%s|%s|%s|%s|%s|%s\n' \
      "${MOCK_VERSION:-180004}" \
      "${MOCK_DATABASE:-imboy_wallet_acceptance}" \
      "${MOCK_SYSTEM_IDENTIFIER:-7520366804851691208}" \
      "${MOCK_MARKER-IMBOY_WALLET_CONSTRAINT_ACCEPTANCE_CLONE}" \
      "${MOCK_RECOVERY:-false}" \
      "${MOCK_READ_ONLY:-off}" ;;
  *) exit 98 ;;
esac
MOCK
chmod +x "$MOCK_BIN/psql"

PASS=0
FAIL=0

ok() {
  PASS=$((PASS + 1))
  echo "  PASS $1"
}

bad() {
  FAIL=$((FAIL + 1))
  echo "  FAIL $1: ${2:-<无详情>}"
}

assert_preconnect_reject() {
  local description="$1" needle="$2"
  shift 2
  : >"$MOCK_CALLS"
  local output rc
  output="$(env PATH="$MOCK_BIN:$PATH" MOCK_CALLS="$MOCK_CALLS" "$@" 2>&1 </dev/null)"
  rc=$?
  if [ "$rc" -eq 0 ]; then
    bad "$description" "预期失败，实际成功"
  elif ! printf '%s' "$output" | grep -q "$needle"; then
    bad "$description" "$output"
  elif [ -s "$MOCK_CALLS" ]; then
    bad "$description" "输入守卫前已调用 psql"
  else
    ok "$description"
  fi
}

assert_dynamic_reject() {
  local description="$1" needle="$2"
  shift 2
  : >"$MOCK_CALLS"
  local output rc
  output="$(env PATH="$MOCK_BIN:$PATH" MOCK_CALLS="$MOCK_CALLS" \
    MOCK_PSQL_MODE=respond "${BASE_ENV[@]}" "$@" \
    bash "$SCRIPT" --precheck 2>&1 </dev/null)"
  rc=$?
  if [ "$rc" -eq 0 ]; then
    bad "$description" "预期失败，实际成功"
  elif ! printf '%s' "$output" | grep -q "$needle"; then
    bad "$description" "$output"
  elif grep -q -- ' -f ' "$MOCK_CALLS"; then
    bad "$description" "动态守卫失败前已加载迁移文件"
  else
    ok "$description"
  fi
}

BASE_ENV=(
  PGHOST=clone.internal
  PGPORT=5432
  PGDATABASE=imboy_wallet_acceptance
  PGUSER=imboy_acceptance
  WALLET_ACCEPTANCE_MIN_ROWS=100000
)

MOCK_OID_STATE="$TMP_ROOT/constraint.oid"
MOCK_VALIDATED_STATE="$TMP_ROOT/constraint.validated"
MOCK_M65_COUNT_STATE="$TMP_ROOT/m65.count"
MOCK_ATOMIC_CLEANUP_SEEN="$TMP_ROOT/atomic-cleanup.seen"

reset_execute_state() {
  : >"$MOCK_OID_STATE"
  : >"$MOCK_M65_COUNT_STATE"
  : >"$MOCK_ATOMIC_CLEANUP_SEEN"
  printf '%s\n' f >"$MOCK_VALIDATED_STATE"
}

echo "== 钱包生产规模克隆 Gate 输入守卫 =="

assert_preconnect_reject "数据库名无专用后缀时拒绝" "必须以 _wallet_acceptance 结尾" \
  env "${BASE_ENV[@]}" PGDATABASE=imboy_pro bash "$SCRIPT" --precheck

assert_preconnect_reject "未声明最小生产规模时拒绝" "WALLET_ACCEPTANCE_MIN_ROWS" \
  env PGHOST=clone.internal PGPORT=5432 PGDATABASE=imboy_wallet_acceptance \
      PGUSER=imboy_acceptance bash "$SCRIPT" --precheck

assert_preconnect_reject "执行模式缺确认令牌时拒绝" "YES_ISOLATED_CLONE" \
  env "${BASE_ENV[@]}" WALLET_ACCEPTANCE_VALIDATE_TIMEOUT_MS=60000 \
      bash "$SCRIPT" --execute

assert_preconnect_reject "执行模式缺验证超时时拒绝" "VALIDATE_TIMEOUT_MS" \
  env "${BASE_ENV[@]}" WALLET_ACCEPTANCE_APPLY=YES_ISOLATED_CLONE \
      bash "$SCRIPT" --execute

assert_preconnect_reject "超大验证超时被拒绝" "1000-3600000" \
  env "${BASE_ENV[@]}" WALLET_ACCEPTANCE_APPLY=YES_ISOLATED_CLONE \
      WALLET_ACCEPTANCE_VALIDATE_TIMEOUT_MS=9999999 \
      bash "$SCRIPT" --execute

assert_dynamic_reject "PostgreSQL 非 18 时拒绝" "必须使用 PostgreSQL 18" \
  MOCK_VERSION=170009

assert_dynamic_reject "实际数据库名变化时拒绝" "实际数据库与 PGDATABASE 不一致" \
  MOCK_DATABASE=other_wallet_acceptance

assert_dynamic_reject "无法读取集群身份时拒绝" "无法读取 PostgreSQL system_identifier" \
  MOCK_SYSTEM_IDENTIFIER=unknown

assert_dynamic_reject "缺少精确数据库标记时拒绝" "缺少隔离克隆标记" \
  MOCK_MARKER=

assert_dynamic_reject "恢复态副本时拒绝" "目标必须是脱离复制的可写克隆" \
  MOCK_RECOVERY=true

assert_dynamic_reject "只读事务目标时拒绝" "目标必须是脱离复制的可写克隆" \
  MOCK_READ_ONLY=on

assert_dynamic_reject "存在其他数据库会话时拒绝" "目标库仍有其他会话" \
  MOCK_OTHER_SESSIONS=1

assert_dynamic_reject "迁移基线不是 64 clean 时拒绝" "克隆必须位于干净迁移版本 64" \
  MOCK_MIGRATION_STATE=63:false

assert_dynamic_reject "目标约束已存在时拒绝" "钱包约束已存在" \
  MOCK_CONSTRAINT_COUNT=1

assert_dynamic_reject "钱包数据低于声明规模时拒绝" "小于验收阈值" \
  MOCK_ROW_COUNT=99999

assert_dynamic_reject "存在历史 frozen 大于 balance 数据时拒绝" "必须人工对账" \
  MOCK_VIOLATIONS=1

assert_dynamic_reject "没有安全探针候选行时拒绝" "没有可安全执行事务内 23514 探针" \
  MOCK_PROBE_ROWS=0

: >"$MOCK_CALLS"
OUT="$(env PATH="$MOCK_BIN:$PATH" MOCK_CALLS="$MOCK_CALLS" \
  MOCK_PSQL_MODE=respond \
  "${BASE_ENV[@]}" bash "$SCRIPT" --precheck 2>&1 </dev/null)"
if printf '%s' "$OUT" | grep -q '只读预检通过' \
   && ! grep -q -- ' -f ' "$MOCK_CALLS"; then
  ok "默认 precheck 全程未加载迁移文件"
else
  bad "默认 precheck 不是纯只读路径" "$OUT"
fi

reset_execute_state
: >"$MOCK_CALLS"
OUT="$(env PATH="$MOCK_BIN:$PATH" MOCK_CALLS="$MOCK_CALLS" \
  MOCK_PSQL_MODE=respond MOCK_REQUIRE_GUARD=1 MOCK_REQUIRE_ATOMIC_OWNERSHIP=1 \
  MOCK_OID_STATE="$MOCK_OID_STATE" MOCK_VALIDATED_STATE="$MOCK_VALIDATED_STATE" \
  MOCK_M65_COUNT_STATE="$MOCK_M65_COUNT_STATE" \
  MOCK_ATOMIC_CLEANUP_SEEN="$MOCK_ATOMIC_CLEANUP_SEEN" \
  "${BASE_ENV[@]}" WALLET_ACCEPTANCE_APPLY=YES_ISOLATED_CLONE \
  WALLET_ACCEPTANCE_VALIDATE_TIMEOUT_MS=1000 \
  bash "$SCRIPT" --execute 2>&1 </dev/null)"
if printf '%s' "$OUT" | grep -q '生产规模隔离克隆验收通过'; then
  ok "每个迁移都在同一会话绑定身份、约束 OID 与变更事务"
else
  bad "execute 写会话身份守卫未真实执行" "$OUT"
fi

reset_execute_state
: >"$MOCK_CALLS"
OUT="$(env PATH="$MOCK_BIN:$PATH" MOCK_CALLS="$MOCK_CALLS" \
  MOCK_PSQL_MODE=respond MOCK_IDENTITY_DRIFT=1 \
  MOCK_OID_STATE="$MOCK_OID_STATE" MOCK_VALIDATED_STATE="$MOCK_VALIDATED_STATE" \
  MOCK_M65_COUNT_STATE="$MOCK_M65_COUNT_STATE" \
  MOCK_ATOMIC_CLEANUP_SEEN="$MOCK_ATOMIC_CLEANUP_SEEN" \
  "${BASE_ENV[@]}" WALLET_ACCEPTANCE_APPLY=YES_ISOLATED_CLONE \
  WALLET_ACCEPTANCE_VALIDATE_TIMEOUT_MS=1000 \
  bash "$SCRIPT" --execute 2>&1 </dev/null)"
if printf '%s' "$OUT" | grep -q 'wallet acceptance target identity changed' \
   && ! grep -q -- ' -f ' "$MOCK_CALLS"; then
  ok "初检后数据库身份漂移会在任何迁移文件加载前停止"
else
  bad "数据库身份漂移未 fail-closed" "$OUT"
fi

reset_execute_state
: >"$MOCK_CALLS"
OUT="$(env PATH="$MOCK_BIN:$PATH" MOCK_CALLS="$MOCK_CALLS" \
  MOCK_PSQL_MODE=respond MOCK_REQUIRE_GUARD=1 MOCK_FAIL_AFTER_65=1 \
  MOCK_CLEANUP_OID_MISMATCH=1 \
  MOCK_OID_STATE="$MOCK_OID_STATE" MOCK_VALIDATED_STATE="$MOCK_VALIDATED_STATE" \
  MOCK_M65_COUNT_STATE="$MOCK_M65_COUNT_STATE" \
  MOCK_ATOMIC_CLEANUP_SEEN="$MOCK_ATOMIC_CLEANUP_SEEN" \
  "${BASE_ENV[@]}" WALLET_ACCEPTANCE_APPLY=YES_ISOLATED_CLONE \
  WALLET_ACCEPTANCE_VALIDATE_TIMEOUT_MS=1000 \
  bash "$SCRIPT" --execute 2>&1 </dev/null)"
if printf '%s' "$OUT" | grep -q 'CLEANUP-FAIL' \
   && [ -s "$MOCK_ATOMIC_CLEANUP_SEEN" ]; then
  ok "异常清理在同一事务锁表、核对 OID 后拒绝删除未知对象"
else
  bad "异常清理所有权边界未真实执行" "$OUT"
fi

echo
echo "总计: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
