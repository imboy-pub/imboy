#!/usr/bin/env bash
# W0-SEC-02 — 在可写、隔离的生产规模克隆上验收钱包约束迁移。
#
# 默认仅做只读预检：
#   bash scripts/verify_wallet_constraint_clone.sh --precheck
#
# 显式执行 65/66 up/down 与锁演练：
#   WALLET_ACCEPTANCE_APPLY=YES_ISOLATED_CLONE \
#     bash scripts/verify_wallet_constraint_clone.sh --execute
#
# 连接使用显式 PGHOST/PGPORT/PGDATABASE/PGUSER；认证交给 .pgpass、PGPASSFILE
# 或临时 PGPASSWORD。脚本不会打印连接串或凭据。
set -euo pipefail

cd "$(dirname "$0")/.."

MODE="${1:---precheck}"
case "$MODE" in
  --precheck|--execute) ;;
  *) echo "[FAIL] 用法: $0 [--precheck|--execute]" >&2; exit 1 ;;
esac

fail() {
  echo "[FAIL] $*" >&2
  exit 1
}

ok() {
  echo "[OK] $*"
}

TARGET_HOST="${PGHOST:-}"
TARGET_PORT="${PGPORT:-}"
TARGET_DB="${PGDATABASE:-}"
TARGET_USER="${PGUSER:-}"
MIN_ROWS="${WALLET_ACCEPTANCE_MIN_ROWS:-}"
VALIDATE_TIMEOUT_MS="${WALLET_ACCEPTANCE_VALIDATE_TIMEOUT_MS:-}"
CLONE_MARKER="IMBOY_WALLET_CONSTRAINT_ACCEPTANCE_CLONE"

[[ "$TARGET_HOST" =~ ^[a-zA-Z0-9._:-]+$ ]] \
  || fail "PGHOST 缺失或格式非法"
[[ "$TARGET_PORT" =~ ^[0-9]+$ ]] && [ "$TARGET_PORT" -ge 1 ] && [ "$TARGET_PORT" -le 65535 ] \
  || fail "PGPORT 必须是 1-65535"
[[ "$TARGET_DB" =~ ^[a-zA-Z0-9_]+_wallet_acceptance$ ]] \
  || fail "PGDATABASE 必须以 _wallet_acceptance 结尾"
[[ "$TARGET_USER" =~ ^[a-zA-Z_][a-zA-Z0-9_-]*$ ]] \
  || fail "PGUSER 缺失或格式非法"
[[ "$MIN_ROWS" =~ ^[0-9]+$ ]] && [ "$MIN_ROWS" -gt 0 ] \
  || fail "必须显式设置正整数 WALLET_ACCEPTANCE_MIN_ROWS"

if [ "$MODE" = "--execute" ]; then
  [ "${WALLET_ACCEPTANCE_APPLY:-}" = "YES_ISOLATED_CLONE" ] \
    || fail "执行模式必须设置 WALLET_ACCEPTANCE_APPLY=YES_ISOLATED_CLONE"
  [[ "$VALIDATE_TIMEOUT_MS" =~ ^[0-9]+$ ]] \
    && [ "$VALIDATE_TIMEOUT_MS" -ge 1000 ] \
    && [ "$VALIDATE_TIMEOUT_MS" -le 3600000 ] \
    || fail "执行模式必须设置 1000-3600000 的 WALLET_ACCEPTANCE_VALIDATE_TIMEOUT_MS"
fi

command -v psql >/dev/null 2>&1 || fail "未找到 psql"

# 连接目标始终由上面的显式参数决定；拒绝继承 service/hostaddr/PGOPTIONS 改写目标或会话。
unset PGHOSTADDR PGSERVICE PGSERVICEFILE PGSYSCONFDIR PGOPTIONS PGAPPNAME PSQLRC
export PGAPPNAME=wallet_constraint_clone_acceptance

PSQL=(
  psql -X
  -h "$TARGET_HOST"
  -p "$TARGET_PORT"
  -U "$TARGET_USER"
  -d "$TARGET_DB"
  -v ON_ERROR_STOP=1
  -v VERBOSITY=verbose
  -qtA
)

query() {
  "${PSQL[@]}" -c "$1" | tr -d '\r\n'
}

MIGRATIONS_DIR="priv/migrations"
M65_UP="$MIGRATIONS_DIR/00000065_wallet_available_balance_constraint.up.sql"
M65_DOWN="$MIGRATIONS_DIR/00000065_wallet_available_balance_constraint.down.sql"
M66_UP="$MIGRATIONS_DIR/00000066_validate_wallet_available_balance_constraint.up.sql"
M66_DOWN="$MIGRATIONS_DIR/00000066_validate_wallet_available_balance_constraint.down.sql"
for migration_file in "$M65_UP" "$M65_DOWN" "$M66_UP" "$M66_DOWN"; do
  [ -f "$migration_file" ] || fail "迁移文件不存在: $migration_file"
done

echo "== W0-SEC-02 隔离生产规模克隆验收：${MODE#--} =="

IDENTITY="$(query "
  SELECT current_setting('server_version_num') || '|' ||
         current_database() || '|' ||
         (SELECT system_identifier::text FROM pg_control_system()) || '|' ||
         coalesce((
           SELECT shobj_description(oid, 'pg_database')
           FROM pg_database
           WHERE datname=current_database()
         ), '') || '|' ||
         pg_is_in_recovery()::text || '|' ||
         current_setting('transaction_read_only');
")"
IFS='|' read -r VERSION_NUM ACTUAL_DB SYSTEM_IDENTIFIER MARKER RECOVERY_STATE READ_ONLY_STATE \
  <<<"$IDENTITY"

[[ "$VERSION_NUM" =~ ^[0-9]+$ ]] || fail "无法识别 PostgreSQL 版本"
[ "$VERSION_NUM" -ge 180000 ] && [ "$VERSION_NUM" -lt 190000 ] \
  || fail "必须使用 PostgreSQL 18，当前 server_version_num=$VERSION_NUM"
ok "PostgreSQL 主版本为 18"

[ "$ACTUAL_DB" = "$TARGET_DB" ] || fail "实际数据库与 PGDATABASE 不一致"
[[ "$SYSTEM_IDENTIFIER" =~ ^[0-9]+$ ]] || fail "无法读取 PostgreSQL system_identifier"
[ "$MARKER" = "$CLONE_MARKER" ] \
  || fail "数据库缺少隔离克隆标记；拒绝继续"
ok "数据库注释是精确隔离克隆标记"

[ "$RECOVERY_STATE:$READ_ONLY_STATE" = "false:off" ] \
  || fail "目标必须是脱离复制的可写克隆，不得是生产主库的流复制只读副本"
ok "目标为非恢复态、可写的独立克隆"

# 每个后续连接（包含 DDL、锁 holder、23514 探针和异常清理）都在同一会话内先
# 重验集群 system_identifier、数据库标记、PG18 与 version=64 clean。DNS/LB
# 切换到同名数据库时，写操作不会仅凭早先的预检继续执行。
MUTATION_GUARD_SQL="
DO \$wallet_acceptance_gate\$
DECLARE
  actual_system_identifier text;
  actual_marker text;
BEGIN
  SELECT system_identifier::text
  INTO actual_system_identifier
  FROM pg_control_system();

  SELECT coalesce(shobj_description(oid, 'pg_database'), '')
  INTO actual_marker
  FROM pg_database
  WHERE datname=current_database();

  IF current_database()<>'$TARGET_DB'
     OR current_setting('server_version_num')::integer<180000
     OR current_setting('server_version_num')::integer>=190000
     OR actual_system_identifier<>'$SYSTEM_IDENTIFIER'
     OR actual_marker<>'$CLONE_MARKER'
     OR pg_is_in_recovery()
     OR current_setting('transaction_read_only')<>'off'
     OR coalesce((
       SELECT version::text || ':' || dirty::text
       FROM public.schema_migrations
       ORDER BY version DESC
       LIMIT 1
     ), '')<>'64:false'
  THEN
    RAISE EXCEPTION USING
      ERRCODE='55000',
      MESSAGE='wallet acceptance target identity changed';
  END IF;
END
\$wallet_acceptance_gate\$;
"

guarded_psql() {
  "${PSQL[@]}" -c "$MUTATION_GUARD_SQL" "$@"
}

guarded_query() {
  guarded_psql -c "$1" | tr -d '\r\n'
}

OTHER_SESSIONS="$(guarded_query "
  SELECT count(*)
  FROM pg_stat_activity
  WHERE datname=current_database()
    AND pid<>pg_backend_pid();
")"
[ "$OTHER_SESSIONS" = "0" ] \
  || fail "目标库仍有其他会话（count=${OTHER_SESSIONS}），拒绝执行验收"
ok "目标库无其他会话"

MIGRATION_STATE="$(guarded_query "
  SELECT version::text || ':' || dirty::text
  FROM public.schema_migrations
  ORDER BY version DESC
  LIMIT 1;
")"
[ "$MIGRATION_STATE" = "64:false" ] \
  || fail "克隆必须位于干净迁移版本 64，当前=$MIGRATION_STATE"
ok "schema_migrations=64:false"

CONSTRAINT_COUNT="$(guarded_query "
  SELECT count(*)
  FROM pg_constraint
  WHERE conrelid='public.wallet'::regclass
    AND conname='chk_wallet_frozen_le_balance';
")"
[ "$CONSTRAINT_COUNT" = "0" ] \
  || fail "钱包约束已存在，克隆不是 65 前基线"
ok "65 前约束基线正确"

ROW_COUNT="$(guarded_query "SELECT count(*) FROM public.wallet;")"
[[ "$ROW_COUNT" =~ ^[0-9]+$ ]] || fail "无法读取 wallet 行数"
[ "$ROW_COUNT" -ge "$MIN_ROWS" ] \
  || fail "wallet 行数 $ROW_COUNT 小于验收阈值 $MIN_ROWS"
RELATION_BYTES="$(guarded_query "SELECT pg_total_relation_size('public.wallet'::regclass);")"
ok "wallet 规模达到阈值（rows=${ROW_COUNT}, relation_bytes=${RELATION_BYTES}）"

VIOLATIONS="$(guarded_query "SELECT count(*) FROM public.wallet WHERE frozen>balance;")"
[ "$VIOLATIONS" = "0" ] \
  || fail "发现 frozen>balance 历史违规 $VIOLATIONS 条；必须人工对账，脚本不会修改真钱数据"
ok "历史数据预检 frozen>balance=0"

PROBE_ROWS="$(guarded_query "
  SELECT count(*)
  FROM public.wallet
  WHERE balance<9223372036854775807;
")"
[ "$PROBE_ROWS" -gt 0 ] \
  || fail "没有可安全执行事务内 23514 探针的钱包行"
ok "存在事务内 23514 探针候选行"

DATA_FINGERPRINT_BEFORE="$(guarded_query "
  SELECT count(*)::text || ':' ||
         coalesce(sum(balance::numeric),0)::text || ':' ||
         coalesce(sum(frozen::numeric),0)::text || ':' ||
         coalesce(sum(version::numeric),0)::text || ':' ||
         coalesce(sum(status::numeric),0)::text
  FROM public.wallet;
")"

if [ "$MODE" = "--precheck" ]; then
  echo
  ok "只读预检通过；未执行 DDL、未修改钱包数据"
  echo "下一步：设置 WALLET_ACCEPTANCE_VALIDATE_TIMEOUT_MS 与 WALLET_ACCEPTANCE_APPLY=YES_ISOLATED_CLONE 后执行 --execute"
  exit 0
fi

HOLDER_OS_PID=""
HOLDER_BACKEND_PID=""
HOLDER_APP_NAME=""
OWNED_CONSTRAINT_OID=""
M65_ATTEMPTED=0
HOLDER_SLEEP_SECONDS=$((VALIDATE_TIMEOUT_MS / 1000 + 30))
RUN_TOKEN="wallet_clone_$$_$(date +%s)"

stop_holder() {
  if [ -n "$HOLDER_BACKEND_PID" ] && [ -n "$HOLDER_APP_NAME" ]; then
    guarded_psql -c "
      SELECT pg_terminate_backend(pid)
      FROM pg_stat_activity
      WHERE pid=$HOLDER_BACKEND_PID
        AND datname=current_database()
        AND application_name='$HOLDER_APP_NAME';
    " \
      >/dev/null 2>&1 || true
  fi
  if [ -n "$HOLDER_OS_PID" ] && kill -0 "$HOLDER_OS_PID" >/dev/null 2>&1; then
    kill "$HOLDER_OS_PID" >/dev/null 2>&1 || true
  fi
  if [ -n "$HOLDER_OS_PID" ]; then
    wait "$HOLDER_OS_PID" >/dev/null 2>&1 || true
  fi
  HOLDER_OS_PID=""
  HOLDER_BACKEND_PID=""
  HOLDER_APP_NAME=""
}

constraint_oid() {
  guarded_query "
    SELECT coalesce((
      SELECT oid::text
      FROM pg_constraint
      WHERE conrelid='public.wallet'::regclass
        AND conname='chk_wallet_frozen_le_balance'
    ), '');
  "
}

single_result_line() {
  printf '%s\n' "$1" | awk '/^[0-9]+(:[tf])?$/ { value=$0; count++ } END { if (count==1) print value }'
}

run_create_migration() {
  local description="$1" file="$2" lock_timeout="$3" output result
  echo "-- $description --"
  if ! output="$(guarded_psql \
      -c "BEGIN; SET LOCAL lock_timeout='${lock_timeout}'; SET LOCAL statement_timeout='10s';" \
      -c '\timing on' \
      -f "$file" \
      -c "
        SELECT oid::text || ':' || CASE WHEN convalidated THEN 't' ELSE 'f' END
        FROM pg_constraint
        WHERE conrelid='public.wallet'::regclass
          AND conname='chk_wallet_frozen_le_balance';
      " \
      -c "COMMIT;" 2>&1)"; then
    printf '%s\n' "$output" >&2
    return 1
  fi
  printf '%s\n' "$output"
  result="$(single_result_line "$output")"
  [[ "$result" =~ ^([0-9]+):f$ ]] || return 1
  OWNED_CONSTRAINT_OID="${BASH_REMATCH[1]}"
}

run_owned_migration() {
  local description="$1" file="$2" lock_mode="$3" settings="$4" expected="$5"
  local output result
  echo "-- $description --"
  if ! output="$(guarded_psql \
      -c "
        BEGIN;
        $settings
        LOCK TABLE public.wallet IN $lock_mode MODE;
        DO \$wallet_constraint_owner\$
        DECLARE
          current_oid oid;
        BEGIN
          SELECT oid INTO current_oid
          FROM pg_constraint
          WHERE conrelid='public.wallet'::regclass
            AND conname='chk_wallet_frozen_le_balance';
          IF current_oid IS DISTINCT FROM '$OWNED_CONSTRAINT_OID'::oid THEN
            RAISE EXCEPTION USING
              ERRCODE='55000',
              MESSAGE='wallet acceptance constraint ownership changed';
          END IF;
        END
        \$wallet_constraint_owner\$;
      " \
      -c '\timing on' \
      -f "$file" \
      -c "
        SELECT coalesce((
          SELECT oid::text || ':' || CASE WHEN convalidated THEN 't' ELSE 'f' END
          FROM pg_constraint
          WHERE conrelid='public.wallet'::regclass
            AND conname='chk_wallet_frozen_le_balance'
        ), '0');
      " \
      -c "COMMIT;" 2>&1)"; then
    printf '%s\n' "$output" >&2
    return 1
  fi
  printf '%s\n' "$output"
  result="$(single_result_line "$output")"
  case "$expected" in
    validated)
      [ "$result" = "$OWNED_CONSTRAINT_OID:t" ] || return 1
      ;;
    rebuilt_not_valid)
      [[ "$result" =~ ^([0-9]+):f$ ]] || return 1
      OWNED_CONSTRAINT_OID="${BASH_REMATCH[1]}"
      ;;
    absent)
      [ "$result" = "0" ] || return 1
      OWNED_CONSTRAINT_OID=""
      M65_ATTEMPTED=0
      ;;
    *) return 1 ;;
  esac
}

cleanup() {
  local rc=$?
  local after_count="" cleanup_failed=0
  stop_holder
  if [ "$rc" -ne 0 ] && [ -n "$OWNED_CONSTRAINT_OID" ]; then
    echo "[WARN] 验收失败，尝试删除本次运行创建的约束 oid=$OWNED_CONSTRAINT_OID" >&2
    # 同一事务先取表锁再核对 OID，避免 check 与按名称 DROP 之间被替换同名约束。
    if guarded_psql -c "
      BEGIN;
      SET LOCAL lock_timeout='2s';
      SET LOCAL statement_timeout='10s';
      LOCK TABLE public.wallet IN ACCESS EXCLUSIVE MODE;
      DO \$wallet_constraint_cleanup\$
      DECLARE
        current_oid oid;
      BEGIN
        SELECT oid INTO current_oid
        FROM pg_constraint
        WHERE conrelid='public.wallet'::regclass
          AND conname='chk_wallet_frozen_le_balance';
        IF current_oid IS DISTINCT FROM '$OWNED_CONSTRAINT_OID'::oid THEN
          RAISE EXCEPTION USING
            ERRCODE='55000',
            MESSAGE='wallet acceptance cleanup ownership changed';
        END IF;
        EXECUTE 'ALTER TABLE public.wallet DROP CONSTRAINT chk_wallet_frozen_le_balance';
      END
      \$wallet_constraint_cleanup\$;
      COMMIT;
    " >/dev/null 2>&1; then
      if after_count="$(guarded_query "
        SELECT count(*)
        FROM pg_constraint
        WHERE conrelid='public.wallet'::regclass
          AND conname='chk_wallet_frozen_le_balance';
      " 2>/dev/null)" && [ "$after_count" = "0" ]; then
        echo "[WARN] 已确认恢复到 65 前约束状态" >&2
      else
        cleanup_failed=1
      fi
    else
      echo "[CLEANUP-FAIL] 当前约束 OID 与本次所有权不符，拒绝删除未知对象" >&2
      cleanup_failed=1
    fi
  elif [ "$rc" -ne 0 ] && [ "$M65_ATTEMPTED" -eq 1 ]; then
    echo "[CLEANUP-FAIL] 65 可能已执行但未取得约束 OID；拒绝猜测删除，请人工检查隔离克隆" >&2
    cleanup_failed=1
  fi
  if [ "$cleanup_failed" -ne 0 ]; then
    echo "[CLEANUP-FAIL] 自动恢复未完成；该克隆必须标记 NO-GO 并人工检查" >&2
  fi
  exit "$rc"
}
trap cleanup EXIT

start_holder() {
  local app_name="$1"
  HOLDER_APP_NAME="$app_name"
  guarded_psql -c "
    SET application_name='$app_name';
    BEGIN;
    LOCK TABLE public.wallet IN ROW EXCLUSIVE MODE;
    SELECT pg_sleep($HOLDER_SLEEP_SECONDS);
    COMMIT;
  " >/dev/null 2>&1 &
  HOLDER_OS_PID=$!
}

holder_backend_pid() {
  local app_name="$1"
  guarded_query "
    SELECT count(*)::text || ':' || coalesce(min(a.pid), 0)::text
    FROM pg_locks l
    JOIN pg_stat_activity a ON a.pid=l.pid
    WHERE a.application_name='$app_name'
      AND a.datname=current_database()
      AND l.relation='public.wallet'::regclass
      AND l.mode='RowExclusiveLock'
      AND l.granted;
  "
}

wait_for_holder() {
  local app_name="$1" attempts=0 candidate
  while [ "$attempts" -lt 50 ]; do
    candidate="$(holder_backend_pid "$app_name")"
    if [[ "$candidate" =~ ^1:([0-9]+)$ ]]; then
      HOLDER_BACKEND_PID="${BASH_REMATCH[1]}"
      return 0
    fi
    kill -0 "$HOLDER_OS_PID" >/dev/null 2>&1 || return 1
    attempts=$((attempts + 1))
    sleep 0.1
  done
  return 1
}

holder_is_alive() {
  local app_name="$1"
  [ "$(guarded_query "
    SELECT count(*)
    FROM pg_locks l
    JOIN pg_stat_activity a ON a.pid=l.pid
    WHERE a.application_name='$app_name'
      AND a.datname=current_database()
      AND l.relation='public.wallet'::regclass
      AND l.mode='RowExclusiveLock'
      AND l.granted;
  ")" = "1" ]
}

expect_65_lock_timeout() {
  local output result
  M65_ATTEMPTED=1
  if output="$(guarded_psql \
      -c "BEGIN; SET LOCAL lock_timeout='500ms'; SET LOCAL statement_timeout='10s';" \
      -f "$M65_UP" \
      -c "
        SELECT oid::text || ':' || CASE WHEN convalidated THEN 't' ELSE 'f' END
        FROM pg_constraint
        WHERE conrelid='public.wallet'::regclass
          AND conname='chk_wallet_frozen_le_balance';
      " \
      -c "COMMIT;" 2>&1)"; then
    result="$(single_result_line "$output")"
    if [[ "$result" =~ ^([0-9]+):f$ ]]; then
      OWNED_CONSTRAINT_OID="${BASH_REMATCH[1]}"
    fi
    fail "65 ADD 在并发写锁下应返回 SQLSTATE 55P03，实际成功"
  fi
  if printf '%s' "$output" | grep -q "ERROR:  55P03:"; then
    if [ -z "$(constraint_oid)" ]; then
      M65_ATTEMPTED=0
      ok "65 ADD 在并发写锁下按 500ms fail-fast（SQLSTATE 55P03）"
    else
      fail "65 ADD 返回 55P03 后仍出现同名约束；拒绝推断对象所有权"
    fi
  else
    fail "65 ADD 锁演练返回非预期错误"
  fi
}

expect_sqlstate() {
  local description="$1" expected="$2" sql="$3" output
  if output="$(guarded_psql -c "$sql" 2>&1)"; then
    fail "${description}：期望 SQLSTATE ${expected}，实际成功"
  fi
  printf '%s' "$output" | grep -q "ERROR:  ${expected}:" \
    || fail "${description}：返回非预期错误"
  ok "${description}（SQLSTATE ${expected}）"
}

echo
echo "== 锁与正反迁移演练 =="
OTHER_SESSIONS="$(guarded_query "
  SELECT count(*)
  FROM pg_stat_activity
  WHERE datname=current_database()
    AND pid<>pg_backend_pid();
")"
[ "$OTHER_SESSIONS" = "0" ] \
  || fail "执行前出现其他会话（count=${OTHER_SESSIONS}），拒绝开始 DDL"
ok "执行前再次确认无其他会话"

ADD_HOLDER_APP="${RUN_TOKEN}_add"
VALIDATE_HOLDER_APP="${RUN_TOKEN}_validate"

start_holder "$ADD_HOLDER_APP"
wait_for_holder "$ADD_HOLDER_APP" || fail "未观察到 65 锁演练持锁会话"
expect_65_lock_timeout
holder_is_alive "$ADD_HOLDER_APP" || fail "65 锁超时后持锁会话异常消失"
stop_holder

M65_ATTEMPTED=1
run_create_migration "65 up: ADD CHECK NOT VALID" "$M65_UP" "2s" \
  || fail "65 up 或同事务 OID 采集失败"
ok "65 up 后 convalidated=false"

start_holder "$VALIDATE_HOLDER_APP"
wait_for_holder "$VALIDATE_HOLDER_APP" || fail "未观察到 66 锁演练持锁会话"
run_owned_migration \
  "66 up: VALIDATE（statement_timeout=${VALIDATE_TIMEOUT_MS}ms）" \
  "$M66_UP" \
  "SHARE UPDATE EXCLUSIVE" \
  "SET LOCAL lock_timeout='500ms'; SET LOCAL statement_timeout='${VALIDATE_TIMEOUT_MS}ms';" \
  "validated" \
  || fail "66 up 失败或约束所有权发生变化"
holder_is_alive "$VALIDATE_HOLDER_APP" \
  || fail "66 成功时 ROW EXCLUSIVE 持锁会话已消失，不能证明锁兼容"
stop_holder
ok "66 up 后 convalidated=true"

expect_sqlstate "已验证约束拒绝违规 UPDATE 且连接退出自动回滚" 23514 "
  BEGIN;
  UPDATE public.wallet
  SET frozen=balance+1
  WHERE id=(
    SELECT id FROM public.wallet
    WHERE balance<9223372036854775807
    ORDER BY id
    LIMIT 1
  );
  ROLLBACK;
"

run_owned_migration \
  "66 down: 恢复 NOT VALID" \
  "$M66_DOWN" \
  "ACCESS EXCLUSIVE" \
  "SET LOCAL lock_timeout='2s'; SET LOCAL statement_timeout='10s';" \
  "rebuilt_not_valid" \
  || fail "66 down 失败或约束所有权发生变化"
ok "66 down 后 convalidated=false"

run_owned_migration \
  "65 down: 删除约束" \
  "$M65_DOWN" \
  "ACCESS EXCLUSIVE" \
  "SET LOCAL lock_timeout='2s'; SET LOCAL statement_timeout='10s';" \
  "absent" \
  || fail "65 down 失败或约束所有权发生变化"
[ "$(guarded_query "
  SELECT count(*)
  FROM pg_constraint
  WHERE conrelid='public.wallet'::regclass
    AND conname='chk_wallet_frozen_le_balance';
")" = "0" ] || fail "65 down 后仍有约束残留"
ok "65 down 后约束无残留"

DATA_FINGERPRINT_AFTER="$(guarded_query "
  SELECT count(*)::text || ':' ||
         coalesce(sum(balance::numeric),0)::text || ':' ||
         coalesce(sum(frozen::numeric),0)::text || ':' ||
         coalesce(sum(version::numeric),0)::text || ':' ||
         coalesce(sum(status::numeric),0)::text
  FROM public.wallet;
")"
[ "$DATA_FINGERPRINT_AFTER" = "$DATA_FINGERPRINT_BEFORE" ] \
  || fail "验收前后钱包聚合指纹变化，拒绝通过"
ok "验收前后钱包聚合指纹一致"

[ "$(guarded_query "
  SELECT version::text || ':' || dirty::text
  FROM public.schema_migrations
  ORDER BY version DESC
  LIMIT 1;
")" = "64:false" ] || fail "验收后迁移跟踪状态变化"
ok "schema_migrations 仍为 64:false"

echo
ok "生产规模隔离克隆验收通过；约束与钱包数据均已恢复到 65 前基线"
