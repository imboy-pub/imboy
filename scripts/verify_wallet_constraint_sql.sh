#!/usr/bin/env bash
# W0-SEC-02 — 钱包可用余额约束的真实 PostgreSQL 集成验证。
#
# 在一次性 PostgreSQL 18 实例中验证：
#   - 65 ADD CHECK NOT VALID 允许历史违规留待对账，但立即拒绝新违规写；
#   - 66 VALIDATE 对历史违规 fail-fast，修正 fixture 后可验证成功；
#   - 借记路径共用的 status/可用余额数据库语义边界；
#   - 66 down -> 65 down 的逆序回滚状态；
#   - ADD 的强锁会等待并发写事务，VALIDATE 可与 ROW EXCLUSIVE 并行。
#
# 只使用脚本创建的合成数据；临时实例退出时自动停止并删除。
# Repo 的四条真实借记 SQL 由 EUnit 契约测试覆盖，本脚本不替代该层验证。
# 用法：bash scripts/verify_wallet_constraint_sql.sh
set -euo pipefail

cd "$(dirname "$0")/.."
MIGRATIONS_DIR="priv/migrations"

PGBIN="${PGBIN:-}"
if [ -z "$PGBIN" ]; then
  for candidate in /opt/homebrew/opt/postgresql@*/bin/initdb \
                   /usr/lib/postgresql/*/bin/initdb \
                   "$(command -v initdb 2>/dev/null || true)"; do
    candidate_dir="$(dirname "$candidate" 2>/dev/null || true)"
    if [ -n "$candidate_dir" ] \
       && [ -x "$candidate_dir/initdb" ] \
       && [ -x "$candidate_dir/postgres" ] \
       && [ -x "$candidate_dir/psql" ] \
       && [ -x "$candidate_dir/pg_ctl" ] \
       && [ -x "$candidate_dir/createdb" ]; then
      PGBIN="$candidate_dir"
      break
    fi
  done
fi

if [ -z "$PGBIN" ] \
   || [ ! -x "$PGBIN/initdb" ] \
   || [ ! -x "$PGBIN/postgres" ] \
   || [ ! -x "$PGBIN/psql" ] \
   || [ ! -x "$PGBIN/pg_ctl" ] \
   || [ ! -x "$PGBIN/createdb" ]; then
  echo "[FAIL] 未找到完整 PostgreSQL 服务器工具集（initdb/postgres/psql/pg_ctl/createdb）"
  exit 1
fi

WALLET_PG_PORT="${PGPORT:-55439}"
case "$WALLET_PG_PORT" in
  ''|*[!0-9]*|??????*)
    echo "[FAIL] PGPORT 必须是 1024-65535 的十进制端口"
    exit 1
    ;;
esac
if [ "$WALLET_PG_PORT" -lt 1024 ] || [ "$WALLET_PG_PORT" -gt 65535 ]; then
  echo "[FAIL] PGPORT 必须是 1024-65535 的十进制端口"
  exit 1
fi

# 不继承调用者的 libpq 连接目标、认证或会话选项，避免客户端绕开私有 socket。
unset PGHOST PGHOSTADDR PGPORT PGDATABASE PGUSER PGPASSWORD PGPASSFILE
unset PGSERVICE PGSERVICEFILE PGSYSCONFDIR PGOPTIONS PGAPPNAME
unset PGCONNECT_TIMEOUT PGTARGETSESSIONATTRS PGCLIENTENCODING
unset PGCHANNELBINDING PGREQUIREAUTH PGSSLMODE PGREQUIRESSL
unset PGSSLCERT PGSSLKEY PGSSLROOTCERT PGSSLCRL PGSSLCRLDIR PGSSLSNI
unset PGMINPROTOCOLVERSION PGMAXPROTOCOLVERSION PGGSSENCMODE
unset PGKRBSRVNAME PGGSSLIB PSQLRC

# 使用固定、安全字符集的路径；该路径会进入 pg_ctl -o 的 postgres 参数串。
PGDATA="$(mktemp -d "/tmp/wallet_constraint_verify.XXXXXX")"
LOCK_HOLDER_PID=""
LOCK_HOLDER_BACKEND_PID=""
SERVER_STARTED=0

stop_lock_holder() {
  if [ -n "$LOCK_HOLDER_PID" ]; then
    if [ -n "$LOCK_HOLDER_BACKEND_PID" ]; then
      "${PSQL[@]}" -c \
        "SELECT pg_terminate_backend($LOCK_HOLDER_BACKEND_PID);" \
        >/dev/null 2>&1 || true
    fi
    if kill -0 "$LOCK_HOLDER_PID" >/dev/null 2>&1; then
      kill "$LOCK_HOLDER_PID" >/dev/null 2>&1 || true
    fi
    wait "$LOCK_HOLDER_PID" >/dev/null 2>&1 || true
    LOCK_HOLDER_PID=""
    LOCK_HOLDER_BACKEND_PID=""
  fi
}

cleanup() {
  stop_lock_holder
  if [ "$SERVER_STARTED" -eq 1 ]; then
    if ! "$PGBIN/pg_ctl" -D "$PGDATA" -m immediate stop >/dev/null 2>&1; then
      echo "[WARN] 临时 PostgreSQL 停止失败，保留数据目录供人工检查：$PGDATA" >&2
      return 0
    fi
    SERVER_STARTED=0
  fi
  case "$PGDATA" in
    /tmp/wallet_constraint_verify.??????)
      rm -rf -- "$PGDATA"
      ;;
    *)
      echo "[WARN] 拒绝删除非预期临时目录：$PGDATA" >&2
      ;;
  esac
}
trap cleanup EXIT

"$PGBIN/initdb" -D "$PGDATA" -U postgres --auth=trust --no-locale -E UTF8 >/dev/null
SERVER_STARTED=1
"$PGBIN/pg_ctl" -D "$PGDATA" \
  -o "-p $WALLET_PG_PORT -k $PGDATA -c listen_addresses=''" -w start >/dev/null
"$PGBIN/createdb" -h "$PGDATA" -p "$WALLET_PG_PORT" -U postgres wallet_constraint_test

PSQL=(
  "$PGBIN/psql"
  -X
  -h "$PGDATA"
  -p "$WALLET_PG_PORT"
  -U postgres
  -d wallet_constraint_test
  -v ON_ERROR_STOP=1
  -v VERBOSITY=verbose
  -qtA
)

PG_VERSION_NUM="$("${PSQL[@]}" -c "SHOW server_version_num;")"
case "$PG_VERSION_NUM" in
  ''|*[!0-9]*)
    echo "[FAIL] 无法识别 PostgreSQL server_version_num：$PG_VERSION_NUM"
    exit 1
    ;;
esac
if [ "$PG_VERSION_NUM" -lt 180000 ] || [ "$PG_VERSION_NUM" -ge 190000 ]; then
  echo "[FAIL] 必须使用 PostgreSQL 18，当前 server_version_num=$PG_VERSION_NUM"
  exit 1
fi

PASS=0
FAIL=0

ok() {
  PASS=$((PASS + 1))
  echo "[OK] $1"
}

bad() {
  FAIL=$((FAIL + 1))
  echo "[FAIL] $1: ${2:-<无详情>}"
}

check_equal() {
  local description="$1" expected="$2" actual="$3"
  if [ "$actual" = "$expected" ]; then
    ok "$description"
  else
    bad "$description" "expected=$expected actual=$actual"
  fi
}

PG_VERSION="$("${PSQL[@]}" -c "SHOW server_version;")"
echo "== PostgreSQL ${PG_VERSION} 钱包约束验收 =="

SOCKET_STATE="$("${PSQL[@]}" -c "
  SELECT (inet_server_addr() IS NULL)::text || ':' ||
         (current_setting('unix_socket_directories')='${PGDATA}')::text;
")"
if [ "$SOCKET_STATE" = "true:true" ]; then
  ok "连接仅使用脚本私有 Unix socket"
else
  bad "连接必须使用脚本私有 Unix socket" "actual=$SOCKET_STATE"
  exit 1
fi

expect_sqlstate() {
  local description="$1" expected_state="$2" sql="$3" output
  if output="$("${PSQL[@]}" -c "$sql" 2>&1)"; then
    bad "$description" "期望 SQLSTATE $expected_state，实际成功"
  elif printf '%s' "$output" | grep -q "ERROR:  ${expected_state}:"; then
    ok "$description"
  else
    bad "$description" "$output"
  fi
}

expect_file_sqlstate() {
  local description="$1" expected_state="$2" file="$3" output
  if output="$("${PSQL[@]}" -f "$file" 2>&1)"; then
    bad "$description" "期望 SQLSTATE $expected_state，实际成功"
  elif printf '%s' "$output" | grep -q "ERROR:  ${expected_state}:"; then
    ok "$description"
  else
    bad "$description" "$output"
  fi
}

run_migration() {
  "${PSQL[@]}" -f "$1" >/dev/null
}

start_row_exclusive_holder() {
  local application_name="$1"
  "${PSQL[@]}" -c "
    SET application_name='${application_name}';
    BEGIN;
    LOCK TABLE public.wallet IN ROW EXCLUSIVE MODE;
    SELECT pg_sleep(30);
    COMMIT;
  " >/dev/null 2>&1 &
  LOCK_HOLDER_PID=$!
}

row_exclusive_lock_count() {
  local application_name="$1"
  "${PSQL[@]}" -c "
    SELECT count(*)
    FROM pg_locks l
    JOIN pg_stat_activity a ON a.pid=l.pid
    WHERE a.application_name='${application_name}'
      AND l.relation='public.wallet'::regclass
      AND l.mode='RowExclusiveLock'
      AND l.granted;
  "
}

row_exclusive_lock_backend_pid() {
  local application_name="$1"
  "${PSQL[@]}" -c "
    SELECT a.pid
    FROM pg_locks l
    JOIN pg_stat_activity a ON a.pid=l.pid
    WHERE a.application_name='${application_name}'
      AND l.relation='public.wallet'::regclass
      AND l.mode='RowExclusiveLock'
      AND l.granted
    LIMIT 1;
  "
}

wait_for_row_exclusive_lock() {
  local application_name="$1" attempts=0 backend_pid
  while [ "$attempts" -lt 50 ]; do
    backend_pid="$(row_exclusive_lock_backend_pid "$application_name")"
    case "$backend_pid" in
      ''|*[!0-9]*)
        ;;
      *)
        LOCK_HOLDER_BACKEND_PID="$backend_pid"
        return 0
        ;;
    esac
    if [ -n "$backend_pid" ]; then
      return 1
    fi
    if ! kill -0 "$LOCK_HOLDER_PID" >/dev/null 2>&1; then
      return 1
    fi
    attempts=$((attempts + 1))
    sleep 0.1
  done
  return 1
}

M65_UP="$MIGRATIONS_DIR/00000065_wallet_available_balance_constraint.up.sql"
M65_DOWN="$MIGRATIONS_DIR/00000065_wallet_available_balance_constraint.down.sql"
M66_UP="$MIGRATIONS_DIR/00000066_validate_wallet_available_balance_constraint.up.sql"
M66_DOWN="$MIGRATIONS_DIR/00000066_validate_wallet_available_balance_constraint.down.sql"

"${PSQL[@]}" -c "
  CREATE TABLE public.wallet (
    id bigint PRIMARY KEY,
    user_id bigint UNIQUE NOT NULL,
    balance bigint NOT NULL,
    frozen bigint NOT NULL,
    version integer NOT NULL DEFAULT 0,
    status smallint NOT NULL DEFAULT 1,
    updated_at timestamptz
  );
  INSERT INTO public.wallet(id,user_id,balance,frozen)
  VALUES (1,101,1000,300),(2,202,500,700);
" >/dev/null

VIOLATIONS="$("${PSQL[@]}" -c "SELECT count(*) FROM public.wallet WHERE frozen>balance;")"
check_equal "fixture 含 1 条历史违规" "1" "$VIOLATIONS"

run_migration "$M65_UP"
STATE="$("${PSQL[@]}" -c "
  SELECT convalidated
  FROM pg_constraint
  WHERE conrelid='public.wallet'::regclass
    AND conname='chk_wallet_frozen_le_balance';
")"
check_equal "65 up 添加 NOT VALID 约束" "f" "$STATE"

expect_sqlstate "65 立即拒绝新违规写" "23514" \
  "INSERT INTO public.wallet(id,user_id,balance,frozen) VALUES (3,303,100,101);"
expect_file_sqlstate "66 对历史违规 fail-fast" "23514" "$M66_UP"

STATE="$("${PSQL[@]}" -c "
  SELECT convalidated::text || ':' ||
         (SELECT count(*)::text FROM public.wallet WHERE frozen>balance)
  FROM pg_constraint
  WHERE conrelid='public.wallet'::regclass
    AND conname='chk_wallet_frozen_le_balance';
")"
check_equal "66 失败后约束仍未验证且违规仍可对账" "false:1" "$STATE"

"${PSQL[@]}" -c "UPDATE public.wallet SET frozen=balance WHERE id=2;" >/dev/null
run_migration "$M66_UP"
STATE="$("${PSQL[@]}" -c "
  SELECT convalidated
  FROM pg_constraint
  WHERE conrelid='public.wallet'::regclass
    AND conname='chk_wallet_frozen_le_balance';
")"
check_equal "修正 fixture 后 66 验证成功" "t" "$STATE"

BOUNDARY="$("${PSQL[@]}" -c "
  UPDATE public.wallet
  SET balance=balance-700,version=version+1
  WHERE user_id=101 AND status=1 AND balance-frozen>=700
  RETURNING balance::text || ':' || frozen::text;
")"
check_equal "可用余额边界借记成功" "300:300" "$BOUNDARY"
"${PSQL[@]}" -c "UPDATE public.wallet SET balance=1000 WHERE id=1;" >/dev/null

CHANGED="$("${PSQL[@]}" -c "
  WITH changed AS (
    UPDATE public.wallet
    SET balance=balance-701
    WHERE user_id=101 AND status=1 AND balance-frozen>=701
    RETURNING 1
  )
  SELECT count(*) FROM changed;
")"
check_equal "超过可用余额时借记 0 行" "0" "$CHANGED"

"${PSQL[@]}" -c "UPDATE public.wallet SET status=0 WHERE id=1;" >/dev/null
CHANGED="$("${PSQL[@]}" -c "
  WITH changed AS (
    UPDATE public.wallet
    SET balance=balance-1
    WHERE user_id=101 AND status=1 AND balance-frozen>=1
    RETURNING 1
  )
  SELECT count(*) FROM changed;
")"
check_equal "停用钱包借记 0 行" "0" "$CHANGED"
"${PSQL[@]}" -c "UPDATE public.wallet SET status=1 WHERE id=1;" >/dev/null

expect_sqlstate "绕过应用守卫的违规 UPDATE 被 CHECK 拒绝" "23514" \
  "UPDATE public.wallet SET balance=299 WHERE id=1;"

run_migration "$M66_DOWN"
STATE="$("${PSQL[@]}" -c "
  SELECT convalidated
  FROM pg_constraint
  WHERE conrelid='public.wallet'::regclass
    AND conname='chk_wallet_frozen_le_balance';
")"
check_equal "66 down 恢复 NOT VALID 状态" "f" "$STATE"
expect_sqlstate "66 down 后仍拒绝新违规写" "23514" \
  "UPDATE public.wallet SET balance=299 WHERE id=1;"

run_migration "$M65_DOWN"
COUNT="$("${PSQL[@]}" -c "
  SELECT count(*)
  FROM pg_constraint
  WHERE conrelid='public.wallet'::regclass
    AND conname='chk_wallet_frozen_le_balance';
")"
check_equal "65 down 删除约束" "0" "$COUNT"

"${PSQL[@]}" -c "
  UPDATE public.wallet SET balance=1000,frozen=300,status=1 WHERE id=1;
  UPDATE public.wallet SET balance=700,frozen=500,status=1 WHERE id=2;
" >/dev/null

start_row_exclusive_holder "wallet_add_lock_holder"
if wait_for_row_exclusive_lock "wallet_add_lock_holder"; then
  if OUTPUT="$("${PSQL[@]}" -c "SET lock_timeout='500ms';" -f "$M65_UP" 2>&1)"; then
    bad "65 ADD 在并发 ROW EXCLUSIVE 下应等待强锁" "实际成功"
  elif printf '%s' "$OUTPUT" | grep -q "ERROR:  55P03:"; then
    HELD_AFTER="$(row_exclusive_lock_count "wallet_add_lock_holder")"
    if [ "$HELD_AFTER" = "1" ]; then
      ok "65 ADD 在并发 ROW EXCLUSIVE 下命中 500ms 锁超时"
    else
      bad "65 ADD 锁演练" "返回 55P03 后持锁事务已异常退出"
    fi
  else
    bad "65 ADD 锁等待返回非预期错误" "$OUTPUT"
  fi
else
  bad "65 ADD 锁演练" "未观察到 ROW EXCLUSIVE 持锁"
fi
stop_lock_holder

run_migration "$M65_UP"
start_row_exclusive_holder "wallet_validate_lock_holder"
if wait_for_row_exclusive_lock "wallet_validate_lock_holder"; then
  if OUTPUT="$("${PSQL[@]}" -c "SET lock_timeout='500ms';" -f "$M66_UP" 2>&1)"; then
    HELD_AFTER="$(row_exclusive_lock_count "wallet_validate_lock_holder")"
    if [ "$HELD_AFTER" = "1" ]; then
      ok "66 VALIDATE 可与 ROW EXCLUSIVE 并发"
    else
      bad "66 VALIDATE 锁演练" "验证成功时持锁事务已异常退出"
    fi
  else
    bad "66 VALIDATE 不应被普通写锁阻塞" "$OUTPUT"
  fi
else
  bad "66 VALIDATE 锁演练" "未观察到 ROW EXCLUSIVE 持锁"
fi
stop_lock_holder

run_migration "$M66_DOWN"
run_migration "$M65_DOWN"
COUNT="$("${PSQL[@]}" -c "
  SELECT count(*)
  FROM pg_constraint
  WHERE conrelid='public.wallet'::regclass
    AND conname='chk_wallet_frozen_le_balance';
")"
check_equal "最终反向迁移无约束残留" "0" "$COUNT"

echo
echo "总计: PASS=${PASS} FAIL=${FAIL}"
[ "$FAIL" -eq 0 ]
