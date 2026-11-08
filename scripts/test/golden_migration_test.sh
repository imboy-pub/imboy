#!/usr/bin/env bash
# Golden Upgrade 迁移账本回归：提取生产函数，替换 Docker/psql 边界。
# 只使用临时文件；不连接数据库、不启动容器，不代表真实 Golden 升级验收。
# shellcheck disable=SC2034 # globals are consumed by production functions extracted with eval below
set -euo pipefail

cd "$(dirname "$0")/../.."
SCRIPT=scripts/golden_upgrade.sh
TMP_ROOT="$(mktemp -d /tmp/imboy_golden_migration.XXXXXX)"
trap 'rm -rf -- "$TMP_ROOT"' EXIT
BIN_DIR="$TMP_ROOT/bin"
CLONE_DIR="$TMP_ROOT/source"
STATE_FILE="$TMP_ROOT/state.json"
MOUNTS_BEFORE="$TMP_ROOT/mounts.txt"
TEST_RELEASE_ROOT="$TMP_ROOT/release/lib"
TEST_RELEASE_MIGRATIONS="$TEST_RELEASE_ROOT/imboy-fixture/priv/migrations"
TEST_PSQL_REPLY="$TMP_ROOT/psql.json"
TEST_LOG="$TMP_ROOT/output.log"
TEST_PSQL_RC=0; TEST_DOCKER_RC=0; TEST_PASS=0
export TEST_PSQL_REPLY TEST_PSQL_RC
mkdir -p "$BIN_DIR" "$CLONE_DIR/priv/migrations" "$TEST_RELEASE_MIGRATIONS"

# 与 deploy_sequence/restore_guard 测试相同：执行真实函数，不给生产脚本增加测试入口。
for name in up_pg state_set_json ret_fail gu_migration_versions gu_schema_snapshot \
    gu_snapshot_before gu_assert_schema report; do
  body="$(sed -n "/^${name}()/,/^}/p" "$SCRIPT")"
  [ -n "$body" ] || { printf 'FAIL: missing function %s\n' "$name" >&2; exit 1; }
  eval "$body"
done
ok() { printf '%s\n' "$*"; }
env_get_var() { printf 'fixture\n'; }

cat > "$BIN_DIR/psql" <<'MOCK'
#!/usr/bin/env bash
set -euo pipefail
# 真实 wrapper 必须拒绝 SQL 错误并隔离 psqlrc；同一条只读语句读取两张表。
[ "$#" = 6 ] && [ "$1" = -X ] && [ "$2" = -v ] && [ "$3" = ON_ERROR_STOP=1 ] \
  && [ "$4" = -At ] && [ "$5" = -c ] || exit 90
case "$6" in
  *'FROM public.schema_migrations)'*'json_agg(version ORDER BY version)'*'FROM public.schema_migrations_history)'*) ;;
  *) exit 91 ;;
esac
cat "$TEST_PSQL_REPLY"
exit "$TEST_PSQL_RC"
MOCK
chmod +x "$BIN_DIR/psql"

docker() {
  if [ "$1" = exec ]; then
    [ "$#" = 5 ] && [ "$2" = imboy_backend ] && [ "$3" = sh ] && [ "$4" = -c ] || return 90
    local script="$5" rc=0
    # 真实容器内文件枚举脚本在本地 fixture 执行，仅替换 release 路径。
    script="${script//\/opt\/imboy\/lib/$TEST_RELEASE_ROOT}"
    sh -c "$script" || rc=$?
    [ "$TEST_DOCKER_RC" = 0 ] || return "$TEST_DOCKER_RC"
    return "$rc"
  fi
  [ "$1" = inspect ] && [ "$2" = -f ] || return 91
  if [ "$3" = '{{.Config.Image}}' ]; then
    printf 'ghcr.io/fixture/backend@sha256:baseline\n'
  else
    printf '/fixture/postgres -> /var/lib/postgresql\n/fixture/garage -> /var/lib/garage/data\n'
  fi
}

fail() { printf 'FAIL: %s\n' "$*" >&2; cat "$TEST_LOG" >&2; exit 1; }
pass() { TEST_PASS=$((TEST_PASS + 1)); printf 'PASS: %s\n' "$*"; }
set_versions() { # <directory> <numeric versions...>
  local dir="$1" version filename; shift
  rm -f "$dir"/*.up.sql
  for version in "$@"; do
    printf -v filename '%08d_fixture.up.sql' "$version"
    : > "$dir/$filename"
  done
}
set_ledger() { # <history JSON> <head version> [dirty JSON]
  jq -nc --argjson history "$1" --argjson version "$2" --argjson dirty "${3:-false}" \
    '{head:[{version:$version,dirty:$dirty}],history:$history}' > "$TEST_PSQL_REPLY"
}
prepare_target() {
  set_versions "$CLONE_DIR/priv/migrations" 1 3 10
  set_versions "$TEST_RELEASE_MIGRATIONS" 1 3 10
  set_ledger '[1,3,10]' 10
  printf '{}\n' > "$STATE_FILE"
  TEST_PSQL_RC=0; TEST_DOCKER_RC=0; STAGE_HINT=''
  RET_FAIL_COUNT=0; RET_FAILURES=''; MIGRATION_RESULT=NOT_RUN; MIG_APPLIED_DELTA=unknown
  SCHEMA_BEFORE_ROWS=1; SCHEMA_BEFORE_VER=3; SCHEMA_AFTER_ROWS=0; SCHEMA_AFTER_VER=0; MIG_FILES=0
  HISTORY_BEFORE_ROWS=2; HISTORY_AFTER_ROWS=0; HISTORY_BEFORE_VERSIONS='[1,3]'
  # 即使容器日志显示迁移成功，账本错误仍须让 MIGRATION=FAIL。
  MIG_NOTE='PASS: all migrations applied'
  : > "$TEST_LOG"
}
reject_target() {
  if gu_assert_schema > "$TEST_LOG" 2>&1; then fail "$1 (accepted)"; fi
  [ "$MIGRATION_RESULT" = FAIL ] && [ "$RET_FAIL_COUNT" = 1 ] \
    || fail "$1 (failure not recorded)"
  pass "$1"
}

prepare_target
set_versions "$CLONE_DIR/priv/migrations" 1 3
set_versions "$TEST_RELEASE_MIGRATIONS" 1 3
set_ledger '[1,3]' 3
gu_snapshot_before > "$TEST_LOG" 2>&1 || fail 'valid baseline snapshot'
jq -e '.snapshot_before | .schema_rows == 1 and .schema_max_version == 3 and
  .schema_dirty == false and .history_count == 2 and .history_versions == [1,3] and
  .backend_image == "ghcr.io/fixture/backend@sha256:baseline"' "$STATE_FILE" > /dev/null \
  || fail 'baseline state does not retain exact migration versions'
[ "$HISTORY_BEFORE_VERSIONS" = '[1,3]' ] && [ "$HISTORY_BEFORE_ROWS" = 2 ] \
  || fail 'baseline globals missing'
pass 'baseline snapshot retains single head and exact history'

# 保留刚建立的真实基线快照，模拟候选 release/source 切换后再断言增量。
set_versions "$CLONE_DIR/priv/migrations" 1 3 10
set_versions "$TEST_RELEASE_MIGRATIONS" 1 3 10
set_ledger '[1,3,10]' 10
gu_assert_schema > "$TEST_LOG" 2>&1 || fail 'valid incremental upgrade'
[ "$SCHEMA_AFTER_ROWS" = 1 ] && [ "$SCHEMA_AFTER_VER" = 10 ] && [ "$HISTORY_AFTER_ROWS" = 3 ] \
  && [ "$MIG_FILES" = 3 ] && [ "$MIG_APPLIED_DELTA" = 1 ] && [ "$MIGRATION_RESULT" = PASS ] \
  || fail 'incremental result fields'
pass 'one head with three history versions passes; set difference is one'

prepare_target
HISTORY_BEFORE_ROWS=3; HISTORY_BEFORE_VERSIONS='[1,3,10]'; SCHEMA_BEFORE_VER=10
gu_assert_schema > "$TEST_LOG" 2>&1 || fail 'no-new-migration upgrade'
[ "$MIG_APPLIED_DELTA" = 0 ] || fail 'no-new-migration delta'
pass 'unchanged valid history passes with zero new migrations'

for history in '[1,2,10]' '[1,3]' '[1,3,9,10]' '[1,3,3,10]' '[]' 'null'; do
  prepare_target
  set_ledger "$history" 10
  reject_target "history must match exact versions: $history"
done
for head in '{"version":10,"dirty":true}' '{"version":10,"dirty":null}' \
    '{"version":3,"dirty":false}' '{"version":"10","dirty":false}'; do
  prepare_target
  jq -nc --argjson head "$head" '{head:[$head],history:[1,3,10]}' > "$TEST_PSQL_REPLY"
  reject_target "head version and dirty must be valid: $head"
done
for heads in '[]' '[{"version":10,"dirty":false},{"version":10,"dirty":false}]'; do
  prepare_target
  jq -nc --argjson head "$heads" '{head:$head,history:[1,3,10]}' > "$TEST_PSQL_REPLY"
  reject_target 'head must contain exactly one row'
done

prepare_target
TEST_PSQL_RC=3
reject_target 'SQL failure propagates even with valid JSON output'
for reply in '' 'not JSON' '{} {}' 'null'; do
  prepare_target
  printf '%s\n' "$reply" > "$TEST_PSQL_REPLY"
  reject_target "empty, malformed or multiple database result rejected: $reply"
done
prepare_target
TEST_DOCKER_RC=1
reject_target 'docker failure propagates even with valid release filenames'

for dir in "$TEST_RELEASE_MIGRATIONS" "$CLONE_DIR/priv/migrations"; do
  prepare_target
  set_versions "$dir"
  reject_target "empty migration directory rejected: ${dir#"$TMP_ROOT"/}"
  prepare_target
  : > "$dir/not_a_version.up.sql"
  reject_target "invalid migration filename rejected: ${dir#"$TMP_ROOT"/}"
  prepare_target
  : > "$dir/00000010_duplicate.up.sql"
  reject_target "duplicate migration version rejected: ${dir#"$TMP_ROOT"/}"
done
prepare_target
set_versions "$CLONE_DIR/priv/migrations" 1 2 10
reject_target 'same count but different source and running release is rejected'
prepare_target
HISTORY_BEFORE_VERSIONS='[1,2]'
reject_target 'internally valid target cannot discard an applied baseline version'
prepare_target
HISTORY_BEFORE_VERSIONS='[]'; HISTORY_BEFORE_ROWS=0
reject_target 'missing baseline cannot be reported as real upgrade success'

prepare_target
set_ledger '[1,2,10]' 10
if gu_snapshot_before > "$TEST_LOG" 2>&1; then fail 'invalid baseline accepted'; fi
jq -e 'has("snapshot_before") | not' "$STATE_FILE" > /dev/null \
  || fail 'invalid baseline snapshot written'
[ -n "$STAGE_HINT" ] || fail 'invalid baseline missing diagnostic'
pass 'invalid baseline stops before saving an upgrade snapshot'

# 报告消费实际断言结果；容器日志的 PASS 字样不能覆盖账本判定。
SEC_CLONE=0; SEC_ENVINIT=0; SEC_PULL=0; SEC_INSTALLVN=0; SEC_SANITY=0
SEC_DATAGEN=0; SEC_UPGRADE=0; SEC_SMOKE8=0; SEC_VERIFY=0
MSG_COUNT=2; TO_REF=v0.0.2; FROM_REF=v0.0.1; TO_IMAGE=fixture-target; FROM_IMAGE=fixture-base
EXPECTED_TO_DIGEST=fixture; PROFILE=fixture
prepare_target
TEST_PSQL_RC=3
if gu_assert_schema > "$TEST_LOG" 2>&1; then fail 'report failure fixture'; fi
report FAIL 1 FAIL NOT_RUN > "$TEST_LOG"
grep -q '^GOLDEN_UPGRADE=FAIL .* MIGRATION=FAIL ' "$TEST_LOG" \
  || fail 'report must use assertion failure, not log PASS'
pass 'report records MIGRATION=FAIL when logs claim PASS but SQL fails'
prepare_target
gu_assert_schema > "$TEST_LOG" 2>&1 || fail 'report success fixture'
report PASS 1 PASS PASS > "$TEST_LOG"
grep -q '^GOLDEN_UPGRADE=PASS .* MIGRATION=PASS ' "$TEST_LOG" \
  || fail 'report must use successful ledger assertion'
pass 'report records MIGRATION=PASS only after ledger assertion succeeds'

printf '\nGolden migration fixture tests: %s passed (no container/database acceptance).\n' "$TEST_PASS"
