#!/usr/bin/env bash
# =============================================================================
# 客服域迁移门（CP-ASSET-01）—— 校验"目标 PG 库已应用全部客服域迁移"。
#
# 为什么需要它：客服域（customer_service）功能跨 backend / widget / admin 三面，
# 部署顺序合同要求"后端起量前客服迁移必须全部落库"。现有 check_migrations.sh
# 只做迁移目录的命名/配对静态检查（不连库）；本门补上"连真库对比版本"这一环，
# 部署脚本 / CI / 人工排查共用同一实现。
#
# 对比口径（与 erlang_migrate 落库语义一一对应，见
# deps/erlang_migrate/src/erlang_migrate_pg.erl + erlang_migrate.erl strict 模式）：
#   * schema_migrations        —— golang-migrate 单行语义（version, dirty, applied_at）；
#   * schema_migrations_history —— strict 模式逐版本流水（每版本一行）。
#
# 判定项（任一不过即非零退出，全部通过 exit 0）：
#   S  静态门：复用 scripts/check_migrations.sh（MIG_DIR 环境变量指向同一目录）
#      做迁移目录静态检查——版本号唯一、up/down 成对、命名/非空/自报版本自洽
#      （不复制其解析逻辑；静态违规 = oracle 不合格，exit 4）；
#   O  oracle 非空：--migrations-dir 存在、含 *.up.sql、且能识别出
#      customer_service 域迁移（空/无 CS 迁移 = 空 oracle，exit 4）；
#   E  库非空：schema_migrations 与 schema_migrations_history 两表必须存在
#      （全新空库/未跑过迁移 → exit 2）；
#   C  当前版本合格：schema_migrations 恰一行、dirty=false、
#      version ∈ 目录全集、version >= max(CS 版本)；
#   H  历史完整：CS 版本集合 ⊆ schema_migrations_history（缺任一中间版本
#      = 脏库形态之一）；
#   F  无 foreign sentinel：history 与 schema_migrations 中不出现
#      目录全集之外的版本（如 00000000）。
#
# 退出码：0=通过；2=库状态判定失败；4=oracle 不合格（静态门违规或迁移目录
#         无 *.up.sql / 无 CS 迁移）；5=连库失败。测试脚本
#         scripts/test/customer_service_migration_gate_test.sh
#         断言静态四负例 + 四类库负例均非零、正例为 0。
#
# 用法（PG 连接参数走 libpq 惯例，与 run_pg_behavior_harnesses.sh 一致）：
#   PGHOST=127.0.0.1 PGPORT=4323 PGUSER=imboy_user PGPASSWORD=*** \
#     PGDATABASE=imboy_test_v1 scripts/customer_service_migration_gate.sh
# 可选：--migrations-dir DIR（默认 priv/migrations）。
#
# 注意：本脚本兼容 macOS 自带 bash 3.2（不使用关联数组 / ${var,,} 等）。
# =============================================================================
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

# ---- 参数与默认值 ----------------------------------------------------------
MIG_DIR="priv/migrations"

while [ $# -gt 0 ]; do
  case "$1" in
    --migrations-dir)
      [ $# -ge 2 ] || { echo "error: --migrations-dir 需要一个参数" >&2; exit 4; }
      MIG_DIR="$2"
      shift 2
      ;;
    --migrations-dir=*)
      MIG_DIR="${1#*=}"
      shift
      ;;
    -h|--help)
      sed -n '3,40p' "$0" | sed 's/^# \{0,1\}//'
      exit 0
      ;;
    *)
      echo "error: 未知参数 $1（支持 --migrations-dir / --help）" >&2
      exit 4
      ;;
  esac
done

fail() { echo "  ❌ $*" >&2; FAILED=1; }
ok()   { echo "  ✅ $*"; }

FAILED=0

# ---- S) 静态门：复用 scripts/check_migrations.sh（不复制解析逻辑）-----------
# 通过 MIG_DIR 环境变量把同一目录传给静态门，覆盖其默认 priv/migrations；
# 负例自测（重复版本号 / 缺 down / 不配对 / 版本乱序）用临时 fixture 目录。
echo "── S) 静态门（scripts/check_migrations.sh @ ${MIG_DIR}）──"
if ! MIG_DIR="$MIG_DIR" bash "$REPO_ROOT/scripts/check_migrations.sh"; then
  echo "❌ 客服迁移门 oracle 不合格：静态检查未通过（重复版本号/缺 up/down/不配对/乱序等，见上方输出）" >&2
  exit 4
fi

# ---- O) oracle：迁移目录与 CS 迁移识别 --------------------------------------
if [ ! -d "$MIG_DIR" ]; then
  echo "❌ 客服迁移门 oracle 不合格：目录不存在 $MIG_DIR" >&2
  exit 4
fi

# 全量版本集合（任意域）与客服域版本集合，均取自 *.up.sql 文件名前缀
# NNNNNNNN_<name>.up.sql；解析思路沿用 scripts/check_migrations.sh 的
# 文件名拆分（版本号 = 第一个下划线前的 8 位数字）。
all_versions="$(find "$MIG_DIR" -maxdepth 1 -type f -name '*.up.sql' 2>/dev/null \
  | awk -F/ '{ n = $NF; sub(/_.*/, "", n); if (n ~ /^[0-9]+$/) print n }' \
  | sort -u)"

if [ -z "$all_versions" ]; then
  echo "❌ 客服迁移门 oracle 不合格：$MIG_DIR 下没有任何 *.up.sql（空 oracle）" >&2
  exit 4
fi

cs_versions="$(find "$MIG_DIR" -maxdepth 1 -type f -name '*customer_service*.up.sql' 2>/dev/null \
  | awk -F/ '{ n = $NF; sub(/_.*/, "", n); if (n ~ /^[0-9]+$/) print n }' \
  | sort -u)"

if [ -z "$cs_versions" ]; then
  echo "❌ 客服迁移门 oracle 不合格：$MIG_DIR 下没有识别到 customer_service 域迁移" >&2
  exit 4
fi

cs_max="$(printf '%s\n' "$cs_versions" | tail -1)"
cs_count="$(printf '%s\n' "$cs_versions" | grep -c .)"
echo "oracle: ${MIG_DIR} 全集 $(printf '%s\n' "$all_versions" | grep -c .) 个版本；" \
     "客服域 ${cs_count} 个（max=${cs_max}）: $(printf '%s ' $cs_versions)"

# ---- 连库自检 ---------------------------------------------------------------
: "${PGHOST:=127.0.0.1}"
: "${PGPORT:=5432}"
: "${PGUSER:=imboy_user}"
: "${PGDATABASE:=imboy_test_v1}"
: "${PGPASSWORD:=}"
export PGHOST PGPORT PGUSER PGPASSWORD

if ! psql -X -tAc "SELECT 1" "$PGDATABASE" >/dev/null 2>&1; then
  echo "❌ 客服迁移门连库失败：${PGUSER}@${PGHOST}:${PGPORT}/${PGDATABASE}" >&2
  exit 5
fi

# 一次性读出两表的判定输入（表不存在时输出空，由下方判定报 E 类失败）
db_head_row="$(psql -X -d "$PGDATABASE" -tAc \
  "SELECT version || '|' || dirty FROM schema_migrations ORDER BY version DESC LIMIT 1" \
  2>/dev/null || true)"
db_head_rows="$(psql -X -d "$PGDATABASE" -tAc \
  "SELECT count(*) FROM schema_migrations" 2>/dev/null || true)"
db_hist_versions="$(psql -X -d "$PGDATABASE" -tAc \
  "SELECT version FROM schema_migrations_history ORDER BY version" 2>/dev/null || true)"

# ---- E) 库非空 --------------------------------------------------------------
if [ -z "$db_head_rows" ]; then
  fail "空库：schema_migrations 表不存在（该库从未应用过迁移）"
elif [ -z "$db_hist_versions" ]; then
  fail "schema_migrations_history 表不存在（strict 模式记账缺失，库状态不可信）"
fi

if [ "$FAILED" -ne 0 ]; then
  echo "❌ 客服迁移门未通过（库不可判定的空/缺表状态）" >&2
  exit 2
fi

# ---- C) schema_migrations 单行合格 -----------------------------------------
head_rows_num=$((10#${db_head_rows}))
if [ "$head_rows_num" -ne 1 ]; then
  fail "schema_migrations 应恰有 1 行（golang-migrate 单行语义），实际 ${db_head_rows} 行"
else
  head_version="${db_head_row%%|*}"
  head_dirty="${db_head_row##*|}"
  # 版本号去前导零后与目录全集比对（统一十进制数值比较）
  head_norm="$((10#${head_version}))"

  if [ "$head_dirty" = "t" ] || [ "$head_dirty" = "true" ]; then
    fail "脏库：schema_migrations.dirty=true（version=${head_version}）—— 需人工修复后才可判定"
  fi

  is_known=0
  for v in $all_versions; do
    if [ "$((10#${v}))" -eq "$head_norm" ]; then is_known=1; break; fi
  done
  if [ "$is_known" -eq 0 ]; then
    fail "foreign sentinel：schema_migrations.version=${head_version} 不在迁移目录版本全集内"
  fi

  if [ "$head_norm" -lt "$((10#${cs_max}))" ]; then
    fail "客服域迁移未应用完：库 head=${head_version} < 客服域 max=${cs_max}"
  else
    ok "库 head=${head_version} >= 客服域 max=${cs_max}，dirty=false"
  fi
fi

# ---- H) 客服域历史完整 ------------------------------------------------------
missing_cs=""
for v in $cs_versions; do
  found=0
  for hv in $db_hist_versions; do
    if [ "$((10#${hv}))" -eq "$((10#${v}))" ]; then found=1; break; fi
  done
  if [ "$found" -eq 0 ]; then missing_cs="${missing_cs}${v} "; fi
done
if [ -n "$missing_cs" ]; then
  fail "脏库（历史缺中间版本）：schema_migrations_history 缺客服域迁移版本 ${missing_cs}"
else
  ok "客服域 ${cs_count} 个版本全部在 schema_migrations_history"
fi

# ---- F) 无 foreign sentinel -------------------------------------------------
foreign_hist=""
for hv in $db_hist_versions; do
  known=0
  for v in $all_versions; do
    if [ "$((10#${hv}))" -eq "$((10#${v}))" ]; then known=1; break; fi
  done
  if [ "$known" -eq 0 ]; then foreign_hist="${foreign_hist}${hv} "; fi
done
if [ -n "$foreign_hist" ]; then
  fail "foreign sentinel：schema_migrations_history 出现目录全集之外的版本 ${foreign_hist}"
else
  ok "schema_migrations_history 无未知版本（foreign sentinel 检查通过）"
fi

# ---- 汇总 -------------------------------------------------------------------
if [ "$FAILED" -ne 0 ]; then
  echo "❌ 客服迁移门未通过：目标库 ${PGDATABASE} 不满足客服域迁移全量应用合同" >&2
  exit 2
fi

echo "✅ 客服迁移门通过：${PGDATABASE} 已应用全部 ${cs_count} 个客服域迁移（max=${cs_max}），无脏标记、无 foreign 版本"
