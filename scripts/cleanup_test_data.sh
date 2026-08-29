#!/usr/bin/env bash
# ============================================================
# 本地测试数据清理 / Local test-data cleanup (imboy_v1)
# ------------------------------------------------------------
# 背景：每轮 `make eunit-local` 全量/批量跑都会向本地 imboy_v1
# （生产快照副本）累积测试用户/群/成员/消息等。2026-08-29 盘点：
# 69,485 / 69,831（99.5%）的 user 行是 reg_cosv='perf-test' 的
# 测试用户。本脚本按可唯一识别的标记批量清除测试数据。
#
# 用法 / Usage:
#   bash scripts/cleanup_test_data.sh              # dry-run：只统计不删除
#   APPLY=1 bash scripts/cleanup_test_data.sh      # 真正执行（单事务）
#   TARGET_COSV="perf-test,smoke" APPLY=1 bash ... # 自定义标记集合
#   VACUUM=1 APPLY=1 bash ...                      # 删后回收空间
#
# 安全护栏：
#   - 仅允许本机 (127.0.0.1/localhost) + imboy_v1 库，杜绝误跑生产；
#   - 检测到任何 eunit 运行（含 .worktrees/ 并行会话，共享同库）即拒绝；
#   - 默认 dry-run；APPLY=1 走单事务，任一步失败整体回滚。
#
# 删除顺序：按 (表, uid类列) 逐对清引用行 → group_member（含测试群成员）
# → group（测试群）→ user（测试用户）。命中 51 个外键约束时会报出具体
# 约束并整体回滚，将该表列加入 UID_COLS 或前置阶段即可。
# ============================================================
set -euo pipefail

PGHOST="${PGHOST:-127.0.0.1}"
PGPORT="${PGPORT:-4323}"
PGUSER="${PGUSER:-imboy_user}"
PGDATABASE="${PGDATABASE:-imboy_v1}"
export PGPASSWORD="${PGPASSWORD:-abc54321}"
APPLY="${APPLY:-0}"
TARGET_COSV="${TARGET_COSV:-perf-test}"
WORKDIR="${TMPDIR:-/tmp}/cleanup_test_data.$$"

RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[1;33m'; NC='\033[0m'
info() { echo -e "${GREEN}[cleanup_test_data]${NC} $*"; }
warn() { echo -e "${YELLOW}[cleanup_test_data]${NC} $*"; }
fail() { echo -e "${RED}[cleanup_test_data] ERROR:${NC} $*" >&2; exit 1; }
trap 'rm -rf "$WORKDIR"' EXIT
mkdir -p "$WORKDIR"

# ---------- 护栏 ----------
case "$PGHOST" in
  127.0.0.1|localhost|::1) ;;
  *) fail "拒绝运行：PGHOST=$PGHOST 非本机（本脚本只允许清理本地库）" ;;
esac
[ "$PGDATABASE" = "imboy_v1" ] || fail "拒绝运行：PGDATABASE=$PGDATABASE ≠ imboy_v1"

if pgrep -f 'eunit:test' >/dev/null 2>&1; then
  fail "检测到 eunit 正在运行（主仓或 .worktrees/ 并行会话，共享本库）。请等待其结束。"
fi

PSQL=(psql -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" -d "$PGDATABASE" -v ON_ERROR_STOP=1 -X -q)
"${PSQL[@]}" -c "SELECT 1" >/dev/null 2>&1 || fail "无法连接 $PGHOST:$PGPORT/$PGDATABASE"

# uid 类列（精确名匹配；刻意排除 user_id_sum(算术和)/at_uids(数组) 等非 id 列，
# 数组/异常列由运行期容错跳过并告警）
UID_COLS="uid,user_id,from_uid,to_uid,from_user_id,to_user_id,creator_uid,owner_uid,creator_user_id,owner_user_id,inviter_uid,invitee_uid,sender_uid,receiver_uid,target_uid,author_uid,reporter_uid,operator_uid,reply_to_uid,trustee_uid,ref_user_id,denied_user_id,edit_user_id,last_referer_user_id,replier_user_id"

# ---------- 收集目标 uid（真表：跨 psql 会话可用，结束即删）----------
"${PSQL[@]}" <<SQL
DROP TABLE IF EXISTS _cleanup_test_uids;
CREATE TABLE _cleanup_test_uids AS
  SELECT id AS uid FROM "user" WHERE reg_cosv = ANY(string_to_array('$TARGET_COSV', ','));
SQL

N_UIDS=$("${PSQL[@]}" -t -A -c "SELECT count(*) FROM _cleanup_test_uids")
info "目标标记: reg_cosv IN ($TARGET_COSV) → 测试用户 $N_UIDS 个"
if [ "$N_UIDS" -eq 0 ]; then
  "${PSQL[@]}" -c "DROP TABLE _cleanup_test_uids"
  info "无测试数据，退出"
  exit 0
fi

# ---------- 生成 (表, 列) 清单 ----------
"${PSQL[@]}" <<SQL
DROP TABLE IF EXISTS _cleanup_pairs;
CREATE TABLE _cleanup_pairs AS
  SELECT c.table_name, c.column_name
  FROM information_schema.columns c
  WHERE c.table_schema = 'public'
    AND c.column_name IN (SELECT trim(unnest(string_to_array('$UID_COLS', ','))))
    -- 只删基表：视图（v_*）不可直接 DELETE，辅助表（_cleanup_*）是本脚本自建
    AND c.table_name IN (
      SELECT t.table_name FROM information_schema.tables t
      WHERE t.table_schema = 'public' AND t.table_type = 'BASE TABLE'
    )
    AND c.table_name NOT LIKE '\_cleanup%'
  ORDER BY c.table_name, c.column_name;
SQL

# ---------- dry-run 统计 ----------
info "---- 命中统计（每表每列）----"
TOTAL=0
"${PSQL[@]}" -t -A -F'|' -c "SELECT table_name, column_name FROM _cleanup_pairs" |
while IFS='|' read -r TBL COL; do
  CNT=$("${PSQL[@]}" -t -A -c "SELECT count(*) FROM \"$TBL\" WHERE \"$COL\" IN (SELECT uid FROM _cleanup_test_uids)" 2>/dev/null || echo SKIP)
  if [ "$CNT" = "SKIP" ]; then
    warn "  跳过（类型不匹配等）: $TBL.$COL"
  elif [ "$CNT" != "0" ]; then
    echo "  $TBL.$COL → $CNT 行"
  fi
done
N_GROUP=$("${PSQL[@]}" -t -A -c 'SELECT count(*) FROM "group" WHERE creator_uid IN (SELECT uid FROM _cleanup_test_uids) OR owner_uid IN (SELECT uid FROM _cleanup_test_uids)')
echo "  group（整群，按 creator/owner）→ $N_GROUP 行"
echo "  user（测试用户本体）→ $N_UIDS 行"

if [ "$APPLY" != "1" ]; then
  "${PSQL[@]}" -c "DROP TABLE _cleanup_pairs; DROP TABLE _cleanup_test_uids"
  info "DRY-RUN 结束。确认后执行: APPLY=1 bash $0"
  exit 0
fi

# ---------- 生成事务 SQL 并执行 ----------
TX="$WORKDIR/tx.sql"
{
  echo "BEGIN;"
  # 阶段 0：FK 链专用——按父 id 先删子行（父行随后由阶段 1 按 uid 删）。
  # 与 information_schema 的 54 条 FK 对齐（2026-08-29），新增表时在此补充。
  PRED='IN (SELECT uid FROM _cleanup_test_uids)'
  CH='creator_uid IN (SELECT uid FROM _cleanup_test_uids)'
  echo "DELETE FROM wallet_transaction WHERE wallet_id IN (SELECT id FROM wallet WHERE user_id $PRED);"
  echo "DELETE FROM bot_oauth_grant WHERE bot_id IN (SELECT id FROM bot WHERE user_id $PRED OR owner_uid $PRED);"
  echo "DELETE FROM agent_payment_compensation WHERE mandate_id IN (SELECT id FROM agent_payment_mandate WHERE owner_uid $PRED);"
  echo "DELETE FROM feedback_reply WHERE feedback_id IN (SELECT id FROM feedback WHERE user_id $PRED);"
  for TBL in channel_message_view channel_reaction channel_message channel_order channel_invitation channel_admin channel_subscription channel_price channel_stats_daily; do
    echo "DELETE FROM $TBL WHERE channel_id IN (SELECT id FROM channel WHERE $CH);"
  done
  for TBL in moment_comment moment_like moment_post_acl moment_report moment_timeline; do
    echo "DELETE FROM $TBL WHERE post_id IN (SELECT id FROM moment_post WHERE author_uid $PRED);"
  done
  # 阶段 1：除 user/group/group_member 外，按 (表,列) 逐对删；
  # 只纳入数值型列（text/uuid 等 uid 列在 dry-run 已告警跳过）
  "${PSQL[@]}" -t -A -c "
    SELECT format('DELETE FROM %I WHERE %I IN (SELECT uid FROM _cleanup_test_uids);', p.table_name, p.column_name)
    FROM _cleanup_pairs p
    JOIN information_schema.columns c
      ON c.table_schema = 'public' AND c.table_name = p.table_name AND c.column_name = p.column_name
    WHERE p.table_name NOT IN ('user', 'group', 'group_member')
      AND c.data_type IN ('bigint', 'integer', 'smallint')" |
  while IFS= read -r STMT; do [ -n "$STMT" ] && echo "$STMT"; done
  # 阶段 2：group_member（测试用户成员 + 测试群成员）
  echo "DELETE FROM group_member WHERE user_id IN (SELECT uid FROM _cleanup_test_uids)
     OR group_id IN (SELECT id FROM \"group\" WHERE creator_uid IN (SELECT uid FROM _cleanup_test_uids) OR owner_uid IN (SELECT uid FROM _cleanup_test_uids));"
  # 阶段 3：测试群
  echo "DELETE FROM \"group\" WHERE creator_uid IN (SELECT uid FROM _cleanup_test_uids) OR owner_uid IN (SELECT uid FROM _cleanup_test_uids);"
  # 阶段 4：测试用户本体
  echo "DELETE FROM \"user\" WHERE id IN (SELECT uid FROM _cleanup_test_uids);"
  echo "DROP TABLE _cleanup_pairs; DROP TABLE _cleanup_test_uids;"
  echo "COMMIT;"
} > "$TX"

info "APPLY=1：单事务执行删除（$(grep -c 'DELETE FROM' "$TX") 条 DELETE）…"
"${PSQL[@]}" -f "$TX"

LEFT=$("${PSQL[@]}" -t -A -c "SELECT count(*) FROM \"user\" WHERE reg_cosv = ANY(string_to_array('$TARGET_COSV', ','))")
LEFT_TOTAL=$("${PSQL[@]}" -t -A -c 'SELECT count(*) FROM "user"')
info "完成。剩余标记用户：${LEFT}；user 表总行数：$LEFT_TOTAL"

if [ "${VACUUM:-0}" = "1" ]; then
  info "VACUUM ANALYZE 回收空间…"
  "${PSQL[@]}" -c "VACUUM ANALYZE"
fi
info "全部完成"
