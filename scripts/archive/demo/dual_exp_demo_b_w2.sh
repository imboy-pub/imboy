#!/usr/bin/env bash
# dual_exp_demo_b_w2.sh — Demo B W2 演练（Channel-first-class W2：三卡四层全链 API 级脚本）
#
# 用法:
#   scripts/demo/dual_exp_demo_b_w2.sh                 # 后端须已运行在 127.0.0.1:9800
#   BASE=http://127.0.0.1:9800 scripts/demo/dual_exp_demo_b_w2.sh
#
# 前置:
#   * 本地后端已启动（IMBOYENV=local make run / scripts/start_node.sh，HTTP 9800）
#   * 本地 PG imboy_v1@4323 可达（teardown 按唯一前缀删除 + 残留核查只读 SELECT；
#     口令经 PGPASSWORD 注入，与 config/sys.local.config 一致，禁止硬编码入库）
#   * curl / jq / psql / escript 可用
#
# 覆盖场景（W2 发布计划 ZC-08 Demo B W2 全链）:
#   注册（唯一前缀账号 w2demo-<stamp>-u1/u2）→ Workspace Template
#   → 四关系之 workspace 成员邀请（DB 核查）
#   → Project 创建（断言 Owner 自动入项目 + 非 Project Member 直访 403）
#   → Task → Milestone（create→reach→重复 reach 幂等→status 参数 400→status 过滤）
#   → Channel 关联（link→重复 link 幂等→列表→unlink→unlink 后 404→重连）
#   → 四聚合（pinned/resources/activity/related_posts 空态与非空态断言）
#   → 归档（写操作被 980 拒绝：milestone/link/links/invite；读可通）
#   → 恢复（写放行）→ 移除成员后再邀不自动恢复历史状态（重邀语义）
#   → teardown（按唯一前缀清理全部痕迹 + 残留=0 自验证断言）
#
# 账号策略（与 W0 dual_exp_demo_b.sh 的降级链对齐后的决策）:
#   主路径直接用 scripts/imboy_ctl user create 创建唯一前缀账号——
#   本地共享库 license 用户数上限使 passport/signup 返回 402（W0 已登记），
#   imboy_ctl 为仓内既有无人值守通道；产生的账号纳入 teardown 前缀清理。
#   注册流程不发短信/邮件（local config sms switch=off；email type 不触发外发）。
#
# 断言数固定为 66；输出末尾打印"断言 N/66 通过"。
# 退出码: 0 = 全部 PASS；1 = 存在 FAIL。
set -u

BASE="${BASE:-http://127.0.0.1:9800}"
IMBOY_DIR="${IMBOY_DIR:-$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)}"
CTL="$IMBOY_DIR/scripts/imboy_ctl"

PGHOST="${PGHOST:-127.0.0.1}"; PGPORT="${PGPORT:-4323}"
PGUSER="${PGUSER:-imboy_user}"; PGDATABASE="${PGDATABASE:-imboy_v1}"
# 本地库口令经环境变量注入（与 config/sys.local.config 一致），禁止硬编码入库
export PGPASSWORD="${PGPASSWORD:-}"

# imboy_ctl 连接参数：与 start_node.sh / 默认 cookie 对齐
export IMBOY_CTL_NODE="${IMBOY_CTL_NODE:-imboy@127.0.0.1}"
export IMBOY_CTL_COOKIE="${IMBOY_CTL_COOKIE:-imboy}"

BODY_FILE="$(mktemp /tmp/demo_b_w2_body.XXXXXX)"
trap 'rm -f "$BODY_FILE"' EXIT

# 唯一前缀（时间戳+PID）：两遍演练天然不同前缀，避免污染共享库与其他断言
STAMP="$(date +%Y%m%d%H%M%S)-$$"
WS_PFX="W2Demo-W2-${STAMP}"        # workspace 名称前缀
# 账号前缀：user.account(varchar80) 与 imboy_ctl 写入的 user.mobile(varchar40)
# 共用同一字符串，总长必须 ≤40 —— 固定采用 w2d+14位秒级时间戳+2位随机+N 形态
ACC_STAMP="$(date +%Y%m%d%H%M%S)$(printf '%02d' $((RANDOM % 100)))"
ACC_PFX="w2d${ACC_STAMP}"           # 账号前缀（user.account LIKE 前缀清理用）

# 固定断言总数（与本文件 assert_* 调用数保持一致；teardown 自验证含在内）
TOTAL_ASSERTIONS=66

STEP_NO=0; PASS_N=0; FAIL_N=0; HAVE_FAIL=0

log()  { printf '%s\n' "$*"; }
step() { STEP_NO=$((STEP_NO+1)); log ""; log "=== [$STEP_NO] $1 ==="; }

summary_line() {
  log ""
  log "==============================================="
  if [ "$HAVE_FAIL" = "0" ]; then
    log "DEMO-B-W2 RESULT: ALL PASS (steps=$STEP_NO 断言 $PASS_N/$TOTAL_ASSERTIONS 通过)"
  else
    log "DEMO-B-W2 RESULT: FAILED (steps=$STEP_NO 断言 $PASS_N/$TOTAL_ASSERTIONS 通过, fail=$FAIL_N)"
  fi
  log "==============================================="
}

assert_eq() { # <描述> <实际> <期望>
  if [ "$2" = "$3" ]; then
    PASS_N=$((PASS_N+1)); log "PASS: $1"
  else
    FAIL_N=$((FAIL_N+1)); HAVE_FAIL=1; log "FAIL: $1 (actual=[$2] expected=[$3])"
  fi
}
assert_ne() { # <描述> <实际> <不应为空值判断>
  if [ "$2" != "$3" ]; then
    PASS_N=$((PASS_N+1)); log "PASS: $1"
  else
    FAIL_N=$((FAIL_N+1)); HAVE_FAIL=1; log "FAIL: $1 (值等于被排除值 [$3])"
  fi
}

# req <METHOD> <PATH> <TOKEN> <JSON|空>; 结果: CODE(HTTP) + BODY_FILE(响应体)
req() {
  local m="$1" p="$2" t="$3" d="${4:-}"
  local args=(-sS -m 25 -o "$BODY_FILE" -w '%{http_code}' -X "$m" "$BASE$p")
  [ -n "$t" ] && args+=(-H "Authorization: Bearer $t")
  if [ -n "$d" ]; then
    args+=(-H 'Content-Type: application/json' -d "$d")
  fi
  CODE="$(curl "${args[@]}" 2>/dev/null || echo 000)"
  log "--> $m $p (http=$CODE)"
  head -c 400 "$BODY_FILE" 2>/dev/null | tr '\n' ' ' | sed 's/  */ /g' | sed 's/^/    body: /'; log ""
}
api_ok() { # envelope 断言：HTTP 200 且 code=0
  [ "${CODE:-}" = "200" ] && [ "$(jget '.code')" = "0" ]
}
jget() { jq -r "$1 // empty" "$BODY_FILE" 2>/dev/null || true; }

pgq() { # 只读 SQL 单值查询（teardown 核查复用）
  PGPASSWORD="$PGPASSWORD" psql -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" \
    -d "$PGDATABASE" -At -c "$1" 2>/dev/null || echo "PSQL_ERR"
}
# teardown DELETE：单个 -c 多语句（同一隐式事务）+ ON_ERROR_STOP——
# 任一语句失败即非零退出且整体回滚，不静默吞错（成败必须反映到 transcript 与退出码）
pgx() {
  PGPASSWORD="$PGPASSWORD" psql -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" \
    -d "$PGDATABASE" -q -v ON_ERROR_STOP=1 -c "$1"
}

# ─────────────────────────────────────────────────────────────
step "P0 环境就绪：GET /api/v1/init"
req GET "/api/v1/init" "" ""
assert_eq "init HTTP 200" "$CODE" "200"

# ─────────────────────────────────────────────────────────────
step "P1 创建唯一前缀账号 u1/u2（imboy_ctl；license 上限下 signup 402 的仓内通道）"
UID_A="$(( $(date +%s) * 100000 + $$ * 10 + 1 ))"
UID_B="$(( $(date +%s) * 100000 + $$ * 10 + 2 ))"
ACC_A="${ACC_PFX}u1@smoke.local"; PWD_A="W2Demo1-$(openssl rand -hex 4)"
ACC_B="${ACC_PFX}u2@smoke.local"; PWD_B="W2Demo1-$(openssl rand -hex 4)"

escript "$CTL" user create "$UID_A" -a "$ACC_A" -n w2demoA -p "$PWD_A" >/dev/null 2>&1
TOKEN_A="$(escript "$CTL" user token "$UID_A" 2>/dev/null | tail -1)"
assert_ne "用户 A 就绪(uid=${UID_A})" "$TOKEN_A" ""

escript "$CTL" user create "$UID_B" -a "$ACC_B" -n w2demoB -p "$PWD_B" >/dev/null 2>&1
TOKEN_B="$(escript "$CTL" user token "$UID_B" 2>/dev/null | tail -1)"
assert_ne "用户 B 就绪(uid=${UID_B})" "$TOKEN_B" ""
# imboy_ctl create 失败静默（输出丢弃）会以 FK 500 在 P2 才暴露——此处前置校验账号已落库
CNT_U="$(pgq "SELECT count(*) FROM \"user\" WHERE account IN ('$ACC_A','$ACC_B')")"
assert_eq "前置校验：u1/u2 账号已落库（count=2，防 mobile 超长静默失败）" "$CNT_U" "2"

# ─────────────────────────────────────────────────────────────
step "P2 A 创建 Workspace（Template 原子初始化）"
WS_NAME="${WS_PFX}"
REQ_ID="w2demo-create-$STAMP"
req POST "/api/v1/workspaces" "$TOKEN_A" "{\"name\":\"$WS_NAME\",\"request_id\":\"$REQ_ID\"}"
assert_eq "Template 返回 status=created" "$(jget '.payload.status')" "created"
WS_ID="$(jget '.payload.workspace_id')"
GID_GENERAL="$(jget '.payload.group_id')"
CID_ANN="$(jget '.payload.channel_id')"
assert_ne "workspace_id 取得($WS_ID)" "$WS_ID" ""
assert_ne "General 群 id 取得($GID_GENERAL)" "$GID_GENERAL" ""
assert_ne "Announcements 频道 id 取得($CID_ANN)" "$CID_ANN" ""

# ─────────────────────────────────────────────────────────────
step "P3 四关系之 workspace 成员邀请：A 邀 B（member）+ DB 核查"
sleep 1
req POST "/api/v1/workspaces/$WS_ID/members/invite" "$TOKEN_A" "{\"user_id\":$UID_B,\"role\":\"member\"}"
assert_eq "workspace 成员邀请成功" "$(api_ok && echo OK || echo NO)" "OK"
CNT_WS="$(pgq "SELECT count(*) FROM workspace_member WHERE workspace_id=$WS_ID AND user_id=$UID_B AND status='active'")"
assert_eq "DB 核查：B 是 active workspace_member" "$CNT_WS" "1"

# ─────────────────────────────────────────────────────────────
step "P4 A 创建 Project：Owner 自动入项目 + B 直访 403（W2 项目隔离）"
sleep 1
req POST "/api/v1/workspaces/$WS_ID/projects" "$TOKEN_A" \
  "{\"name\":\"W2Demo-Project-$STAMP\",\"description\":\"demo-b W2 project\"}"
assert_eq "项目创建成功" "$(api_ok && echo OK || echo NO)" "OK"
PID="$(jget '.payload.id')"
assert_ne "project id 取得($PID)" "$PID" ""

sleep 1
req GET "/api/v1/projects/$PID/members" "$TOKEN_A"
assert_eq "members 列表可读" "$(api_ok && echo OK || echo NO)" "OK"
OWNER_IN="$(jq -r --argjson o "$UID_A" '[.payload.list[]?.user_id] | index($o) != null' "$BODY_FILE" 2>/dev/null)"
assert_eq "Owner 自动入项目（members 含 owner uid=${UID_A}）" "$OWNER_IN" "true"
CNT_M="$(jget '.payload.total')"
assert_eq "初始 members 仅 Owner 一人（total=1）" "$CNT_M" "1"

sleep 1
req GET "/api/v1/projects/$PID/members" "$TOKEN_B"
assert_eq "B 直访 members 被拒 code=403（非 Project Member）" "$(jget '.code')" "403"

# ─────────────────────────────────────────────────────────────
step "P5 Task：A 建任务指派 B（workspace member 可指派）"
sleep 1
req POST "/api/v1/projects/$PID/tasks" "$TOKEN_A" \
  "{\"title\":\"W2Demo 任务 $STAMP\",\"assignee_id\":$UID_B}"
assert_eq "任务创建并指派成功" "$(api_ok && echo OK || echo NO)" "OK"
TID="$(jget '.payload.id')"
assert_ne "task id 取得($TID)" "$TID" ""

# ─────────────────────────────────────────────────────────────
step "P6 Milestone：create → status 参数 400 → reach → 重复 reach 幂等 → status 过滤"
sleep 1
req POST "/api/v1/projects/$PID/milestones" "$TOKEN_A" \
  "{\"name\":\"W2Demo-M1-$STAMP\",\"due_date\":\"2026-09-30\"}"
assert_eq "里程碑创建成功" "$(api_ok && echo OK || echo NO)" "OK"
MSID="$(jget '.payload.id')"
assert_ne "milestone id 取得($MSID)" "$MSID" ""
assert_eq "初始 status=planned" "$(jget '.payload.status')" "planned"

sleep 1
req POST "/api/v1/projects/$PID/milestones" "$TOKEN_A" \
  "{\"name\":\"bad\",\"status\":\"reached\"}"
assert_eq "create/update 携带 status 字段被 400 拒绝（状态机唯一入口 /reach）" "$(jget '.code')" "400"

sleep 1
req POST "/api/v1/milestones/$MSID/reach" "$TOKEN_A" "{}"
assert_eq "reach → status_flag=reached" "$(jget '.payload.status_flag')" "reached"
sleep 1
req POST "/api/v1/milestones/$MSID/reach" "$TOKEN_A" "{}"
assert_eq "重复 reach 幂等 → status_flag=already_reached" "$(jget '.payload.status_flag')" "already_reached"

sleep 1
req GET "/api/v1/projects/$PID/milestones?status=reached" "$TOKEN_A"
REACHED_HAS="$(jq -r --argjson m "$MSID" '[.payload.list[]?.id] | index($m) != null' "$BODY_FILE" 2>/dev/null)"
assert_eq "list?status=reached 过滤含已达成里程碑" "$REACHED_HAS" "true"

# ─────────────────────────────────────────────────────────────
step "P7 Channel 关联：link → 幂等 → 列表 → unlink → 404 → 重连"
sleep 1
req POST "/api/v1/projects/$PID/channels" "$TOKEN_A" "{\"channel_id\":$CID_ANN}"
assert_eq "link 频道 → status_flag=created" "$(jget '.payload.status_flag')" "created"
sleep 1
req POST "/api/v1/projects/$PID/channels" "$TOKEN_A" "{\"channel_id\":$CID_ANN}"
assert_eq "重复 link 幂等 → status_flag=existing" "$(jget '.payload.status_flag')" "existing"

sleep 1
req GET "/api/v1/projects/$PID/channels" "$TOKEN_A"
LINKED_HAS="$(jq -r --argjson c "$CID_ANN" '[.payload.list[]?.channel_id] | index($c) != null' "$BODY_FILE" 2>/dev/null)"
assert_eq "关联频道列表含 Announcements" "$LINKED_HAS" "true"

sleep 1
req POST "/api/v1/channel/$CID_ANN/message" "$TOKEN_A" \
  "{\"content\":\"[W2Demo] related-posts seed $STAMP\",\"msg_type\":\"text\",\"request_id\":\"w2demo-msg-$STAMP\"}"
assert_eq "关联频道发帖成功（related_posts 数据源）" "$(api_ok && echo OK || echo NO)" "OK"

sleep 1
req GET "/api/v1/projects/$PID/aggregations/related_posts" "$TOKEN_A"
assert_eq "related_posts 非空（len>0）" "$(jq -r '.payload | length > 0' "$BODY_FILE" 2>/dev/null)" "true"
RP_CLEAN="$(jq -r '[.payload[] | keys[]?] | any(. == "content" or . == "body") | not' "$BODY_FILE" 2>/dev/null)"
assert_eq "related_posts 为有界摘要（无正文字段）" "$RP_CLEAN" "true"

sleep 1
req POST "/api/v1/projects/$PID/channels/$CID_ANN/unlink" "$TOKEN_A" "{}"
assert_eq "unlink → status_flag=unlinked" "$(jget '.payload.status_flag')" "unlinked"
sleep 1
req POST "/api/v1/projects/$PID/channels/$CID_ANN/unlink" "$TOKEN_A" "{}"
assert_eq "unlink 缺失关联 → 404（定向删除语义）" "$(jget '.code')" "404"

sleep 1
req POST "/api/v1/projects/$PID/channels" "$TOKEN_A" "{\"channel_id\":$CID_ANN}"
assert_eq "重新 link（为归档/聚合场景保持关联态）→ created" "$(jget '.payload.status_flag')" "created"

# ─────────────────────────────────────────────────────────────
step "P8 四聚合：pinned/resources 空态 + activity 非空 + B 入项目后成员可读"
sleep 1
req GET "/api/v1/projects/$PID/aggregations/pinned?page=1&size=20" "$TOKEN_A"
assert_eq "pinned 可用且空态（total=0）" "$(jget '.payload.total')" "0"

sleep 1
req GET "/api/v1/projects/$PID/aggregations/resources" "$TOKEN_A"
assert_eq "resources 空态（links 未配置返回 []）" "$(jget '.payload | length')" "0"

sleep 1
req POST "/api/v1/projects/$PID/links/update" "$TOKEN_A" \
  "{\"links\":[{\"name\":\"w2demo-docs\",\"url\":\"https://example.com/w2demo/$STAMP\"}]}"
assert_eq "update_links 全量替换成功" "$(api_ok && echo OK || echo NO)" "OK"
sleep 1
req GET "/api/v1/projects/$PID/aggregations/resources" "$TOKEN_A"
RES_N="$(jq -r '.payload | length' "$BODY_FILE" 2>/dev/null)"
assert_eq "resources 非空（links 返回，len=${RES_N}）" "$RES_N" "1"

sleep 1
req GET "/api/v1/projects/$PID/aggregations/activity?page=1&size=20" "$TOKEN_A"
ACT_TOTAL="$(jget '.payload.total')"
if [ "${ACT_TOTAL:-0}" -gt 0 ] 2>/dev/null; then
  PASS_N=$((PASS_N+1)); log "PASS: activity 可用且非空（total=${ACT_TOTAL}）"
else
  FAIL_N=$((FAIL_N+1)); HAVE_FAIL=1; log "FAIL: activity 空或不可用（total=${ACT_TOTAL}）"
fi
ACT_CLEAN="$(jq -r '[.payload.list[]?.payload | keys[]?] | any(. == "content" or . == "body") | not' "$BODY_FILE" 2>/dev/null)"
assert_eq "activity 仅元数据（payload 无正文字段）" "$ACT_CLEAN" "true"

sleep 1
req POST "/api/v1/projects/$PID/members/invite" "$TOKEN_A" "{\"user_id\":$UID_B}"
assert_eq "A 邀 B 入项目 → status_flag=created" "$(jget '.payload.status_flag')" "created"
sleep 1
req GET "/api/v1/projects/$PID/aggregations/pinned?page=1&size=20" "$TOKEN_B"
assert_eq "B（active Project Member）可读 pinned 聚合" "$(api_ok && echo OK || echo NO)" "OK"

# ─────────────────────────────────────────────────────────────
step "P9 归档 Workspace：W2 写端点全被 980 拒，读可通"
sleep 1
req POST "/api/v1/workspaces/$WS_ID/archive" "$TOKEN_A" "{}"
assert_eq "归档成功" "$(api_ok && echo OK || echo NO)" "OK"

sleep 1
req POST "/api/v1/projects/$PID/milestones" "$TOKEN_A" "{\"name\":\"archived-nope-$STAMP\"}"
assert_eq "归档后 milestone create 被拒 code=980" "$(jget '.code')" "980"
sleep 1
req POST "/api/v1/milestones/$MSID/reach" "$TOKEN_A" "{}"
assert_eq "归档后 milestone reach 被拒 code=980" "$(jget '.code')" "980"
sleep 1
req POST "/api/v1/projects/$PID/channels" "$TOKEN_A" "{\"channel_id\":$CID_ANN}"
assert_eq "归档后 channel link 被拒 code=980" "$(jget '.code')" "980"
sleep 1
req POST "/api/v1/projects/$PID/links/update" "$TOKEN_A" "{\"links\":[{\"name\":\"nope\",\"url\":\"https://example.com/nope\"}]}"
assert_eq "归档后 links/update 被拒 code=980" "$(jget '.code')" "980"
sleep 1
req POST "/api/v1/projects/$PID/members/invite" "$TOKEN_A" "{\"user_id\":$UID_B}"
assert_eq "归档后 project member invite 被拒 code=980" "$(jget '.code')" "980"

sleep 1
req GET "/api/v1/projects/$PID/aggregations/pinned?page=1&size=20" "$TOKEN_A"
assert_eq "归档后 pinned 聚合读可通" "$(api_ok && echo OK || echo NO)" "OK"
sleep 1
req GET "/api/v1/projects/$PID/members" "$TOKEN_A"
assert_eq "归档后 members 列表读可通" "$(api_ok && echo OK || echo NO)" "OK"
sleep 1
req GET "/api/v1/projects/$PID/aggregations/activity?page=1&size=20" "$TOKEN_A"
assert_eq "归档后 activity 聚合读可通" "$(api_ok && echo OK || echo NO)" "OK"

# ─────────────────────────────────────────────────────────────
step "P10 恢复 Workspace → W2 写放行"
sleep 1
req POST "/api/v1/workspaces/$WS_ID/restore" "$TOKEN_A" "{}"
assert_eq "恢复成功" "$(api_ok && echo OK || echo NO)" "OK"
sleep 1
req POST "/api/v1/projects/$PID/links/update" "$TOKEN_A" \
  "{\"links\":[{\"name\":\"w2demo-after-restore\",\"url\":\"https://example.com/restore/$STAMP\"}]}"
assert_eq "恢复后 links/update 放行" "$(api_ok && echo OK || echo NO)" "OK"

# ─────────────────────────────────────────────────────────────
step "P11 移除成员后再邀：不自动恢复历史状态（W2 重邀语义）"
sleep 1
req POST "/api/v1/projects/$PID/members/remove" "$TOKEN_A" "{\"user_id\":$UID_B}"
assert_eq "移除 B → status_flag=removed" "$(jget '.payload.status_flag')" "removed"

sleep 1
req GET "/api/v1/projects/$PID/members" "$TOKEN_B"
assert_eq "移除后 B 直访 members → 403" "$(jget '.code')" "403"
PM_STATUS="$(pgq "SELECT status FROM project_member WHERE project_id=$PID AND user_id=$UID_B")"
assert_eq "DB 核查：B 的 project_member 行 status=removed" "$PM_STATUS" "removed"

sleep 1
req POST "/api/v1/projects/$PID/members/invite" "$TOKEN_A" "{\"user_id\":$UID_B}"
assert_eq "重邀 → status_flag=created（覆盖激活，非静默 existing）" "$(jget '.payload.status_flag')" "created"
INV_BY="$(pgq "SELECT invited_by FROM project_member WHERE project_id=$PID AND user_id=$UID_B")"
assert_eq "DB 核查：invited_by 重置为本次邀请人 A（历史不保留）" "$INV_BY" "$UID_A"
PM_OWNER="$(pgq "SELECT owner_id FROM project WHERE id=$PID")"
assert_eq "DB 核查：重邀不改变项目 Owner（仍为 A）" "$PM_OWNER" "$UID_A"
sleep 1
req GET "/api/v1/projects/$PID/members" "$TOKEN_B"
assert_eq "重邀后 B 直访 members 恢复 200（恢复的是成员身份本身）" "$(api_ok && echo OK || echo NO)" "OK"

# ─────────────────────────────────────────────────────────────
step "P12 teardown：按唯一前缀清理全部痕迹 + 残留=0 自验证"
WSIDS="$(pgq "SELECT coalesce(string_agg(id::text, ','), '-1') FROM workspace WHERE name LIKE '${WS_PFX}%'")"
UIDS="$(pgq "SELECT coalesce(string_agg(id::text, ','), '-1') FROM \"user\" WHERE account LIKE '${ACC_PFX}%'")"
log "    teardown scope: WSIDS=[$WSIDS] UIDS=[$UIDS] ACC_PFX=[$ACC_PFX]"
if [ "$WSIDS" = "PSQL_ERR" ] || [ "$UIDS" = "PSQL_ERR" ] || [ -z "$WSIDS" ] || [ -z "$UIDS" ]; then
  FAIL_N=$((FAIL_N+1)); HAVE_FAIL=1
  log "FAIL: teardown 范围解析失败（WSIDS/UIDS 不可用；检查 PGPASSWORD/PG 连接）"
else
  TEARDOWN_SQL="
DELETE FROM project_event     WHERE project_id IN (SELECT id FROM project WHERE workspace_id IN ($WSIDS));
DELETE FROM project_member    WHERE workspace_id IN ($WSIDS) OR user_id IN ($UIDS);
DELETE FROM project_milestone WHERE workspace_id IN ($WSIDS);
DELETE FROM project_channel_rel WHERE workspace_id IN ($WSIDS);
DELETE FROM project_task      WHERE project_id IN (SELECT id FROM project WHERE workspace_id IN ($WSIDS));
DELETE FROM project           WHERE workspace_id IN ($WSIDS);
DELETE FROM channel_comment   WHERE channel_id IN (SELECT id FROM channel WHERE workspace_id IN ($WSIDS));
DELETE FROM channel_message   WHERE channel_id IN (SELECT id FROM channel WHERE workspace_id IN ($WSIDS)) OR author_id IN ($UIDS);
DELETE FROM channel_subscription WHERE channel_id IN (SELECT id FROM channel WHERE workspace_id IN ($WSIDS)) OR user_id IN ($UIDS);
DELETE FROM channel           WHERE workspace_id IN ($WSIDS);
DELETE FROM group_member      WHERE group_id IN (SELECT id FROM \"group\" WHERE workspace_id IN ($WSIDS)) OR user_id IN ($UIDS);
DELETE FROM \"group\"         WHERE workspace_id IN ($WSIDS);
DELETE FROM workspace_member  WHERE workspace_id IN ($WSIDS) OR user_id IN ($UIDS);
DELETE FROM workspace         WHERE id IN ($WSIDS);
DELETE FROM conversation      WHERE user_id IN ($UIDS);
DELETE FROM \"user\"          WHERE id IN ($UIDS);"
  if pgx "$TEARDOWN_SQL"; then
    PASS_N=$((PASS_N+1)); log "PASS: teardown DELETE 单事务执行成功（ON_ERROR_STOP）"
  else
    FAIL_N=$((FAIL_N+1)); HAVE_FAIL=1; log "FAIL: teardown DELETE 执行失败（见上方 psql 报错）"
  fi
fi

R_WS="$(pgq "SELECT count(*) FROM workspace WHERE name LIKE '${WS_PFX}%'")"
assert_eq "teardown 自验证：workspace 残留=0" "$R_WS" "0"
R_PJ="$(pgq "SELECT count(*) FROM project WHERE name LIKE '%${STAMP}%'")"
assert_eq "teardown 自验证：project 残留=0" "$R_PJ" "0"
R_U="$(pgq "SELECT count(*) FROM \"user\" WHERE account LIKE '${ACC_PFX}%'")"
assert_eq "teardown 自验证：user 残留=0" "$R_U" "0"
R_CH="$(pgq "SELECT count(*) FROM channel WHERE name LIKE 'Announcements' AND workspace_id IS NOT NULL AND workspace_id NOT IN (SELECT id FROM workspace)")"
assert_eq "teardown 自验证：孤儿 workspace 频道=0（共享库无残留）" "$R_CH" "0"
R_PM="$(pgq "SELECT count(*) FROM project_member pm LEFT JOIN \"user\" u ON u.id=pm.user_id WHERE u.id IS NULL")"
assert_eq "teardown 自验证：孤儿 project_member 行=0" "$R_PM" "0"

# ─────────────────────────────────────────────────────────────
summary_line
exit "$HAVE_FAIL"
