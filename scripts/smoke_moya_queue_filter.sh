#!/usr/bin/env bash
# ============================================================
# 墨芽待评队列「快速检索」联调冒烟 / Moya review-queue filter smoke
# ------------------------------------------------------------
# 为什么需要它：
#   按作业 / 提交人 / 提交时间范围收窄这条链路，参数要穿过
#   handler（读 QS）→ logic（白名单归一）→ repo（拼 SQL）→ PG 四层。
#   任何一层静默丢弃或拼错，前端看到的都是「筛了没反应」，而：
#     · 单元测试只覆盖纯函数，不碰真 PG；
#     · 单测全绿也不代表常驻进程已重载新代码（本项目已发生数次）。
#   本脚本对「真实 HTTP + 真实 PG」取证，且**期望值现场从库里算出来**，
#   不写死行数 —— 换库/换数据也能用。
#
# 前置：后端在跑（默认 127.0.0.1:9700）。
# 用法：bash scripts/smoke_moya_queue_filter.sh [BASE_URL]
# 退出码：0=全过；1=有断言失败（含「进程跑旧代码」）
# 只读：全部 SELECT / GET，不改任何数据；**不打印 jwt_key**。
# ============================================================
set -uo pipefail

IMBOY_ROOT="${IMBOY_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
BASE="${1:-${BASE:-http://127.0.0.1:9700}}"

PG_HOST="${PG_HOST:-127.0.0.1}"
PG_PORT="${PG_PORT:-4323}"
PG_USER="${PG_USER:-imboy_user}"
PG_DB="${PG_DB:-imboy_v1}"
PG_PASS="${PG_PASS:-abc54321}"

PASS=0
FAIL=0
ok() { PASS=$((PASS + 1)); echo "  ✓ $1"; }
bad() { FAIL=$((FAIL + 1)); echo "  ✗ $1"; }

sql() { PGPASSWORD="$PG_PASS" psql -h "$PG_HOST" -p "$PG_PORT" -U "$PG_USER" -d "$PG_DB" -X -tAc "$1" 2>/dev/null; }

echo "== 目标后端 ${BASE} =="

# ---- 0. 服务探活（注意健康检查路径是 /healthz，不是 /api/v1/healthz） ----
if [ "$(curl -s -o /dev/null -m 5 -w '%{http_code}' "${BASE}/healthz")" != "200" ]; then
  echo "  ✗ ${BASE}/healthz 未就绪 —— 后端没起来？"
  exit 1
fi
ok "服务探活 /healthz"

# ---- 1. 取教师身份（token 的 uid 必须是真实的 teacher，否则 ACL 会拒） ----
TEACHER_UID="$(sql "SELECT user_id FROM class_staff WHERE role='teacher' AND status='active' ORDER BY user_id LIMIT 1")"
if [ -z "$TEACHER_UID" ]; then
  echo "  ✗ 库里找不到 active 的 teacher（class_staff）—— 无法取得有权限的身份"
  exit 1
fi
ok "教师身份 uid=${TEACHER_UID}"

# 教师可见的班级集合（与后端 queue 的 ACL 口径一致）
GROUPS_SQL="SELECT group_id FROM class_staff WHERE user_id=${TEACHER_UID} AND role='teacher' AND status='active'"

# ---- 2. 现场签发教师 token（HS256 + 本地 jwt_key；只读配置，不打印） ----
JWT_KEY="$(sed -n 's/.*{jwt_key,[[:space:]]*<<"\([^"]*\)">>}.*/\1/p' "${IMBOY_ROOT}/config/sys.local.config" | head -1)"
if [ -z "$JWT_KEY" ]; then
  echo "  ✗ 读不到 config/sys.local.config 的 jwt_key"
  exit 1
fi
TOKEN="$(cd "$IMBOY_ROOT" && JWT_KEY="$JWT_KEY" TEACHER_UID="$TEACHER_UID" erl -noshell \
  -pa ebin -pa deps/jose/ebin -pa deps/jsx/ebin -eval '
    K = list_to_binary(os:getenv("JWT_KEY")),
    U = list_to_integer(os:getenv("TEACHER_UID")),
    E = erlang:system_time(second) + 600,
    io:format("~s", [jwerl:sign(#{sub => <<"tk">>, exp => E, uid => U}, hs256, K)]),
    halt(0).' 2>/dev/null | tail -1)"
if [ -z "$TOKEN" ]; then
  echo "  ✗ 签发 token 失败（jose/shim 没编好？make compile 后重试）"
  exit 1
fi
ok "签发教师 token（长度 ${#TOKEN}）"

# ---- 3. 发起请求并取出业务码与 total ----
# 注意：本项目业务错误走 **HTTP 200 + 信封 code**，所以断言 code 而不是 HTTP 状态。
api() {
  curl -s -m 15 -H "Authorization: Bearer ${TOKEN}" "${BASE}/api/v1/moya/review-queue?$1"
}
# 信封结构：{"code":..,"msg":..,"payload":{"list":[..],"total":N}} —— code 在外层，
# total/list 在 payload 里。混用会得到 None，是本脚本第一次跑就踩的坑。
biz_code() { printf '%s' "$1" | python3 -c "import sys,json;print(json.load(sys.stdin).get('code'))" 2>/dev/null; }
pl_total() { printf '%s' "$1" | python3 -c "import sys,json;print(json.load(sys.stdin).get('payload',{}).get('total'))" 2>/dev/null; }

# ---- 4. 基准 ----
BASE_BODY="$(api 'size=1&page=1')"
BASE_CODE="$(biz_code "$BASE_BODY")"
BASE_TOTAL="$(pl_total "$BASE_BODY")"
if [ "$BASE_CODE" != "0" ]; then
  bad "基准请求失败：$(printf '%s' "$BASE_BODY" | head -c 160)"
  echo "== 通过 ${PASS} / 失败 ${FAIL} =="
  exit 1
fi
ok "基准：total=${BASE_TOTAL}"
if [ "${BASE_TOTAL:-0}" = "0" ]; then
  echo "  ⚠ 基准为 0，后续收窄断言无从分辨；请先在待评队列造数据"
fi

# ---- 5. 按作业收窄（期望值从库里算） ----
GT_ID="$(sql "SELECT gt.id FROM group_task gt JOIN group_task_assignment a ON a.task_id = gt.task_id JOIN homework_submission hs ON hs.assignment_id = a.id WHERE gt.group_id IN (${GROUPS_SQL}) AND hs.status='submitted' GROUP BY gt.id ORDER BY count(*) DESC LIMIT 1")"
if [ -n "$GT_ID" ]; then
  WANT_TASK="$(sql "SELECT count(*) FROM homework_submission hs JOIN group_task_assignment a ON a.id = hs.assignment_id JOIN group_task gt ON gt.task_id = a.task_id JOIN \"group\" g ON g.id = gt.group_id WHERE hs.status='submitted' AND g.id IN (${GROUPS_SQL}) AND gt.id = ${GT_ID}")"
  GOT_TASK="$(pl_total "$(api "size=1&page=1&task_id=${GT_ID}")")"
  if [ "$GOT_TASK" = "$WANT_TASK" ] && [ "$GOT_TASK" != "$BASE_TOTAL" ]; then
    ok "按作业 task_id=${GT_ID} 收窄：${BASE_TOTAL} → ${GOT_TASK}"
  else
    bad "按作业 task_id=${GT_ID}：得到 ${GOT_TASK}，库里应为 ${WANT_TASK}（若等于基准 ${BASE_TOTAL} 说明条件被忽略）"
  fi
else
  echo "  ⚠ 该班级没有已提交的作业，跳过作业收窄断言"
fi

# ---- 6. 按提交人收窄 ----
# 用「不存在的 learner」证伪：若条件生效必为 0；若被丢弃会等于基准。
GOT_NONE="$(pl_total "$(api "size=1&page=1&learner_id=1")")"
if [ "$GOT_NONE" = "0" ]; then
  ok "按不存在的提交人 learner_id=1 → 0（条件确实生效）"
else
  bad "按不存在的提交人得到的 total=${GOT_NONE}（应 0；等于基准 ${BASE_TOTAL} 即条件被忽略）"
fi

# ---- 7. 按提交时间范围收窄 ----
# 白名单（?TIME_PARAM_RE）放行 5 种形态，其中「无时区」三种最隐蔽：
# codec 的 elib_dt:rfc3339_to/2 解析不了它们 → 退化为 PG 纪元 2000-01-01 →
# 比较恒真 → 过滤静默变成「不过滤」，返回全量且不报错。
# 所以只测带偏移的形态会漏掉它，必须逐形态验证「未来时间 → 0」。
# 注意：`+` 在 query string 里会被解成空格，偏移必须编码成 %2B。
q_enc() { printf '%s' "$1" | sed 's/+/%2B/g'; }

TOMORROW="$(date -v+1d +%Y-%m-%d 2>/dev/null || date -d '+1 day' +%Y-%m-%d)"
for SHAPE in "" "T00:00" "T00:00:00" "T00:00:00+08:00" "T00:00:00Z"; do
  V="${TOMORROW}${SHAPE}"
  GOT="$(pl_total "$(api "size=1&page=1&submitted_from=$(q_enc "$V")")")"
  if [ "$GOT" = "0" ]; then
    ok "submitted_from=${V}（未来）→ 0（该形态确实生效）"
  else
    bad "submitted_from=${V} 得到 total=${GOT}（应 0；等于基准 ${BASE_TOTAL} 即条件被静默忽略）"
  fi
done

TODAY="$(date +%Y-%m-%d)"
WANT_TODAY="$(sql "SELECT count(*) FROM homework_submission hs JOIN group_task_assignment a ON a.id = hs.assignment_id JOIN group_task gt ON gt.task_id = a.task_id JOIN \"group\" g ON g.id = gt.group_id WHERE hs.status='submitted' AND g.id IN (${GROUPS_SQL}) AND hs.submitted_at >= (('${TODAY}T00:00:00'::text)::timestamptz) AND hs.submitted_at < ((('${TODAY}T00:00:00'::text)::timestamptz) + interval '1 day')")"
GOT_TODAY="$(pl_total "$(api "size=1&page=1&submitted_from=${TODAY}T00:00:00&submitted_to=${TOMORROW}T00:00:00")")"
if [ "$GOT_TODAY" = "$WANT_TODAY" ]; then
  ok "今日闭开区间 [${TODAY}, ${TOMORROW}) → ${GOT_TODAY}（与库一致）"
else
  bad "今日区间得到 ${GOT_TODAY}，库里应为 ${WANT_TODAY}"
fi

# ---- 8. 非法时间格式必须被拒（deny-by-default，业务码 422） ----
BAD_CODE="$(biz_code "$(api "size=1&page=1&submitted_from=oops")")"
if [ "$BAD_CODE" = "422" ]; then
  ok "非法时间格式 → 业务码 422（未把未校验串塞进 SQL）"
else
  bad "非法时间格式返回 code=${BAD_CODE}（应 422；若为 0 说明校验未生效，可能跑着旧代码）"
fi

# ---- 9. 新代码标志：手动 AI 触发端点是否已加载 ----
# 401 = 路由存在且需鉴权（新代码已加载）；404 = 进程还是旧 beam。
AI_CODE="$(curl -s -o /dev/null -m 5 -w '%{http_code}' -X POST "${BASE}/api/v1/moya/submissions/1/ai-draft")"
if [ "$AI_CODE" = "401" ]; then
  ok "手动 AI 触发端点已加载（HTTP 401 = 存在且需鉴权）"
else
  bad "手动 AI 触发端点 HTTP ${AI_CODE}（404 表示进程仍在跑旧代码，需重启后端）"
fi

echo "== 通过 ${PASS} / 失败 ${FAIL} =="
[ "$FAIL" -eq 0 ]
