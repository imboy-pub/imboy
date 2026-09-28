#!/usr/bin/env bash
# ============================================================
# 墨芽 AI 回课「手动触发」端到端冒烟 / Moya AI draft manual trigger smoke
# ------------------------------------------------------------
# 为什么需要它：
#   老师端「让 AI 看一遍」这条链路要穿过
#   handler（POST :id/ai-draft）→ logic（draft_guard/draft 幂等分支）→
#   repo（入队/重排）→ elib_async（进程内定点 run_draft）→ worker → PG 五层。
#   任何一层静默失效，老师看到的都是一屏不动的「整理中…」——**看起来像假的**，
#   而不是报错。2026-09-14 的现场正是如此：calligraphy_review_draft 89/89 行停在
#   queued、0 行曾进入 running（本地 ecron 从无 teaching_ai_worker 作业 → 无人领取）。
#
#   本脚本用真实 HTTP + 真实 PG 证明两件事：
#     ① 手动触发**不依赖 ecron**：POST 后草稿会离开 queued（走进程内 run_draft）；
#     ② 离开后落到哪个终态是**可解释**的：本地未配 teaching_ai_llm_provider 时
#        必然是 failed + provider_unavailable，而不是沉默。
#
# ⚠️ 本脚本会**改数据**（这就是被测行为本身）：它会把指定提交的 AI 草稿从
#    queued 推进到终态。只动一行、只动 AI 草稿，且前后都打印，便于复核。
#    默认取「最近一行 queued 草稿」；也可显式指定：bash $0 <submission_id>
#
# 前置：后端在跑（默认 127.0.0.1:9700）。
# 用法：bash scripts/smoke_moya_ai_draft.sh [submission_id] [BASE_URL]
# 退出码：0=链路通（草稿确实离开 queued）；1=失败
# ============================================================
set -uo pipefail

IMBOY_ROOT="${IMBOY_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
SUBMISSION_ID="${1:-}"
BASE="${2:-${BASE:-http://127.0.0.1:9700}}"

PG_HOST="${PG_HOST:-127.0.0.1}"
PG_PORT="${PG_PORT:-4323}"
PG_USER="${PG_USER:-imboy_user}"
PG_DB="${PG_DB:-imboy_v1}"
PG_PASS="${PG_PASS:-abc54321}"

PASS=0
FAIL=0
ok() { PASS=$((PASS + 1)); echo "  ✓ $1"; }
bad() { FAIL=$((FAIL + 1)); echo "  ✗ $1"; }
info() { echo "  · $1"; }

sql() { PGPASSWORD="$PG_PASS" psql -h "$PG_HOST" -p "$PG_PORT" -U "$PG_USER" -d "$PG_DB" -X -tAc "$1" 2>/dev/null; }

echo "== 目标后端 ${BASE} =="

# ---- 0. 探活（健康检查路径是 /healthz，不是 /api/v1/healthz） ----
if [ "$(curl -s -o /dev/null -m 5 -w '%{http_code}' "${BASE}/healthz")" != "200" ]; then
  echo "  ✗ ${BASE}/healthz 未就绪 —— 后端没起来？"
  exit 1
fi
ok "服务探活 /healthz"

# ---- 1. 教师身份（token 的 uid 必须是该提交所在班级的 teacher/manager，否则 ACL 拒） ----
TEACHER_UID="$(sql "SELECT user_id FROM class_staff WHERE role='teacher' AND status='active' ORDER BY user_id LIMIT 1")"
if [ -z "$TEACHER_UID" ]; then
  echo "  ✗ 库里找不到 active 的 teacher（class_staff）"
  exit 1
fi
ok "教师身份 uid=${TEACHER_UID}"

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

# ---- 3. 被测对象：默认取最近一行 queued 草稿 ----
if [ -z "$SUBMISSION_ID" ]; then
  SUBMISSION_ID="$(sql "SELECT submission_id FROM calligraphy_review_draft WHERE status='queued' ORDER BY id DESC LIMIT 1")"
fi
if [ -z "$SUBMISSION_ID" ]; then
  echo "  ⚠ 库里没有 queued 草稿可测；请先在小程序提交一份作业，或显式传 submission_id"
  echo "== 通过 ${PASS} / 失败 ${FAIL} =="
  exit 1
fi
ok "被测提交 submission_id=${SUBMISSION_ID}"

before_status() { sql "SELECT status FROM calligraphy_review_draft WHERE submission_id=${SUBMISSION_ID} ORDER BY id DESC LIMIT 1"; }
before_err() { sql "SELECT coalesce(error_code,'-') FROM calligraphy_review_draft WHERE submission_id=${SUBMISSION_ID} ORDER BY id DESC LIMIT 1"; }

S0="$(before_status)"
ok "触发前状态：${S0:-<none>}"
[ "$S0" = "queued" ] || info "注意：触发前不是 queued（是 ${S0:-null}），本次不构成「离开 queued」的证明"

# ---- 4. 手动触发（业务错误走 HTTP 200 + 信封 code，故断言 code 而非 HTTP 状态） ----
BODY="$(curl -s -m 20 -X POST "${BASE}/api/v1/moya/submissions/${SUBMISSION_ID}/ai-draft" \
  -H "Authorization: Bearer ${TOKEN}" -H 'Content-Type: application/json' -d '{}')"
CODE="$(printf '%s' "$BODY" | python3 -c "import sys,json;print(json.load(sys.stdin).get('code'))" 2>/dev/null)"
if [ "$CODE" = "0" ]; then
  ok "POST :id/ai-draft 受理：$(printf '%s' "$BODY" | head -c 200)"
else
  bad "POST :id/ai-draft 失败：$(printf '%s' "$BODY" | head -c 200)"
fi

# ---- 5. 等终态（进程内 elib_async → run_draft；本地通常秒级） ----
AFTER=""
for _ in $(seq 1 20); do
  AFTER="$(before_status)"
  if [ "$AFTER" != "queued" ] && [ -n "$AFTER" ]; then
    break
  fi
  sleep 0.5
done

echo "  ── 状态迁移 ──"
info "submission_id=${SUBMISSION_ID}"
info "before : ${S0:-<none>}  (error_code=$(before_err))"
AFTER_ERR="$(before_err)"
info "after  : ${AFTER:-<none>}  (error_code=${AFTER_ERR})"

if [ "$AFTER" = "queued" ]; then
  bad "草稿仍停在 queued —— 手动触发没有把它跑起来（run_draft 未执行 / claim 未抢到）"
else
  ok "草稿已离开 queued → ${AFTER}（证明手动触发不依赖 ecron）"
fi

# ---- 6. 终态可解释性：本地未配 provider 时必须是 provider_unavailable ----
if [ "$AFTER" = "failed" ]; then
  # provider 是否配置，现场从配置文件读（只读，不打印 key）
  PROV="$(sed -n 's/.*{teaching_ai_llm_provider,[[:space:]]*<<"\([^"]*\)">>}.*/\1/p' "${IMBOY_ROOT}/config/sys.local.config" | head -1)"
  if [ -z "$PROV" ]; then
    if [ "$AFTER_ERR" = "provider_unavailable" ]; then
      ok "provider 未配置 ⇒ 落 failed/provider_unavailable，与预期一致（是「没开通」，不是「坏了」）"
      info "老师端会显示：「AI 服务还没开通，先手写吧，写完照常发布。」——诚实且可操作"
    else
      bad "provider 未配置，但落的是 error_code=${AFTER_ERR}（应为 provider_unavailable，别让排查的人猜）"
    fi
  else
    info "provider 已配置为 <<${PROV}>>，failed 时应看 error_code=${AFTER_ERR} 是否指向真实故障"
  fi
fi

echo "== 通过 ${PASS} / 失败 ${FAIL} =="
[ "$FAIL" -eq 0 ]
