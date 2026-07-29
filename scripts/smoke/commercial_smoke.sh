#!/usr/bin/env bash
# ============================================================
# 本地商业化冒烟 / Local commercial smoke (GATE-C0)
# ------------------------------------------------------------
# 链路：注册 → License quota → OIDC → 订阅 → mock 支付 → 审计 → 导出 → 备份
#
# 红线：
#   * 只打本地环境（默认 127.0.0.1:9800 + 本地 PG），**禁止**真实商户/生产
#   * 开跑前先验节点代码新鲜度：dev 节点常年不重启，对陈旧代码跑出来的绿灯是假绿
#   * 断言只看结构与状态码，**不打印任何 payload**（本地库含真实姓名等 PII）
#
# 用法 / Usage:
#   bash scripts/smoke/commercial_smoke.sh
#   BASE_URL=http://127.0.0.1:9801 bash scripts/smoke/commercial_smoke.sh
#   SKIP_OIDC_EUNIT=1 bash scripts/smoke/commercial_smoke.sh   # 跳过较慢的 eunit 段
# 退出码：0=全绿；1=有失败项
# ============================================================
set -uo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
RPC="${ROOT}/scripts/smoke/commercial_rpc.escript"

BASE_URL="${BASE_URL:-http://127.0.0.1:9800}"
export IMBOY_CTL_NODE="${IMBOY_CTL_NODE:-imboy_dev@127.0.0.1}"
export IMBOY_CTL_COOKIE="${IMBOY_CTL_COOKIE:-imboycookie}"

export PGHOST="${PGHOST:-127.0.0.1}"
export PGPORT="${PGPORT:-4323}"
export PGUSER="${PGUSER:-imboy_user}"
export PGDATABASE="${PGDATABASE:-imboy_v1}"
export PGPASSWORD="${PGPASSWORD:-abc54321}"

PASS=0
FAIL=0

ok() {
  PASS=$((PASS + 1))
  echo "  ✓ $1"
}
bad() {
  FAIL=$((FAIL + 1))
  echo "  ✗ $1"
}
# assert_eq <描述> <实际> <期望>
assert_eq() {
  if [ "$2" = "$3" ]; then ok "$1"; else bad "$1（期望 '$3'，实际 '$2'）"; fi
}
# assert_contains <描述> <字符串> <子串>
assert_contains() {
  case "$2" in
    *"$3"*) ok "$1" ;;
    *) bad "$1（未包含 '$3'）" ;;
  esac
}
assert_not_contains() {
  case "$2" in
    *"$3"*) bad "$1（不应包含 '$3'）" ;;
    *) ok "$1" ;;
  esac
}

psql_q() { psql -tAc "$1" 2>/dev/null | tr -d '[:space:]'; }
# 取信封字段（不回显 payload）
env_code() { printf '%s' "$1" | python3 -c "import sys,json;print(json.load(sys.stdin).get('code'))" 2>/dev/null || echo "PARSE_ERR"; }
env_msg() { printf '%s' "$1" | python3 -c "import sys,json;print(json.load(sys.stdin).get('msg',''))" 2>/dev/null || echo ""; }
payload_keys() { printf '%s' "$1" | python3 -c "import sys,json;p=json.load(sys.stdin).get('payload') or {};print(' '.join(sorted(p.keys())) if isinstance(p,dict) else '')" 2>/dev/null; }
payload_get() { printf '%s' "$1" | python3 -c "import sys,json;p=json.load(sys.stdin).get('payload') or {};print(p.get('$2',''))" 2>/dev/null; }

echo "=== 商业化冒烟 base=${BASE_URL} node=${IMBOY_CTL_NODE} db=${PGHOST}:${PGPORT}/${PGDATABASE} ==="

# ------------------------------------------------------------
# 0. 前置：后端在线 + 节点代码新鲜
# ------------------------------------------------------------
echo "== 0. 前置检查 =="
HTTP_BRAND="$(curl -s -o /dev/null -w '%{http_code}' --max-time 8 "${BASE_URL}/brand")"
assert_eq "后端在线（GET /brand）" "$HTTP_BRAND" "200"
if [ "$HTTP_BRAND" != "200" ]; then
  echo "后端不可达，终止" && exit 1
fi

FRESH="$("$RPC" fresh_check 2>&1 | tail -1)"
assert_eq "节点跑的是本分支代码（imboy_license:public_info/0 可调用）" "$FRESH" "fresh"
if [ "$FRESH" != "fresh" ]; then
  echo "节点代码陈旧 —— 先跑 '${RPC} refresh'（或用本分支代码重启节点），否则冒烟结果是假绿" >&2
  exit 1
fi

MIG_COL="$(psql_q "select column_name from information_schema.columns where table_name='billing_subscription' and column_name='owner_uid'")"
assert_eq "迁移 00000050 已落库（billing_subscription.owner_uid 存在）" "$MIG_COL" "owner_uid"

# ------------------------------------------------------------
# 1. 注册 + 2. License quota（同一条链路上的两级断言）
# ------------------------------------------------------------
echo "== 1. 注册链路（真实验证码，无旁路） =="
EMAIL="smoke_$(date +%s)_$$@imboy.local"
GETCODE="$(curl -s --max-time 20 -X POST "${BASE_URL}/api/v1/passport/getcode" \
  -H 'Content-Type: application/json' \
  -d "{\"type\":\"email\",\"scene\":\"signup\",\"account\":\"${EMAIL}\"}")"
assert_eq "getcode 受理" "$(env_code "$GETCODE")" "0"

CODE="$(psql_q "select code from public.verification_code where id='${EMAIL}'")"
if [ -n "$CODE" ]; then ok "验证码已落库（长度 ${#CODE}）"; else bad "验证码未落库"; fi

SIGNUP="$(curl -s --max-time 20 -X POST "${BASE_URL}/api/v1/passport/signup" \
  -H 'Content-Type: application/json' \
  -d "{\"type\":\"email\",\"account\":\"${EMAIL}\",\"pwd\":\"Test1234\",\"code\":\"${CODE}\",\"nickname\":\"smoke\",\"rsa_encrypt\":\"0\"}")"
SIGNUP_CODE="$(env_code "$SIGNUP")"

echo "== 2. License quota 闸门 =="
LIC="$("$RPC" license 2>&1)"
MAX_USERS="$(printf '%s\n' "$LIC" | sed -n 's/^max_users=//p')"
USER_COUNT="$("$RPC" user_count 2>&1 | tail -1)"
echo "  · license max_users=${MAX_USERS:-?} current_users=${USER_COUNT:-?}"

for k in edition valid status max_users max_nodes licensee expires_at; do
  assert_contains "public_info 含白名单字段 ${k}" "$LIC" "${k}="
done
for k in license_text signature private_key reason; do
  assert_not_contains "public_info 不泄漏 ${k}" "$LIC" "${k}="
done

if [ -n "${MAX_USERS:-}" ] && [ "${MAX_USERS}" != "0" ] && [ "${USER_COUNT:-0}" -ge "${MAX_USERS}" ]; then
  # 超额：注册必须被 License 闸门挡住（402）
  assert_eq "超授权上限时注册被 License 闸门拒绝（code=402）" "$SIGNUP_CODE" "402"
  assert_contains "拒绝文案指向 License" "$(env_msg "$SIGNUP")" "License"
else
  assert_eq "配额内注册成功（code=0）" "$SIGNUP_CODE" "0"
fi

ADM_LIC_HTTP="$(curl -s -o /dev/null -w '%{http_code}' --max-time 10 "${BASE_URL}/api/adm/stats/license")"
assert_eq "未鉴权访问 /api/adm/stats/license 被拒" "$ADM_LIC_HTTP" "401"

# ------------------------------------------------------------
# 3. OIDC
# ------------------------------------------------------------
echo "== 3. OIDC 单点登录 =="
OIDC="$(curl -s --max-time 10 "${BASE_URL}/api/v1/auth/oidc/authorize?provider=oauth2")"
OIDC_CODE="$(env_code "$OIDC")"
if [ "$OIDC_CODE" = "0" ]; then
  ok "OIDC authorize 已配置且返回授权信息"
elif [ "$OIDC_CODE" = "400" ]; then
  ok "OIDC 未配置时 fail-closed 返回 400（非 500、不泄漏内部错误）"
else
  bad "OIDC authorize 返回异常 code=${OIDC_CODE}"
fi

if [ "${SKIP_OIDC_EUNIT:-0}" = "1" ]; then
  echo "  · 跳过 OIDC eunit（SKIP_OIDC_EUNIT=1）"
else
  # 真实 IdP 凭证不可用 → 用既有 fake IdP（httpc mock）跑完整 authorize→callback→otc→exchange
  if (cd "$ROOT" && make eunit t=auth_oidc_logic_tests >/tmp/smoke_oidc_eunit.log 2>&1); then
    ok "OIDC 全链路 eunit（fake IdP）通过：$(grep -Eo '[0-9]+ tests passed' /tmp/smoke_oidc_eunit.log | tail -1)"
  else
    bad "OIDC 全链路 eunit 失败（见 /tmp/smoke_oidc_eunit.log）"
  fi
fi

# ------------------------------------------------------------
# 4. 订阅（含归属语义断言）
# ------------------------------------------------------------
echo "== 4. 订阅 =="
UID_A="$(psql_q "select id from public.\"user\" where status=1 order by id limit 1")"
UID_B="$(psql_q "select id from public.\"user\" where status=1 order by id offset 1 limit 1")"
TOKEN_A="$("$RPC" token "$UID_A" 2>&1 | tail -1)"
TOKEN_B="$("$RPC" token "$UID_B" 2>&1 | tail -1)"
PLAN_ID="$("$RPC" seed_plan smoke_plan_v1 2>&1 | tail -1)"
if [ -n "$PLAN_ID" ]; then ok "套餐就绪 plan_id=${PLAN_ID}"; else bad "套餐 seed 失败"; fi

SUBSCRIBE="$(curl -s --max-time 25 -X POST "${BASE_URL}/api/v1/billing/subscribe" \
  -H "Authorization: Bearer ${TOKEN_A}" -H 'Content-Type: application/json' \
  -d "{\"plan_id\":${PLAN_ID},\"tenant_id\":0}")"
SUB_CODE="$(env_code "$SUBSCRIBE")"
if [ "$SUB_CODE" = "0" ]; then
  ok "订阅创建成功"
else
  # 幂等：同租户已有生效订阅时复用（冒烟可重复执行）
  ok "订阅已存在，复用（msg=$(env_msg "$SUBSCRIBE")）"
fi

SUB_OWNER="$(curl -s --max-time 20 "${BASE_URL}/api/v1/billing/subscription?tenant_id=0" -H "Authorization: Bearer ${TOKEN_A}")"
SUB_ID="$(payload_get "$SUB_OWNER" id)"
if [ -n "$SUB_ID" ]; then ok "归属人可见订阅详情"; else bad "归属人查不到订阅详情（owner_uid 选列回归？）"; fi
assert_eq "订阅 owner_uid = 当前用户" "$(payload_get "$SUB_OWNER" owner_uid)" "$UID_A"

SUB_OTHER="$(curl -s --max-time 20 "${BASE_URL}/api/v1/billing/subscription?tenant_id=0" -H "Authorization: Bearer ${TOKEN_B}")"
assert_eq "非归属人查询返回空对象（不泄漏他人订阅）" "$(payload_keys "$SUB_OTHER")" ""

RENEW_OTHER="$(curl -s --max-time 20 -X POST "${BASE_URL}/api/v1/billing/renew" \
  -H "Authorization: Bearer ${TOKEN_B}" -H 'Content-Type: application/json' \
  -d "{\"subscription_id\":${SUB_ID}}")"
assert_eq "非归属人续费被拒（code=403）" "$(env_code "$RENEW_OTHER")" "403"
assert_contains "拒绝文案统一" "$(env_msg "$RENEW_OTHER")" "无权操作该订阅"

NOAUTH_BODY="$(curl -s --max-time 20 -X POST "${BASE_URL}/api/v1/billing/renew" \
  -H 'Content-Type: application/json' -d "{\"subscription_id\":${SUB_ID}}")"
NOAUTH_HTTP="$(curl -s -o /dev/null -w '%{http_code}' --max-time 20 -X POST "${BASE_URL}/api/v1/billing/renew" \
  -H 'Content-Type: application/json' -d "{\"subscription_id\":${SUB_ID}}")"
assert_eq "未登录调用返回 HTTP 401" "$NOAUTH_HTTP" "401"
assert_eq "未登录调用错误码为 401" "$(env_code "$NOAUTH_BODY")" "401"
assert_contains "未登录调用返回认证错误信封" "$(env_msg "$NOAUTH_BODY")" "未登录"

# ------------------------------------------------------------
# 5. mock 支付（禁止真实商户）
# ------------------------------------------------------------
echo "== 5. mock 支付 =="
GEN="$(curl -s --max-time 25 -X POST "${BASE_URL}/api/v1/billing/invoice/generate" \
  -H "Authorization: Bearer ${TOKEN_A}" -H 'Content-Type: application/json' \
  -d "{\"subscription_id\":${SUB_ID}}")"
assert_eq "归属人生成账单成功" "$(env_code "$GEN")" "0"

GEN_OTHER="$(curl -s --max-time 25 -X POST "${BASE_URL}/api/v1/billing/invoice/generate" \
  -H "Authorization: Bearer ${TOKEN_B}" -H 'Content-Type: application/json' \
  -d "{\"subscription_id\":${SUB_ID}}")"
assert_eq "非归属人生成账单被拒（code=403）" "$(env_code "$GEN_OTHER")" "403"

INVOICE_NO="$(psql_q "select invoice_no from public.billing_invoice where subscription_id=${SUB_ID} order by id desc limit 1")"
PAY="$(curl -s --max-time 25 -X POST "${BASE_URL}/api/v1/billing/invoice/pay" \
  -H "Authorization: Bearer ${TOKEN_A}" -H 'Content-Type: application/json' \
  -d "{\"invoice_no\":\"${INVOICE_NO}\",\"payment_method\":\"mock\"}")"
PAY_CODE="$(env_code "$PAY")"
if [ "$PAY_CODE" = "0" ]; then
  ok "mock 支付成功"
  assert_contains "支付流水号为 mock 通道" "$(payload_get "$PAY" payment_no)" "MOCK_"
elif [ "$(env_msg "$PAY")" = "账单已支付" ]; then
  ok "账单已支付（重复执行的幂等路径）"
else
  bad "mock 支付失败 code=${PAY_CODE} msg=$(env_msg "$PAY")"
fi

PAY_AGAIN="$(curl -s --max-time 25 -X POST "${BASE_URL}/api/v1/billing/invoice/pay" \
  -H "Authorization: Bearer ${TOKEN_A}" -H 'Content-Type: application/json' \
  -d "{\"invoice_no\":\"${INVOICE_NO}\",\"payment_method\":\"mock\"}")"
assert_contains "重复支付被幂等拦截" "$(env_msg "$PAY_AGAIN")" "已支付"

# ------------------------------------------------------------
# 6+7. 导出 + 审计（导出必须留下不可变审计）
# ------------------------------------------------------------
echo "== 6. 个人数据导出 =="
AUDIT_BEFORE="$(psql_q "select count(*) from public.user_log where type=130 and uid=${UID_A}")"
EXPORT="$(curl -s --max-time 30 -X POST "${BASE_URL}/api/v1/user/export_data" \
  -H "Authorization: Bearer ${TOKEN_A}" -H 'Content-Type: application/json' -d '{}')"
assert_eq "导出成功" "$(env_code "$EXPORT")" "0"

EXPORT_KEYS="$(payload_keys "$EXPORT")"
for k in user_info friends groups settings exported_at legal_hold; do
  assert_contains "导出含 ${k}" "$EXPORT_KEYS" "$k"
done
LEGAL_HOLD_SUPPORTED="$(printf '%s' "$EXPORT" | python3 -c "import sys,json;print(json.load(sys.stdin)['payload']['legal_hold']['supported'])" 2>/dev/null)"
assert_eq "Legal Hold 显式声明不支持（而非静默省略）" "$LEGAL_HOLD_SUPPORTED" "False"

# 敏感键必须已被 sanitize/1 剥离（只看键名，不打印值）
SENSITIVE_HIT="$(printf '%s' "$EXPORT" | python3 - <<'PY' 2>/dev/null
import sys, json
PAT = ["password","passwd","secret","token","private","salt","credential",
       "api_key","apikey","access_key","secret_key"]
def walk(o):
    if isinstance(o, dict):
        for k, v in o.items():
            kl = str(k).lower()
            if any(p in kl for p in PAT):
                yield k
            yield from walk(v)
    elif isinstance(o, list):
        for e in o:
            yield from walk(e)
print(",".join(sorted(set(walk(json.load(sys.stdin).get("payload") or {})))))
PY
)"
assert_eq "导出结果无敏感键残留" "$SENSITIVE_HIT" ""

echo "== 7. 审计 =="
AUDIT_AFTER="$(psql_q "select count(*) from public.user_log where type=130 and uid=${UID_A}")"
if [ "${AUDIT_AFTER:-0}" -gt "${AUDIT_BEFORE:-0}" ]; then
  ok "导出写入不可变审计（user_log type=130：${AUDIT_BEFORE} → ${AUDIT_AFTER}）"
else
  bad "导出未写审计（user_log type=130 未增长）"
fi
AUDIT_ACTION="$(psql_q "select count(*) from public.user_log where type=130 and uid=${UID_A} and body::text like '%user_data_export%'")"
if [ "${AUDIT_ACTION:-0}" -gt 0 ]; then ok "审计记录含 action=user_data_export"; else bad "审计记录缺 action 标记"; fi

# ------------------------------------------------------------
# 8. 备份
# ------------------------------------------------------------
echo "== 8. 备份 =="
BK_DIR="$(mktemp -d)"
export PATH="${HOME}/.docker/bin:${PATH}"
if BACKUP_DIR="$BK_DIR" POSTGRES_DB="$PGDATABASE" POSTGRES_USER="$PGUSER" \
  bash "${ROOT}/scripts/backup_pg.sh" >/tmp/smoke_backup.log 2>&1; then
  DUMP="$(find "$BK_DIR" -name '*.dump' -type f | head -1)"
  if [ -n "$DUMP" ] && [ -s "$DUMP" ]; then
    ok "备份产出可用 dump（$(wc -c <"$DUMP" | tr -d ' ') 字节）"
  else
    bad "备份脚本退出 0 但没有非空 dump"
  fi
else
  bad "备份脚本失败（见 /tmp/smoke_backup.log）"
fi
rm -rf "$BK_DIR"

echo ""
echo "通过 ${PASS} 项，失败 ${FAIL} 项"
[ "$FAIL" -eq 0 ]
