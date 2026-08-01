#!/usr/bin/env bash
# ============================================================
# Alertmanager 渲染与校验测试 / Alertmanager render + validation tests
# ------------------------------------------------------------
# 覆盖 B-25。不启动 Alertmanager、不发任何通知。
# 用法: bash scripts/test/alertmanager_render_test.sh
# ============================================================
set -uo pipefail
cd "$(dirname "$0")/../.."

PASS=0; FAIL=0
RED='\033[0;31m'; GREEN='\033[0;32m'; NC='\033[0m'
ok()  { echo -e "${GREEN}  PASS${NC} $1"; PASS=$((PASS+1)); }
bad() { echo -e "${RED}  FAIL${NC} $1"; echo "    ${2:-<空>}"; FAIL=$((FAIL+1)); }

WORK="$(mktemp -d)"; trap 'rm -rf "$WORK"' EXIT
R="deploy/alertmanager/render.sh"

# 用给定 env 内容跑渲染，回显退出码与输出
render() {  # render <env内容>
  printf '%s\n' "$1" > "$WORK/am.env"
  ALERT_ENV_FILE="$WORK/am.env" ALERT_OUT_FILE="$WORK/out.yml" \
    bash "$R" >"$WORK/log" 2>&1
  echo $?
}

echo "== B-25 Alertmanager 渠道配置 =="

# 1) 全空（默认态）→ 渲染成功，但必须刺眼提醒「无人被通知」
RC="$(render "ALERT_RECEIVER_CRITICAL=null")"
if [ "$RC" = "0" ] && grep -q "不会通知任何人" "$WORK/log"; then
  ok "未配置时渲染成功但大声警告"
else
  bad "未配置时未给出警告" "rc=$RC $(cat "$WORK/log")"
fi

# 2) 选 email 却缺 smarthost → 必须失败。
#    这是最危险的半配状态：Alertmanager 会启动成功但**静默发不出邮件**，
#    比不配还糟——你以为配好了。
RC="$(render "ALERT_RECEIVER_CRITICAL=email
ALERT_EMAIL_TO=a@example.com")"
if [ "$RC" != "0" ] && grep -q "ALERT_SMTP_SMARTHOST 为空" "$WORK/log"; then
  ok "选 email 但缺 smarthost 时失败"
else
  bad "半配的 email 未被拦住" "rc=$RC $(cat "$WORK/log")"
fi

# 3) 选 email 却缺收件人 → 失败
RC="$(render "ALERT_RECEIVER_CRITICAL=email
ALERT_SMTP_SMARTHOST=smtp.example.com:587
ALERT_SMTP_FROM=am@example.com")"
if [ "$RC" != "0" ] && grep -q "ALERT_EMAIL_TO 为空" "$WORK/log"; then
  ok "选 email 但缺收件人时失败"
else
  bad "缺收件人未被拦住" "rc=$RC $(cat "$WORK/log")"
fi

# 4) 选 webhook 却缺 URL → 失败
RC="$(render "ALERT_RECEIVER_CRITICAL=webhook")"
if [ "$RC" != "0" ] && grep -q "ALERT_WEBHOOK_URL 为空" "$WORK/log"; then
  ok "选 webhook 但缺 URL 时失败"
else
  bad "缺 webhook URL 未被拦住" "rc=$RC $(cat "$WORK/log")"
fi

# 5) 非法渠道名 → 失败（写错成 dingtalk 之类时立刻发现，而不是启动后才报错）
RC="$(render "ALERT_RECEIVER_CRITICAL=dingtalk")"
if [ "$RC" != "0" ] && grep -q "取值非法" "$WORK/log"; then
  ok "非法渠道名被拒绝"
else
  bad "非法渠道名未被拒绝" "rc=$RC $(cat "$WORK/log")"
fi

# 6) 配齐 webhook → 渲染成功，且值真的落进产物、critical 分支指向它
RC="$(render "ALERT_RECEIVER_CRITICAL=webhook
ALERT_WEBHOOK_URL=http://hook.internal:8060/send")"
if [ "$RC" = "0" ] && grep -q "http://hook.internal:8060/send" "$WORK/out.yml"; then
  ok "配齐后 webhook URL 落进产物"
else
  bad "配齐后未渲染出 URL" "rc=$RC $(cat "$WORK/log")"
fi
if grep -A3 "severity: critical" "$WORK/out.yml" | grep -q "receiver: 'webhook'"; then
  ok "critical 分支指向已配置的渠道"
else
  bad "critical 分支未指向配置的渠道" "$(grep -A4 'severity: critical' "$WORK/out.yml")"
fi

# 7) 产物里不得残留未展开的 ${VAR}
# ⚠️ 先断言产物存在：文件不存在时 grep 找不到东西，这条会变成**空断言**恒绿
#    （第一版就是这样，渲染全失败它还是 PASS）。
if [ ! -f "$WORK/out.yml" ]; then
  bad "产物不存在，无法检查占位符" ""
elif grep -q '\${' "$WORK/out.yml"; then
  bad "产物残留未展开的占位符" "$(grep -n '\${' "$WORK/out.yml" | head -3)"
else
  ok "产物无残留占位符"
fi

# 8) 仓库里**不得**出现真实联系方式：模板与样例只能有 example.com 之类
if grep -rnE '@(gmail|qq|163|outlook)\.com|oapi\.dingtalk\.com|open\.feishu\.cn' \
     deploy/alertmanager/alertmanager.yml.template deploy/alertmanager/alertmanager.env.example >/dev/null 2>&1; then
  bad "模板/样例里出现了疑似真实联系方式" \
      "$(grep -rnE '@(gmail|qq|163|outlook)\.com|oapi\.dingtalk\.com|open\.feishu\.cn' deploy/alertmanager/ | head -3)"
else
  ok "模板/样例无真实联系方式"
fi

echo
echo "总计: PASS=${PASS} FAIL=${FAIL}"
[ "$FAIL" -eq 0 ]
