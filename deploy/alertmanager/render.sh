#!/usr/bin/env bash
# ============================================================
# 渲染 Alertmanager 配置 / Render Alertmanager config
# ------------------------------------------------------------
#   alertmanager.yml.template + alertmanager.env → alertmanager.yml
#
# 用法 / Usage:
#   bash deploy/alertmanager/render.sh
#   DRY_RUN=1 bash deploy/alertmanager/render.sh   # 只校验不落盘
#
# 为什么要有校验：B-25 的缺陷不是"没写通知配置"，是**写了但全是 null，
# 而没有任何东西会因此报错** —— 部署看起来一切正常，直到真出故障时才发现
# 没有人被通知到。这个脚本负责把"没配"这件事变成刺眼的。
# ============================================================
set -euo pipefail

cd "$(dirname "$0")"

TEMPLATE="alertmanager.yml.template"
ENV_FILE="${ALERT_ENV_FILE:-alertmanager.env}"
OUT="${ALERT_OUT_FILE:-alertmanager.yml}"
DRY_RUN="${DRY_RUN:-0}"

RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[1;33m'; NC='\033[0m'
info() { echo -e "${GREEN}[alertmanager]${NC} $*"; }
warn() { echo -e "${YELLOW}[alertmanager]${NC} $*" >&2; }
fail() { echo -e "${RED}[alertmanager] ERROR:${NC} $*" >&2; exit 1; }

[ -f "$TEMPLATE" ] || fail "模板不存在: $TEMPLATE"
command -v envsubst >/dev/null 2>&1 \
  || fail "envsubst 未安装（Debian/Ubuntu: apt-get install -y gettext-base）"

if [ -f "$ENV_FILE" ]; then
  # ⚠️ 不能写成 `. "./$ENV_FILE"`：ENV_FILE 是绝对路径时会拼成 .//abs/path
  #   （等价于相对路径 ./abs/path）而找不到文件 —— 测试里实测踩到。
  #   `.` 只在参数不含斜杠时才查 PATH，所以补斜杠即可，不要硬加 ./
  case "$ENV_FILE" in
    */*) SRC_PATH="$ENV_FILE" ;;
    *)   SRC_PATH="./$ENV_FILE" ;;
  esac
  # shellcheck disable=SC1090
  set -a; . "$SRC_PATH"; set +a
  info "已加载 $ENV_FILE"
else
  warn "$ENV_FILE 不存在，全部取空值 —— 渲染结果将是「只进 UI，无人被通知」。
  正确做法: cp alertmanager.env.example alertmanager.env && \$EDITOR alertmanager.env"
fi

# 未设置的变量一律给空串，避免 envsubst 把 ${VAR} 原样留在 yml 里
# （原样留下会让 Alertmanager 启动时报难懂的解析错，不如空值 + 下面的显式校验）
: "${ALERT_RECEIVER_DEFAULT:=null}"
: "${ALERT_RECEIVER_CRITICAL:=null}"
: "${ALERT_SMTP_SMARTHOST:=}"
: "${ALERT_SMTP_FROM:=}"
: "${ALERT_SMTP_USERNAME:=}"
: "${ALERT_SMTP_PASSWORD:=}"
: "${ALERT_SMTP_REQUIRE_TLS:=true}"
: "${ALERT_EMAIL_TO:=}"
: "${ALERT_WEBHOOK_URL:=}"
export ALERT_RECEIVER_DEFAULT ALERT_RECEIVER_CRITICAL \
       ALERT_SMTP_SMARTHOST ALERT_SMTP_FROM ALERT_SMTP_USERNAME ALERT_SMTP_PASSWORD \
       ALERT_SMTP_REQUIRE_TLS ALERT_EMAIL_TO ALERT_WEBHOOK_URL

# ---------- 一致性校验：选了渠道就必须把它需要的值填齐 ----------
# 选了 email 却没填 smarthost，Alertmanager 会启动成功但**静默发不出邮件**，
# 这比不配还危险——你以为配好了。
check_receiver() {
  local which="$1" name="$2"
  case "$name" in
    null) return 0 ;;
    email)
      [ -n "$ALERT_SMTP_SMARTHOST" ] || fail "${which} 选了 email 但 ALERT_SMTP_SMARTHOST 为空"
      [ -n "$ALERT_SMTP_FROM" ]      || fail "${which} 选了 email 但 ALERT_SMTP_FROM 为空"
      [ -n "$ALERT_EMAIL_TO" ]       || fail "${which} 选了 email 但 ALERT_EMAIL_TO 为空"
      ;;
    webhook)
      [ -n "$ALERT_WEBHOOK_URL" ] || fail "${which} 选了 webhook 但 ALERT_WEBHOOK_URL 为空"
      ;;
    *)
      fail "${which} 取值非法: '${name}'（可选 null | email | webhook）"
      ;;
  esac
}
check_receiver "ALERT_RECEIVER_DEFAULT"  "$ALERT_RECEIVER_DEFAULT"
check_receiver "ALERT_RECEIVER_CRITICAL" "$ALERT_RECEIVER_CRITICAL"

# ---------- 渲染 ----------
# 只保留**真的被某条 route 指向**的 receiver。
# 为什么必须这么做：Alertmanager 对 `url: ''` / `to: ''` 这种空值 receiver 会
# 拒绝加载整份配置 —— 于是"没填渠道"会从「只进 UI」恶化成「Alertmanager 起不来」，
# 比修之前更糟。没配就整段不输出。
keep_receiver() {  # keep_receiver <name> -> 0 保留 / 1 删除
  [ "$ALERT_RECEIVER_DEFAULT" = "$1" ] || [ "$ALERT_RECEIVER_CRITICAL" = "$1" ]
}

# 按 #>>>RECEIVER:x / #<<<RECEIVER:x 标记整段保留或删除；标记行本身始终去掉
strip_unused_receivers() {
  local keep_email=0 keep_webhook=0
  keep_receiver email   && keep_email=1
  keep_receiver webhook && keep_webhook=1
  awk -v ke="$keep_email" -v kw="$keep_webhook" '
    /^#>>>RECEIVER:email$/   { blk="email";   next }
    /^#<<<RECEIVER:email$/   { blk="";        next }
    /^#>>>RECEIVER:webhook$/ { blk="webhook"; next }
    /^#<<<RECEIVER:webhook$/ { blk="";        next }
    blk == "email"   && ke != 1 { next }
    blk == "webhook" && kw != 1 { next }
    { print }
  '
}

RENDERED="$(envsubst < "$TEMPLATE" | strip_unused_receivers)"

if [ "$DRY_RUN" = "1" ]; then
  info "DRY_RUN=1：校验通过，未落盘"
else
  printf '%s\n' "$RENDERED" > "$OUT"
  info "已渲染 → $OUT"
fi

# ---------- 最后的刺眼提醒 ----------
# 放在渲染**之后**：即使只是想先跑起来，也要让人看见这条。
if [ "$ALERT_RECEIVER_CRITICAL" = "null" ]; then
  warn "⚠️  ALERT_RECEIVER_CRITICAL=null：**critical 告警不会通知任何人，只进 UI**。
  Sellable 判据第 6 条「故障有人被通知到（不是只进 UI）」在此状态下为假。
  上线前必须配成 email 或 webhook，并真停一次 PG 验证 5 分钟内能收到。"
else
  info "critical 告警渠道: ${ALERT_RECEIVER_CRITICAL}"
fi
