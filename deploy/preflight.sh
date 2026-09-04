#!/usr/bin/env bash
# IMBoy 生产部署前置检查 / Pre-flight check for production deployment
#
# Usage:
#   bash deploy/preflight.sh                                  # 检查 .env 和系统资源
#   bash deploy/preflight.sh --docker                         # 同上，附加检查 Docker 环境
#   bash deploy/preflight.sh --edition community [--docker]   # 社区版口径：garage 凭据
#                                                              # 为硬校验（内置核心服务），
#                                                              # 跳过销售发布支付强校验
#   bash deploy/preflight.sh --edition business [--docker]    # 商务版口径（默认，同旧版行为）
#
# Exit codes:
#   0 = all checks passed
#   1 = one or more ERROR checks failed

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ENV_FILE="${SCRIPT_DIR}/.env"
ERRORS=0
WARNINGS=0

# ── 参数解析 ──────────────────────────────────────────────────────────────────
# EDITION 默认 business：preflight 单独运行时维持既有检查口径不变（garage WARN、
# 销售发布支付强校验）。install.sh 会按部署版本显式传入 --edition。
DOCKER_CHECK=0
EDITION="business"
while [[ $# -gt 0 ]]; do
    case "$1" in
        --docker) DOCKER_CHECK=1 ;;
        --edition)
            [[ $# -ge 2 ]] || { echo "错误：--edition 需要值 community|business" >&2; exit 1; }
            shift
            EDITION="$1"
            ;;
        --edition=*) EDITION="${1#*=}" ;;
        *)
            echo "未知参数：$1（支持 --docker 与 --edition community|business）" >&2
            exit 1
            ;;
    esac
    shift
done
case "$EDITION" in
    community|business) ;;
    *)
        echo "错误：--edition 仅支持 community|business（当前: ${EDITION}）" >&2
        exit 1
        ;;
esac

RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m'

ok()   { echo -e "  ${GREEN}[OK]${NC}    $*"; }
warn() { echo -e "  ${YELLOW}[WARN]${NC}  $*"; WARNINGS=$((WARNINGS+1)); }
err()  { echo -e "  ${RED}[ERROR]${NC} $*"; ERRORS=$((ERRORS+1)); }
info() { echo -e "  ${BLUE}[INFO]${NC}  $*"; }

echo ""
echo "=========================================="
echo " IMBoy Production Pre-flight Check"
echo "=========================================="
echo ""

# ── 1. .env 文件存在 ─────────────────────────────────────────────────────────
echo "▶ 1. 检查 .env 配置文件 / Checking .env file"

if [[ ! -f "$ENV_FILE" ]]; then
    err ".env 文件不存在：请先 cp .env.example .env 并填入配置"
    echo ""
    echo "❌ 无法继续：缺少 .env 文件"
    exit 1
fi
ok ".env 文件存在"

# 加载 .env（跳过注释和空行）
set +u
while IFS='=' read -r key value; do
    [[ "$key" =~ ^#.*$ ]] && continue
    [[ -z "$key" ]] && continue
    export "$key"="${value}"
done < "$ENV_FILE"
set -u

# ── 2. 必填变量未留占位符 ──────────────────────────────────────────────────────
echo ""
echo "▶ 2. 检查必填变量 / Checking required variables"

check_var() {
    local var_name="$1"
    local val="${!var_name:-}"
    if [[ -z "$val" ]]; then
        err "$var_name 未设置"
    elif [[ "$val" == *"CHANGE_ME"* ]] || [[ "$val" == *"example.com"* && "$var_name" != "SENTRY_DSN" ]]; then
        err "$var_name 仍为占位符值：$val"
    else
        ok "$var_name 已设置"
    fi
}

check_var "API_DOMAIN"
check_var "ADMIN_DOMAIN"
check_var "POSTGRES_USER"
check_var "POSTGRES_PASSWORD"
check_var "POSTGRES_DB"
check_var "JWT_KEY"
check_var "POSTGRE_AES_KEY"
check_var "ADM_COOKIE_SECRET"
check_var "IMBOY_SOLIDIFIED_KEY"
check_var "IMBOY_SOLIDIFIED_KEY_IV"
# password_salt 是 imboy_app:validate_runtime_config/0 的 ensure_required_secret 强制项，
# 但此前 .env.example / preflight / compose 三处都没有它 —— 后端会以
# {missing_required_config, password_salt} 启动失败，且报错只在容器日志里。
check_var "IMBOY_PASSWORD_SALT"
check_var "GRAFANA_ADMIN_PASSWORD"
# LiveKit：compose 中该服务无条件启动。此前用 ${LIVEKIT_API_KEY:-devkey} 兜底，
# 忘配即以公开已知密钥上线 —— 任何人可签发 token 加入或录制任意通话。
check_var "LIVEKIT_API_KEY"
check_var "LIVEKIT_API_SECRET"

is_domain() {
    [[ "$1" =~ ^[A-Za-z0-9]([A-Za-z0-9.-]*[A-Za-z0-9])$ ]] \
        && [[ "$1" == *.* ]] && [[ "$1" != *..* ]]
}
is_email() { [[ "$1" =~ ^[^[:space:]@]+@[^[:space:]@]+\.[^[:space:]@]+$ ]]; }

if ! is_domain "${API_DOMAIN:-}"; then err "API_DOMAIN 不是有效的纯域名（不要带 https:// 或路径）"; fi
if ! is_domain "${ADMIN_DOMAIN:-}"; then err "ADMIN_DOMAIN 不是有效的纯域名（不要带 https:// 或路径）"; fi
if [[ "${API_DOMAIN:-}" == "${ADMIN_DOMAIN:-}" ]]; then err "API_DOMAIN 与 ADMIN_DOMAIN 不能相同"; fi
if ! is_email "${CERTBOT_EMAIL:-}"; then err "CERTBOT_EMAIL 格式无效"; fi

UPTRACE_ENABLED_VALUE="$(echo "${UPTRACE_ENABLED:-false}" | tr '[:upper:]' '[:lower:]' | tr -d '[:space:]')"
case "$UPTRACE_ENABLED_VALUE" in
    true|1)
        for var_name in UPTRACE_DOMAIN UPTRACE_ADMIN_EMAIL UPTRACE_SERVICE_SECRET \
                        UPTRACE_PG_PASSWORD UPTRACE_CLICKHOUSE_PASSWORD UPTRACE_REDIS_PASSWORD \
                        UPTRACE_ADMIN_PASSWORD UPTRACE_PROJECT_TOKEN; do
            check_var "$var_name"
        done
        if ! is_domain "${UPTRACE_DOMAIN:-}"; then err "UPTRACE_DOMAIN 不是有效的纯域名"; fi
        if [[ "${UPTRACE_DOMAIN:-}" == "${API_DOMAIN:-}" || "${UPTRACE_DOMAIN:-}" == "${ADMIN_DOMAIN:-}" ]]; then
            err "UPTRACE_DOMAIN 必须与 API_DOMAIN / ADMIN_DOMAIN 不同"
        fi
        if ! is_email "${UPTRACE_ADMIN_EMAIL:-}"; then err "UPTRACE_ADMIN_EMAIL 格式无效"; fi
        ok "Uptrace 可选栈已启用并配置"
        ;;
    false|0|"") info "Uptrace 未启用" ;;
    *) err "UPTRACE_ENABLED 仅支持 true/1/false/0" ;;
esac

# SMTP 不设单独假开关：客户填写任一核心项即视为启用，并要求整组完整。
if [[ -n "${IMBOY_SMTP_RELAY:-}${IMBOY_SMTP_USERNAME:-}${IMBOY_SMTP_PASSWORD:-}${IMBOY_SMTP_FROM:-}" ]]; then
    check_var "IMBOY_SMTP_RELAY"
    check_var "IMBOY_SMTP_USERNAME"
    check_var "IMBOY_SMTP_PASSWORD"
    SMTP_FROM_VALUE="${IMBOY_SMTP_FROM:-${IMBOY_SMTP_USERNAME:-}}"
    if ! is_email "$SMTP_FROM_VALUE"; then err "IMBOY_SMTP_FROM（或回退的 USERNAME）必须是有效邮箱"; fi
    if ! [[ "${IMBOY_SMTP_PORT:-}" =~ ^[0-9]+$ ]] || (( IMBOY_SMTP_PORT < 1 || IMBOY_SMTP_PORT > 65535 )); then
        err "IMBOY_SMTP_PORT 必须是 1-65535"
    fi
    case "$(echo "${IMBOY_SMTP_SSL:-}" | tr '[:upper:]' '[:lower:]')" in
        true|1|false|0) ok "SMTP TLS 开关有效" ;;
        *) err "IMBOY_SMTP_SSL 仅支持 true/1/false/0" ;;
    esac
else
    info "SMTP 未配置，邮件验证码/通知不可用"
fi

SMS_SWITCH="$(echo "${IMBOY_SMS_SWITCH:-off}" | tr '[:upper:]' '[:lower:]' | tr -d '[:space:]')"
case "$SMS_SWITCH" in
    off) info "短信发送未启用" ;;
    on)
        case "${IMBOY_SMS_PLATFORM:-}" in
            yjsms)
                check_var "IMBOY_YJSMS_ACCOUNT"
                check_var "IMBOY_YJSMS_SECRET"
                check_var "IMBOY_YJSMS_URL"
                [[ "${IMBOY_YJSMS_URL:-}" == https://* ]] || err "IMBOY_YJSMS_URL 必须使用 https://"
                ;;
            jsms)
                check_var "IMBOY_JPUSH_APP_KEY"
                check_var "IMBOY_JPUSH_MASTER_SECRET"
                check_var "IMBOY_JSMS_TEMP_ID"
                check_var "IMBOY_JSMS_SIGN_ID"
                ;;
            aliyun) err "IMBOY_SMS_PLATFORM=aliyun 尚无发送实现；当前仅支持 yjsms 或 jsms" ;;
            *) err "IMBOY_SMS_PLATFORM 必须为 yjsms 或 jsms" ;;
        esac
        ;;
    *) err "IMBOY_SMS_SWITCH 仅支持 on 或 off" ;;
esac

# RSA 密钥文件（检查路径已设置且文件存在）
#
# ⚠️ .env 里配的是**容器内**路径（/opt/imboy/priv_runtime/...），而本脚本跑在
# 宿主机上。直接 `[[ -f "$path" ]]` 检查容器路径在宿主机必然为假 —— 这会让
# preflight 无条件报两个 ERROR，install.sh 因此永远过不去。
# compose 的映射是 ${DATA_DIR}/backend_priv → /opt/imboy/priv_runtime，
# 按此把容器路径翻译回宿主机路径再检查。
container_to_host_path() {
    local p="$1"
    case "$p" in
        /opt/imboy/priv_runtime/*)
            echo "${SCRIPT_DIR}/${DATA_DIR:-./data}/backend_priv/${p#/opt/imboy/priv_runtime/}"
            ;;
        /opt/imboy/log/*)
            echo "${SCRIPT_DIR}/${DATA_DIR:-./data}/backend_log/${p#/opt/imboy/log/}"
            ;;
        *)
            echo "$p"
            ;;
    esac
}

check_rsa_key() {
    local var_name="$1"
    local path="${!var_name:-}"
    local host_path
    if [[ -z "$path" ]]; then
        err "$var_name 未设置（生产 fail-fast）"
        return
    fi
    host_path="$(container_to_host_path "$path")"
    if [[ ! -f "$host_path" ]]; then
        err "$var_name=$path 文件不存在（宿主机路径 ${host_path}；由 install.sh 自动生成，或见 .env.example 的生成方式）"
    else
        ok "$var_name 文件存在（宿主机 ${host_path}）"
    fi
}
check_rsa_key "IMBOY_LOGIN_RSA_PUB_KEY_FILE"
check_rsa_key "IMBOY_LOGIN_RSA_PRIV_KEY_FILE"

# API_AUTH_SWITCH 必须是 on
if [[ "${IMBOY_API_AUTH_SWITCH:-}" != "on" ]]; then
    err "IMBOY_API_AUTH_SWITCH 未设置为 on（生产环境 fail-fast）"
else
    ok "IMBOY_API_AUTH_SWITCH=on"
fi

# 销售版核心策略：不得把 E2EE、频道或付费频道入口以默认关闭状态发布。
PRODUCT_PROFILE="$(echo "${IMBOY_PRODUCT_PROFILE:-community}" | tr '[:upper:]' '[:lower:]' | tr -d '[:space:]')"
E2EE_MODE="$(echo "${IMBOY_E2EE_MODE:-required}" | tr '[:upper:]' '[:lower:]' | tr -d '[:space:]')"
if [[ "$PRODUCT_PROFILE" != "community" && "$PRODUCT_PROFILE" != "enterprise" ]]; then
    err "IMBOY_PRODUCT_PROFILE 必须为 community 或 enterprise"
else
    ok "IMBOY_PRODUCT_PROFILE=${PRODUCT_PROFILE}"
fi
if [[ "$E2EE_MODE" != "required" && "$E2EE_MODE" != "compliance" ]]; then
    err "IMBOY_E2EE_MODE 必须为 required 或 compliance（销售版严格 E2EE）"
else
    ok "IMBOY_E2EE_MODE=${E2EE_MODE}"
fi
for FEATURE_ENV in IMBOY_FEATURE_E2EE IMBOY_FEATURE_CHANNEL IMBOY_FEATURE_CHANNEL_ORDER; do
    FEATURE_VALUE="$(echo "${!FEATURE_ENV:-true}" | tr '[:upper:]' '[:lower:]' | tr -d '[:space:]')"
    if [[ "$FEATURE_VALUE" != "true" && "$FEATURE_VALUE" != "1" ]]; then
        err "$FEATURE_ENV 必须为 true/1（销售版核心能力不可关闭）"
    else
        ok "$FEATURE_ENV=true"
    fi
done

# Garage 是 community/business 单机包的核心服务，凭据全部由 install.sh 生成。
if [[ -z "${IMBOY_GARAGE_ENDPOINT:-}" || -z "${IMBOY_GARAGE_ACCESS_KEY:-}" || -z "${IMBOY_GARAGE_SECRET_KEY:-}" ]]; then
    err "Garage S3 未配置（IMBOY_GARAGE_ENDPOINT / ACCESS_KEY / SECRET_KEY）"
elif [[ -z "${GARAGE_RPC_SECRET:-}" ]]; then
    err "GARAGE_RPC_SECRET 未设置"
else
    ok "Garage S3 已配置（单机内置服务）"
fi

# SENTRY_DSN 是可选的，但警告
if [[ -z "${SENTRY_DSN:-}" ]]; then
    warn "SENTRY_DSN 未设置（可选，但建议生产环境配置以获取错误追踪）"
else
    ok "SENTRY_DSN 已设置"
fi

# ── 2c. 插件生命周期开关 / Plugin lifecycle switch (A-28) ──────────────────────
PLUGIN_LIFECYCLE="$(echo "${IMBOY_PLUGIN_LIFECYCLE_ENABLED:-false}" | tr '[:upper:]' '[:lower:]' | tr -d '[:space:]')"
if [[ "$PLUGIN_LIFECYCLE" != "true" && "$PLUGIN_LIFECYCLE" != "1" ]]; then
    info "IMBOY_PLUGIN_LIFECYCLE_ENABLED=${PLUGIN_LIFECYCLE}（动态插件写操作未启用，默认期望态）"
    info "  /api/adm/plugin/* 的 install/enable/disable/upgrade/uninstall/reset/force_uninstall 返回「功能未启用」"
    info "  内置功能开关（channel/moment/location/group_collab）是纯 manifest，不受影响"
else
    info "IMBOY_PLUGIN_LIFECYCLE_ENABLED=true（动态插件写操作已放行）"
    info "  SEC-02 收口生效：install path 必须位于受控插件根（IMBOY_PLUGIN_ROOT，默认 priv/plugins）内"
    # SEC-02（审计 #44）：商务档（IMBOY_PRODUCT_PROFILE=enterprise）强制插件可信签名。
    # Erlang 侧 imboy_plugin_signature:signature_required/0 对 enterprise 恒为 true，
    # 缺有效可信公钥时生产启动 fail-fast（imboy_app:ensure_plugin_signature_config/0）
    # 且 install 全部拒绝 —— 此处在部署前就把死配置拦下，避免带病上线。
    if [[ "$PRODUCT_PROFILE" == "enterprise" ]]; then
        if [[ -z "${IMBOY_PLUGIN_TRUSTED_PUBLIC_KEY_FILES:-}" ]]; then
            err "商务档（IMBOY_PRODUCT_PROFILE=enterprise）启用动态插件生命周期必须配置可信签名公钥：IMBOY_PLUGIN_TRUSTED_PUBLIC_KEY_FILES（逗号分隔 32 字节 Ed25519 公钥文件路径），否则后端启动 fail-fast、install 全部拒绝"
        else
            MISSING=0
            IFS=',' read -ra KEY_FILES <<< "$IMBOY_PLUGIN_TRUSTED_PUBLIC_KEY_FILES"
            for KEY_FILE in "${KEY_FILES[@]}"; do
                KEY_FILE="$(echo "$KEY_FILE" | xargs)"
                [[ -z "$KEY_FILE" ]] && continue
                if [[ ! -f "$KEY_FILE" ]]; then
                    err "可信公钥文件不存在：$KEY_FILE（后端加载时跳过该 key，等价缺配置）"
                    MISSING=1
                fi
            done
            if [[ $MISSING -eq 0 ]]; then
                ok "商务档插件签名可信公钥文件已配置"
            fi
        fi
    fi
fi

# ── 2b. 支付模式与凭据检查 / Payment mode & credentials ───────────────────────
echo ""
echo "▶ 2b. 检查支付配置 / Checking payment configuration"

# 社区版：支付网关由 docker-compose.community.yml 硬编码关闭（false），.env 中的
# 支付变量不进容器 —— 销售发布强校验（IMBOY_SALES_RELEASE / live 凭据）在社区
# 版部署下既不适用也无法满足（无商户凭据），跳过以避免把社区安装堵死在死锁上。
# 商务版：维持原有全部检查（行为不变）。
if [[ "$EDITION" == "community" ]]; then
    info "社区版：支付网关由 docker-compose.community.yml 固定关闭（IMBOY_PAYMENT_GATEWAY_ENABLED=false），跳过销售发布支付强校验"
    info "  充值 / 网关回调 / 提现端点将返回「功能未启用」；站内钱包账务（余额/流水/红包/转账）不受影响"
else
PAYMENT_MODE="${IMBOY_PAYMENT_MODE:-sandbox}"
GATEWAY_ENABLED="$(echo "${IMBOY_PAYMENT_GATEWAY_ENABLED:-false}" | tr '[:upper:]' '[:lower:]' | tr -d '[:space:]')"
SALES_RELEASE_RAW="$(echo "${IMBOY_SALES_RELEASE:-true}" | tr '[:upper:]' '[:lower:]' | tr -d '[:space:]')"
case "$SALES_RELEASE_RAW" in
    true|1) SALES_RELEASE=true ;;
    false|0) SALES_RELEASE=false ;;
    *)
        err "IMBOY_SALES_RELEASE 必须为 true/1 或 false/0"
        SALES_RELEASE=false
        ;;
esac

# 外部支付网关总开关关闭时，网关端点（充值/回调/提现）由 handler 直接返回
# ERR_FEATURE_DISABLED，后端也跳过网关凭据的启动校验，因此这里不该拦人。
# 关闭是默认值：没有真实商户凭据的部署方本来就装不起来（sandbox 在 strict
# 环境 fail-fast，live 又缺凭据 fail-fast），那是死锁不是安全。
if [[ "$GATEWAY_ENABLED" != "true" && "$GATEWAY_ENABLED" != "1" ]]; then
    if [[ "$SALES_RELEASE" == "true" ]]; then
        err "销售发布要求 IMBOY_PAYMENT_GATEWAY_ENABLED=true；当前外部支付网关未启用"
        info "  若仅运行社区/钱包演示，请显式设置 IMBOY_SALES_RELEASE=false"
    else
        info "IMBOY_PAYMENT_GATEWAY_ENABLED=${GATEWAY_ENABLED}（外部支付网关未启用）"
        info "  充值 / 网关回调 / 提现端点将返回「功能未启用」；站内钱包账务（余额/流水/红包/转账）不受影响"
        info "  需要对外收款时：置为 true，并按下方要求配齐 IMBOY_PAYMENT_MODE=live + 至少一个网关的完整凭据"
    fi
elif [[ "$PAYMENT_MODE" == "live" ]]; then
    ok "IMBOY_PAYMENT_MODE=live（真实扣款模式）"
    all_set() {
        local var_name
        for var_name in "$@"; do [[ -n "${!var_name:-}" ]] || return 1; done
    }
    any_set() {
        local var_name
        for var_name in "$@"; do [[ -z "${!var_name:-}" ]] || return 0; done
        return 1
    }
    WECHAT_CREDENTIAL_VARS=(IMBOY_WECHAT_MCH_ID IMBOY_WECHAT_APP_ID IMBOY_WECHAT_API_V3_KEY
                           IMBOY_WECHAT_CERT_SERIAL IMBOY_WECHAT_PRIVATE_KEY
                           IMBOY_WECHAT_PLATFORM_PUBLIC_KEY)
    WECHAT_VARS=("${WECHAT_CREDENTIAL_VARS[@]}" IMBOY_WECHAT_NOTIFY_URL)
    ALIPAY_CREDENTIAL_VARS=(IMBOY_ALIPAY_APP_ID IMBOY_ALIPAY_PRIVATE_KEY
                           IMBOY_ALIPAY_PUBLIC_KEY)
    ALIPAY_VARS=("${ALIPAY_CREDENTIAL_VARS[@]}" IMBOY_ALIPAY_NOTIFY_URL)
    STRIPE_VARS=(IMBOY_STRIPE_SECRET_KEY IMBOY_STRIPE_WEBHOOK_SECRET)
    WECHAT_OK=false
    ALIPAY_OK=false
    STRIPE_OK=false
    if all_set "${WECHAT_VARS[@]}" && [[ "${IMBOY_WECHAT_NOTIFY_URL}" == https://* ]]; then
        WECHAT_OK=true
    elif any_set "${WECHAT_CREDENTIAL_VARS[@]}"; then
        err "微信支付配置不完整，需填写 ${WECHAT_VARS[*]}，且回调必须为 HTTPS"
    fi
    if all_set "${ALIPAY_VARS[@]}" && [[ "${IMBOY_ALIPAY_NOTIFY_URL}" == https://* ]]; then
        ALIPAY_OK=true
    elif any_set "${ALIPAY_CREDENTIAL_VARS[@]}"; then
        err "支付宝配置不完整，需填写 ${ALIPAY_VARS[*]}，且回调必须为 HTTPS"
    fi
    if all_set "${STRIPE_VARS[@]}"; then
        STRIPE_OK=true
    elif any_set "${STRIPE_VARS[@]}"; then
        err "Stripe 配置不完整，需填写 ${STRIPE_VARS[*]}"
    fi
    if $WECHAT_OK; then ok "微信支付凭据完整"; fi
    if $ALIPAY_OK; then ok "支付宝凭据完整"; fi
    if $STRIPE_OK; then ok "Stripe 凭据完整"; fi
    if ! $WECHAT_OK && ! $ALIPAY_OK && ! $STRIPE_OK; then
        err "IMBOY_PAYMENT_MODE=live 但没有任何网关填写完整凭据 — 启动将 fail-fast"
        info "至少配置以下其一："
        info "  微信：${WECHAT_VARS[*]}"
        info "  支付宝：${ALIPAY_VARS[*]}"
        info "  Stripe：IMBOY_STRIPE_SECRET_KEY + IMBOY_STRIPE_WEBHOOK_SECRET"
    fi
else
    if [[ "$SALES_RELEASE" == "true" ]]; then
        err "销售发布要求 IMBOY_PAYMENT_MODE=live；当前为 ${PAYMENT_MODE}"
    else
        warn "IMBOY_PAYMENT_MODE=${PAYMENT_MODE}（沙箱模式，回调不验签）— 上线前须改为 live"
    fi
fi
fi  # end edition != community（商务版支付强校验）

# License 检查
LICENSE_FILE="${IMBOY_LICENSE_FILE:-}"
EDITION="${IMBOY_EDITION:-community}"
if [[ "$EDITION" != "community" ]]; then
    if [[ -z "$LICENSE_FILE" ]]; then
        err "IMBOY_EDITION=${EDITION} 但 IMBOY_LICENSE_FILE 未设置"
    elif [[ ! -f "$LICENSE_FILE" ]]; then
        err "IMBOY_LICENSE_FILE=$LICENSE_FILE 文件不存在"
    else
        ok "License 文件存在：$LICENSE_FILE"
    fi
else
    if [[ -n "$LICENSE_FILE" ]]; then
        ok "IMBOY_LICENSE_FILE 已设置（将升级授权）"
    else
        info "社区版运行中，无需 License 文件"
    fi
fi

# ── 3. 系统资源 ───────────────────────────────────────────────────────────────
echo ""
echo "▶ 3. 检查系统资源 / Checking system resources"

# 核心栈至少 4GB；启用 Uptrace（ClickHouse + Redis）至少 8GB，建议 16GB。
TOTAL_MEM_KB=0
if [[ -f /proc/meminfo ]]; then
    TOTAL_MEM_KB=$(grep MemTotal /proc/meminfo | awk '{print $2}')
elif command -v sysctl &>/dev/null; then
    TOTAL_MEM_KB=$(sysctl -n hw.memsize 2>/dev/null | awk '{print int($1/1024)}' || echo 0)
fi
TOTAL_MEM_GB=$(( TOTAL_MEM_KB / 1024 / 1024 ))
MIN_MEM_GB=4; RECOMMENDED_MEM_GB=8
if [[ "$UPTRACE_ENABLED_VALUE" == "true" || "$UPTRACE_ENABLED_VALUE" == "1" ]]; then
    MIN_MEM_GB=8; RECOMMENDED_MEM_GB=16
fi
if (( TOTAL_MEM_GB >= RECOMMENDED_MEM_GB )); then
    ok "内存 ${TOTAL_MEM_GB}GB >= ${RECOMMENDED_MEM_GB}GB"
elif (( TOTAL_MEM_GB >= MIN_MEM_GB )); then
    warn "内存 ${TOTAL_MEM_GB}GB（建议 >= ${RECOMMENDED_MEM_GB}GB）"
else
    err "内存 ${TOTAL_MEM_GB}GB < ${MIN_MEM_GB}GB，无法可靠运行"
fi

# 核心栈至少 10GB；Uptrace 的 ClickHouse 开启后至少 20GB。
if command -v df &>/dev/null; then
    AVAIL_KB=$(df -k . 2>/dev/null | tail -1 | awk '{print $4}')
    AVAIL_GB=$(( AVAIL_KB / 1024 / 1024 ))
    MIN_DISK_GB=10; RECOMMENDED_DISK_GB=20
    if [[ "$UPTRACE_ENABLED_VALUE" == "true" || "$UPTRACE_ENABLED_VALUE" == "1" ]]; then
        MIN_DISK_GB=20; RECOMMENDED_DISK_GB=40
    fi
    if (( AVAIL_GB >= RECOMMENDED_DISK_GB )); then
        ok "可用磁盘空间 ${AVAIL_GB}GB >= ${RECOMMENDED_DISK_GB}GB"
    elif (( AVAIL_GB >= MIN_DISK_GB )); then
        warn "可用磁盘 ${AVAIL_GB}GB（建议 >= ${RECOMMENDED_DISK_GB}GB）"
    else
        err "可用磁盘 ${AVAIL_GB}GB < ${MIN_DISK_GB}GB，存储不足"
    fi
fi

# ── 4. 端口占用 ───────────────────────────────────────────────────────────────
echo ""
echo "▶ 4. 检查端口占用 / Checking port availability"

check_port() {
    local port="$1"
    if command -v ss &>/dev/null; then
        if ss -tlnp 2>/dev/null | grep -q ":${port} "; then
            warn "端口 $port 已被占用"
        else
            ok "端口 $port 可用"
        fi
    elif command -v lsof &>/dev/null; then
        if lsof -iTCP:"$port" -sTCP:LISTEN &>/dev/null 2>&1; then
            warn "端口 $port 已被占用"
        else
            ok "端口 $port 可用"
        fi
    else
        info "无法检查端口 ${port}（ss/lsof 不可用）"
    fi
}

check_port 80
check_port 443
check_port "${BACKEND_PORT:-9800}"
check_port "${PG_PORT:-5432}"
check_port "${GRAFANA_PORT:-3000}"

# ── 4b. OIDC 多节点一次性状态 ─────────────────────────────────────────────────
# auth_oidc_logic 的 state/otc 存在**节点本地 ETS**（?ONETIME_TAB）。
# 多节点部署时，authorize 在 A 节点写入的 state，callback 若被负载均衡打到
# B 节点就取不到，登录会以「state 无效」失败 —— 表现得像遭到攻击，运维极难定位。
# 因此多节点 + 启用 OIDC 必须显式拦下，而不是让它上线后随机失败。
echo ""
echo "▶ 4b. 检查 OIDC 多节点状态共享 / Checking OIDC multi-node state"

OIDC_ENABLED="${IMBOY_OIDC_ENABLED:-${SSO_OAUTH2_ENABLED:-}}"
if [[ -z "${CLUSTER_NODES:-}" ]]; then
    ok "单节点部署，OIDC 节点本地状态可用"
elif [[ "$OIDC_ENABLED" != "true" && "$OIDC_ENABLED" != "1" ]]; then
    info "多节点部署但未启用 OIDC，跳过"
elif [[ "${IMBOY_LB_STICKY_SESSION:-}" == "true" || "${IMBOY_LB_STICKY_SESSION:-}" == "1" ]]; then
    # 粘性会话下同一浏览器会话固定打到同一后端，节点本地状态是安全的
    ok "多节点 + OIDC，已声明负载均衡启用粘性会话"
else
    err "多节点（CLUSTER_NODES 已设置）+ 启用 OIDC，但 OIDC state/otc 只存节点本地 ETS。请二选一：a) 负载均衡开启粘性会话并设置 IMBOY_LB_STICKY_SESSION=true；b) 改为单节点承载 OIDC 回调（CLUSTER_NODES 留空）"
fi

# ── 5. Docker 检查（可选）────────────────────────────────────────────────────
if [[ "$DOCKER_CHECK" -eq 1 ]]; then
    echo ""
    echo "▶ 5. 检查 Docker 环境 / Checking Docker environment"

    if ! command -v docker &>/dev/null; then
        err "docker 命令不存在，请安装 Docker 24+"
    else
        DOCKER_VER=$(docker version --format '{{.Server.Version}}' 2>/dev/null || echo "unknown")
        ok "Docker 已安装（版本：${DOCKER_VER}）"
    fi

    if ! docker compose version &>/dev/null 2>&1; then
        err "docker compose 插件不可用，请安装 Docker Compose v2"
    else
        ok "docker compose 插件可用"
    fi

    if ! docker info &>/dev/null 2>&1; then
        err "Docker daemon 未运行，请启动 Docker"
    else
        ok "Docker daemon 运行中"
    fi
fi

# ── 汇总 ────────────────────────────────────────────────────────────────────
echo ""
echo "=========================================="
if (( ERRORS > 0 )); then
    echo -e " ${RED}结果: $ERRORS 个 ERROR，$WARNINGS 个 WARN${NC}"
    echo " 请修复所有 ERROR 后再部署"
    echo "=========================================="
    exit 1
elif (( WARNINGS > 0 )); then
    echo -e " ${YELLOW}结果: 0 个 ERROR，$WARNINGS 个 WARN${NC}"
    echo " 可以继续部署，但建议解决以上警告"
    echo "=========================================="
    exit 0
else
    echo -e " ${GREEN}结果: 全部通过 ✓${NC}"
    echo "=========================================="
    exit 0
fi
