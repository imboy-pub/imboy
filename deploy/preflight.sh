#!/usr/bin/env bash
# IMBoy 生产部署前置检查 / Pre-flight check for production deployment
#
# Usage:
#   bash deploy/preflight.sh            # 检查 .env 和系统资源
#   bash deploy/preflight.sh --docker   # 同上，附加检查 Docker 环境
#
# Exit codes:
#   0 = all checks passed
#   1 = one or more ERROR checks failed

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ENV_FILE="${SCRIPT_DIR}/.env"
ERRORS=0
WARNINGS=0

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
check_var "GRAFANA_ADMIN_PASSWORD"

# SENTRY_DSN 是可选的，但警告
if [[ -z "${SENTRY_DSN:-}" ]]; then
    warn "SENTRY_DSN 未设置（可选，但建议生产环境配置以获取错误追踪）"
else
    ok "SENTRY_DSN 已设置"
fi

# ── 2b. 支付模式与凭据检查 / Payment mode & credentials ───────────────────────
echo ""
echo "▶ 2b. 检查支付配置 / Checking payment configuration"

PAYMENT_MODE="${IMBOY_PAYMENT_MODE:-sandbox}"
if [[ "$PAYMENT_MODE" == "live" ]]; then
    ok "IMBOY_PAYMENT_MODE=live（真实扣款模式）"
    # 检查至少一个网关凭据完整
    WECHAT_OK=false
    ALIPAY_OK=false
    STRIPE_OK=false
    [[ -n "${IMBOY_WECHAT_MCH_ID:-}" && -n "${IMBOY_WECHAT_API_V3_KEY:-}" && -n "${IMBOY_WECHAT_PLATFORM_PUBLIC_KEY:-}" ]] && WECHAT_OK=true
    [[ -n "${IMBOY_ALIPAY_APP_ID:-}" && -n "${IMBOY_ALIPAY_PRIVATE_KEY:-}" && -n "${IMBOY_ALIPAY_PUBLIC_KEY:-}" ]] && ALIPAY_OK=true
    [[ -n "${IMBOY_STRIPE_SECRET_KEY:-}" && -n "${IMBOY_STRIPE_WEBHOOK_SECRET:-}" ]] && STRIPE_OK=true
    if $WECHAT_OK; then ok "微信支付凭据完整"; fi
    if $ALIPAY_OK; then ok "支付宝凭据完整"; fi
    if $STRIPE_OK; then ok "Stripe 凭据完整"; fi
    if ! $WECHAT_OK && ! $ALIPAY_OK && ! $STRIPE_OK; then
        err "IMBOY_PAYMENT_MODE=live 但没有任何网关填写完整凭据 — 启动将 fail-fast"
        info "至少配置以下其一："
        info "  微信：IMBOY_WECHAT_MCH_ID + IMBOY_WECHAT_API_V3_KEY + IMBOY_WECHAT_PLATFORM_PUBLIC_KEY（+其余5项）"
        info "  支付宝：IMBOY_ALIPAY_APP_ID + IMBOY_ALIPAY_PRIVATE_KEY + IMBOY_ALIPAY_PUBLIC_KEY"
        info "  Stripe：IMBOY_STRIPE_SECRET_KEY + IMBOY_STRIPE_WEBHOOK_SECRET"
    fi
else
    warn "IMBOY_PAYMENT_MODE=${PAYMENT_MODE}（沙箱模式，回调不验签）— 上线前须改为 live"
fi

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

# 内存 >= 8GB
TOTAL_MEM_KB=0
if [[ -f /proc/meminfo ]]; then
    TOTAL_MEM_KB=$(grep MemTotal /proc/meminfo | awk '{print $2}')
elif command -v sysctl &>/dev/null; then
    TOTAL_MEM_KB=$(sysctl -n hw.memsize 2>/dev/null | awk '{print int($1/1024)}' || echo 0)
fi
TOTAL_MEM_GB=$(( TOTAL_MEM_KB / 1024 / 1024 ))
if (( TOTAL_MEM_GB >= 8 )); then
    ok "内存 ${TOTAL_MEM_GB}GB >= 8GB"
elif (( TOTAL_MEM_GB >= 4 )); then
    warn "内存 ${TOTAL_MEM_GB}GB（建议 >= 8GB，当前可运行但性能受限）"
else
    err "内存 ${TOTAL_MEM_GB}GB < 4GB，无法可靠运行"
fi

# 磁盘 >= 20GB
if command -v df &>/dev/null; then
    AVAIL_KB=$(df -k . 2>/dev/null | tail -1 | awk '{print $4}')
    AVAIL_GB=$(( AVAIL_KB / 1024 / 1024 ))
    if (( AVAIL_GB >= 20 )); then
        ok "可用磁盘空间 ${AVAIL_GB}GB >= 20GB"
    elif (( AVAIL_GB >= 10 )); then
        warn "可用磁盘 ${AVAIL_GB}GB（建议 >= 20GB）"
    else
        err "可用磁盘 ${AVAIL_GB}GB < 10GB，存储不足"
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
        info "无法检查端口 $port（ss/lsof 不可用）"
    fi
}

check_port 80
check_port 443
check_port "${BACKEND_PORT:-9800}"
check_port "${PG_PORT:-5432}"
check_port "${GRAFANA_PORT:-3000}"

# ── 5. Docker 检查（可选）────────────────────────────────────────────────────
if [[ "${1:-}" == "--docker" ]]; then
    echo ""
    echo "▶ 5. 检查 Docker 环境 / Checking Docker environment"

    if ! command -v docker &>/dev/null; then
        err "docker 命令不存在，请安装 Docker 24+"
    else
        DOCKER_VER=$(docker version --format '{{.Server.Version}}' 2>/dev/null || echo "unknown")
        ok "Docker 已安装（版本：$DOCKER_VER）"
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
