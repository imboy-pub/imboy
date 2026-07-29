#!/usr/bin/env bash
# ============================================================
# TLS 证书到期检查 / TLS certificate expiry check
# ------------------------------------------------------------
# 把证书到期时间推到 Pushgateway，供 imboy-alerts.yml 的
# IMBoyTLSCertExpiringSoon / IMBoyTLSCertExpired 告警消费。
#
# 为什么不用 blackbox_exporter：当前 deploy/ 下没有部署 Prometheus
# 采集栈，引入 exporter 需要先搭一整套服务。本脚本复用备份已有的
# Pushgateway 通路，零新增服务即可让告警有真实的指标产出方。
# 有完整采集栈后可改用 blackbox_exporter 的 probe_ssl_earliest_cert_expiry。
#
# 用法 / Usage:
#   IMBOY_TLS_DOMAINS="im.example.com,api.example.com" bash scripts/check_tls_expiry.sh
#   PUSHGATEWAY_URL=http://pushgateway:9091 bash scripts/check_tls_expiry.sh
#
# 环境变量 / Env:
#   IMBOY_TLS_DOMAINS  逗号分隔的域名列表（必填，不设置则跳过并退出 0）
#   TLS_PORT           默认 443
#   PUSHGATEWAY_URL    未设置时只打印不推送
# ============================================================
set -euo pipefail

. "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/metrics_push.sh"

IMBOY_TLS_DOMAINS="${IMBOY_TLS_DOMAINS:-}"
TLS_PORT="${TLS_PORT:-443}"

GREEN='\033[0;32m'; YELLOW='\033[1;33m'; RED='\033[0;31m'; NC='\033[0m'
info() { echo -e "${GREEN}[tls_expiry]${NC} $*"; }
warn() { echo -e "${YELLOW}[tls_expiry]${NC} $*"; }
err()  { echo -e "${RED}[tls_expiry] ERROR:${NC} $*" >&2; }

if [ -z "$IMBOY_TLS_DOMAINS" ]; then
  warn "IMBOY_TLS_DOMAINS 未设置，跳过 TLS 到期检查"
  exit 0
fi

command -v openssl >/dev/null 2>&1 || { err "openssl 未安装"; exit 1; }

# 把 openssl 的 notAfter 时间转成 unix 时间戳。
# macOS(BSD date) 与 Linux(GNU date) 参数不同，两种都试。
notafter_to_epoch() {
  local not_after="$1"
  date -j -f "%b %d %T %Y %Z" "$not_after" "+%s" 2>/dev/null \
    || date -d "$not_after" "+%s" 2>/dev/null \
    || echo ""
}

FAILED=0
IFS=',' read -r -a DOMAINS <<< "$IMBOY_TLS_DOMAINS"
for raw in "${DOMAINS[@]}"; do
  domain="$(echo "$raw" | tr -d '[:space:]')"
  [ -n "$domain" ] || continue

  not_after="$(
    echo | openssl s_client -servername "$domain" -connect "${domain}:${TLS_PORT}" 2>/dev/null \
      | openssl x509 -noout -enddate 2>/dev/null \
      | cut -d= -f2
  )"

  if [ -z "$not_after" ]; then
    err "无法获取 ${domain} 的证书（连接失败或非 TLS 端口）"
    FAILED=1
    continue
  fi

  expiry_ts="$(notafter_to_epoch "$not_after")"
  if [ -z "$expiry_ts" ]; then
    err "无法解析 ${domain} 的到期时间: ${not_after}"
    FAILED=1
    continue
  fi

  days_left="$(( (expiry_ts - $(date -u +%s)) / 86400 ))"
  info "${domain} 证书到期 ${not_after}（剩余 ${days_left} 天）"
  push_tls_expiry "$domain" "$expiry_ts"
done

# 有域名检查失败时以非 0 退出，让调度器（cron）能感知；
# 已成功的域名指标此时已推送，不受影响。
exit "$FAILED"
