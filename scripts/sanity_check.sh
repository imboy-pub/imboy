#!/usr/bin/env bash
# IMBoy 部署后健康自检 / Post-deploy sanity check
#
# 用法 / Usage（从任意目录）:
#   bash scripts/sanity_check.sh            # 完整 8 项校验
#   bash scripts/sanity_check.sh --skip-tls # 本地无 TLS/域名时跳过 nginx/Grafana 严格项
#
# 退出码 / Exit code: 任一 [ERROR] -> 1；全部通过 -> 0
set -uo pipefail

SKIP_TLS=0
[ "${1:-}" = "--skip-tls" ] && SKIP_TLS=1

# 定位到 deploy/ 目录（compose 与 .env 所在）
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
DEPLOY_DIR="$SCRIPT_DIR/../deploy"
cd "$DEPLOY_DIR" 2>/dev/null || { echo "[ERROR] 找不到 deploy/ 目录"; exit 1; }

# 加载 .env（取 PG 连接、端口、域名）
if [ -f .env ]; then
    set -a; . ./.env; set +a
fi
POSTGRES_USER="${POSTGRES_USER:-imboy_user}"
POSTGRES_DB="${POSTGRES_DB:-imboy_pro}"
BACKEND_PORT="${BACKEND_PORT:-9800}"
GRAFANA_PORT="${GRAFANA_PORT:-3000}"
COMPOSE="docker compose -f docker-compose.prod.yml"

GREEN='\033[0;32m'; RED='\033[0;31m'; YELLOW='\033[0;33m'; NC='\033[0m'
FAIL=0; WARN=0
ok()   { echo -e "${GREEN}[OK]${NC}   $1"; }
err()  { echo -e "${RED}[ERROR]${NC} $1"; FAIL=$((FAIL+1)); }
warn() { echo -e "${YELLOW}[WARN]${NC} $1"; WARN=$((WARN+1)); }

echo "==== IMBoy 部署后自检（skip_tls=$SKIP_TLS）===="

# 1. 容器全部 running
if $COMPOSE ps --status running 2>/dev/null | grep -q imboy_backend; then
    ok "核心容器在运行"
else
    err "核心容器未全部 running（$COMPOSE ps 查看）"
fi

# 2. PostgreSQL 就绪
if $COMPOSE exec -T imboy_pg18 pg_isready -U "$POSTGRES_USER" -d "$POSTGRES_DB" >/dev/null 2>&1; then
    ok "PostgreSQL pg_isready 通过"
else
    err "PostgreSQL 未就绪"
fi

# 3. 后端 HTTP 响应（连接成功即视为存活，根路径可能 200/404 均算 up）
CODE=$(curl -s -o /dev/null -w "%{http_code}" --max-time 5 "http://localhost:${BACKEND_PORT}/" 2>/dev/null || echo "000")
if [ "$CODE" != "000" ]; then
    ok "后端 HTTP 响应（:${BACKEND_PORT} -> $CODE）"
else
    err "后端 :${BACKEND_PORT} 无 HTTP 响应"
fi

# 4. 后端日志含启动成功标记且无近期 crash
LOGS=$($COMPOSE logs --tail=300 imboy_backend 2>/dev/null)
if echo "$LOGS" | grep -qE "started on port|imboy started"; then
    ok "后端日志含启动成功标记"
else
    warn "后端日志未见 'started on port'（可能仍在启动）"
fi
if echo "$LOGS" | grep -qiE "CRASH REPORT|=ERROR REPORT.*application: imboy.*exited"; then
    err "后端日志含 crash/退出报告"
else
    ok "后端日志无 crash 报告"
fi

# 5. 数据库迁移已执行（schema_migrations 有记录）
ROWS=$($COMPOSE exec -T imboy_pg18 psql -U "$POSTGRES_USER" -d "$POSTGRES_DB" -tAc \
    "SELECT count(*) FROM schema_migrations" 2>/dev/null | tr -d '[:space:]')
if [ -n "$ROWS" ] && [ "$ROWS" -gt 0 ] 2>/dev/null; then
    ok "数据库迁移已执行（schema_migrations=$ROWS）"
else
    err "schema_migrations 为空或不可查（迁移未跑？）"
fi

# 6. 管理后台静态页响应
ACODE=$($COMPOSE exec -T imboy_admin wget -qO- http://localhost/health 2>/dev/null)
if [ "$ACODE" = "ok" ]; then
    ok "管理后台 /health 响应 ok"
else
    warn "管理后台 /health 未返回 ok（检查 imboy_admin 容器）"
fi

# 7/8. nginx TLS 与 Grafana（本地无域名时降级）
if [ "$SKIP_TLS" = "1" ]; then
    warn "已跳过 nginx 443 / Grafana 严格校验（--skip-tls）"
else
    if $COMPOSE exec -T imboy_nginx sh -c 'wget -qO- --no-check-certificate https://localhost/ >/dev/null 2>&1'; then
        ok "nginx 443 响应"
    else
        err "nginx 443 无响应（DNS/证书/端口）"
    fi
    # certbot 容器存在性（自动续期 TLS 证书）
    if $COMPOSE ps --status running 2>/dev/null | grep -q imboy_certbot; then
        ok "certbot 容器在运行（TLS 自动续期）"
    else
        warn "certbot 容器未运行（证书将无法自动续期）"
    fi
    GCODE=$(curl -s -o /dev/null -w "%{http_code}" --max-time 5 "http://localhost:${GRAFANA_PORT}/" 2>/dev/null || echo "000")
    if [ "$GCODE" != "000" ]; then
        ok "Grafana :${GRAFANA_PORT} 响应（$GCODE）"
    else
        err "Grafana :${GRAFANA_PORT} 无响应"
    fi
fi

echo "==== 结果：失败 $FAIL 项 / 告警 $WARN 项 ===="
[ "$FAIL" -gt 0 ] && exit 1
exit 0
