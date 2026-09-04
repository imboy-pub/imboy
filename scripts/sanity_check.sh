#!/usr/bin/env bash
# IMBoy 部署后健康自检 / Post-deploy sanity check
#
# 用法 / Usage（从任意目录）:
#   bash scripts/sanity_check.sh                # 完整 8 项校验（默认 prod compose）
#   bash scripts/sanity_check.sh --skip-tls     # 本地无 TLS/域名时跳过 nginx/Grafana 严格项
#   bash scripts/sanity_check.sh -f docker-compose.community.yml
#   COMPOSE_FILE=docker-compose.community.yml bash scripts/sanity_check.sh
#
# 选项 / Options:
#   --skip-tls              跳过 nginx 443 / Grafana 严格校验（本地无 TLS/域名时）
#   -f, --compose-file FILE 指定 compose 文件；可重复传入 overlay
#                           亦可用空格分隔的 COMPOSE_FILES 环境变量覆盖
#   -h, --help              显示本帮助
#
# 退出码 / Exit code: 任一 [ERROR] -> 1；全部通过 -> 0
set -uo pipefail

SKIP_TLS=0
COMPOSE_FILES_VALUE="${COMPOSE_FILES:-${COMPOSE_FILE:-docker-compose.prod.yml}}"
COMPOSE_FILES_LIST=()

usage() {
    sed -n '2,16p' "$0" | sed 's/^# \{0,1\}//'
}

while [ $# -gt 0 ]; do
    case "$1" in
        --skip-tls)
            SKIP_TLS=1
            ;;
        -f|--compose-file)
            if [ $# -lt 2 ]; then
                echo "[ERROR] $1 需要一个参数（compose 文件名）" >&2
                exit 2
            fi
            COMPOSE_FILES_LIST+=("$2")
            shift
            ;;
        --compose-file=*)
            compose_file="${1#*=}"
            [ -n "$compose_file" ] || { echo "[ERROR] --compose-file 不能为空" >&2; exit 2; }
            COMPOSE_FILES_LIST+=("$compose_file")
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            echo "[ERROR] 未知参数: $1（见 --help）" >&2
            usage
            exit 2
            ;;
    esac
    shift
done

if [ "${#COMPOSE_FILES_LIST[@]}" -eq 0 ]; then
    for compose_file in $COMPOSE_FILES_VALUE; do COMPOSE_FILES_LIST+=("$compose_file"); done
fi

# 定位到 deploy/ 目录（compose 与 .env 所在）
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
DEPLOY_DIR="$SCRIPT_DIR/../deploy"
cd "$DEPLOY_DIR" 2>/dev/null || { echo "[ERROR] 找不到 deploy/ 目录"; exit 1; }

# compose 文件存在性检查（避免静默检错栈）
COMPOSE_ARGS=()
for compose_file in "${COMPOSE_FILES_LIST[@]}"; do
    if [ ! -f "$compose_file" ]; then
        echo "[ERROR] compose 文件不存在: deploy/$compose_file"
        exit 1
    fi
    COMPOSE_ARGS+=(-f "$compose_file")
done

# 加载 .env（取 PG 连接、端口、域名）
if [ -f .env ]; then
    set -a; . ./.env; set +a
fi
POSTGRES_USER="${POSTGRES_USER:-imboy_user}"
POSTGRES_DB="${POSTGRES_DB:-imboy_pro}"
BACKEND_PORT="${BACKEND_PORT:-9800}"
GRAFANA_PORT="${GRAFANA_PORT:-3000}"
compose() { docker compose "${COMPOSE_ARGS[@]}" "$@"; }
COMPOSE_LABEL="${COMPOSE_FILES_LIST[*]}"

GREEN='\033[0;32m'; RED='\033[0;31m'; YELLOW='\033[0;33m'; NC='\033[0m'
FAIL=0; WARN=0
ok()   { echo -e "${GREEN}[OK]${NC}   $1"; }
err()  { echo -e "${RED}[ERROR]${NC} $1"; FAIL=$((FAIL+1)); }
warn() { echo -e "${YELLOW}[WARN]${NC} $1"; WARN=$((WARN+1)); }

echo "==== IMBoy 部署后自检（compose=${COMPOSE_LABEL} skip_tls=${SKIP_TLS}）===="

# 1. 编排内核心容器全部 running（init job 除外）
RUNNING_SERVICES="$(compose ps --status running --services 2>/dev/null)"
EXPECTED_SERVICES="imboy_pg18 garage imboy_backend imboy_admin imboy_nginx imboy_certbot imboy_livekit"
case "$COMPOSE_LABEL" in
    *docker-compose.uptrace.yml*)
        EXPECTED_SERVICES="$EXPECTED_SERVICES uptrace_clickhouse uptrace_redis uptrace uptrace_otelcol"
        ;;
esac
for service in $EXPECTED_SERVICES; do
    if printf '%s\n' "$RUNNING_SERVICES" | grep -qx "$service"; then
        ok "$service 容器在运行"
    else
        err "$service 容器未运行"
    fi
done

# 2. PostgreSQL 就绪
if compose exec -T imboy_pg18 pg_isready -U "$POSTGRES_USER" -d "$POSTGRES_DB" >/dev/null 2>&1; then
    ok "PostgreSQL pg_isready 通过"
else
    err "PostgreSQL 未就绪"
fi

# 3. 后端健康端点
HEALTH="$(curl -fsS --max-time 5 "http://localhost:${BACKEND_PORT}/healthz" 2>/dev/null || true)"
if printf '%s' "$HEALTH" | grep -q '"status":"ok"'; then
    ok "后端 /healthz 响应 ok"
else
    err "后端 :${BACKEND_PORT}/healthz 未返回 status=ok"
fi

# 4. 后端日志含启动成功标记且无近期 crash
LOGS=$(compose logs --tail=300 imboy_backend 2>/dev/null)
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
ROWS=$(compose exec -T imboy_pg18 psql -U "$POSTGRES_USER" -d "$POSTGRES_DB" -tAc \
    "SELECT count(*) FROM schema_migrations" 2>/dev/null | tr -d '[:space:]')
if [ -n "$ROWS" ] && [ "$ROWS" -gt 0 ] 2>/dev/null; then
    ok "数据库迁移已执行（schema_migrations=${ROWS}）"
else
    err "schema_migrations 为空或不可查（迁移未跑？）"
fi

# 6. 管理后台静态页响应
ACODE=$(compose exec -T imboy_admin wget -qO- http://localhost/health 2>/dev/null)
if [ "$ACODE" = "ok" ]; then
    ok "管理后台 /health 响应 ok"
else
    warn "管理后台 /health 未返回 ok（检查 imboy_admin 容器）"
fi

# 7/8. nginx TLS 与 Grafana（本地无域名时降级）
if [ "$SKIP_TLS" = "1" ]; then
    warn "已跳过 nginx 443 / Grafana 严格校验（--skip-tls）"
else
    if compose exec -T imboy_nginx sh -c 'wget -qO- --no-check-certificate https://localhost/ >/dev/null 2>&1'; then
        ok "nginx 443 响应"
    else
        err "nginx 443 无响应（DNS/证书/端口）"
    fi
    # certbot 容器存在性（自动续期 TLS 证书）
    if printf '%s\n' "$RUNNING_SERVICES" | grep -qx imboy_certbot; then
        ok "certbot 容器在运行（TLS 自动续期）"
    else
        warn "certbot 容器未运行（证书将无法自动续期）"
    fi
    if printf '%s\n' "$RUNNING_SERVICES" | grep -qx imboy_grafana; then
        GCODE=$(curl -s -o /dev/null -w "%{http_code}" --max-time 5 "http://localhost:${GRAFANA_PORT}/" 2>/dev/null || echo "000")
        if [ "$GCODE" != "000" ]; then
            ok "Grafana :${GRAFANA_PORT} 响应（${GCODE}）"
        else
            err "Grafana :${GRAFANA_PORT} 无响应"
        fi
    else
        ok "Grafana 未启用，跳过"
    fi
fi

# 9. Uptrace overlay 启用时验证内部健康端点
case "$COMPOSE_LABEL" in
*docker-compose.uptrace.yml*)
    if compose exec -T imboy_nginx wget -qO- http://uptrace/api/health >/dev/null 2>&1; then
        ok "Uptrace /api/health 响应"
    else
        err "Uptrace /api/health 无响应"
    fi
    ;;
esac

echo "==== 结果：失败 ${FAIL} 项 / 告警 ${WARN} 项 ===="
[ "$FAIL" -gt 0 ] && exit 1
exit 0
