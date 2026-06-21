#!/usr/bin/env bash
# IMBoy 一键部署 / One-command deployment.
#
# 把 README 的"五步部署"收敛成一条命令：
#   preflight → 生成 .env(自动随机密钥) → 起服务 → 等健康 → sanity → 打印访问地址
#
#   cd imboy/deploy && bash install.sh
#
# 设计：机器能自动的全自动（密钥），只让人填机器不可能知道的（域名）。
# 幂等：.env 已存在则不动；重复运行只会确保服务在跑。
set -euo pipefail
cd "$(dirname "$0")"

COMPOSE="docker compose -f docker-compose.prod.yml"
# 需要随机密钥的字段（域名不在内，必须人工填）
SECRET_VARS="POSTGRES_PASSWORD JWT_KEY POSTGRE_AES_KEY ADM_COOKIE_SECRET GRAFANA_ADMIN_PASSWORD"

say()  { printf '\n\033[1;36m==> %s\033[0m\n' "$*"; }
die()  { printf '\n\033[1;31m❌ %s\033[0m\n' "$*" >&2; exit 1; }

# 替换 .env 中 KEY=... 行（按字段名精确匹配，值含特殊字符也安全；BSD/GNU 通用）。
set_var() {
  local key="$1" val="$2" tmp
  tmp="$(mktemp)"
  awk -v k="$key" -v v="$val" 'BEGIN{FS=OFS="="} $1==k{print k"="v; next} {print}' .env >"$tmp"
  mv "$tmp" .env
}
get_var() { grep -E "^$1=" .env | head -1 | cut -d= -f2-; }

# 1) 前置检查
say "前置检查 (preflight)"
bash preflight.sh --docker || die "preflight 未通过，修复 ERROR 后重跑"

# 2) 配置 .env
if [ ! -f .env ]; then
  say "首次部署：生成 .env 并写入随机密钥"
  cp .env.example .env
  command -v openssl >/dev/null || die "需要 openssl 生成密钥"
  for v in $SECRET_VARS; do set_var "$v" "$(openssl rand -hex 16)"; done
  printf '\n\033[1;33m⚠️  密钥已自动生成。还需人工填写 2 个域名后重跑本脚本：\033[0m\n'
  printf '    %s/.env 中的 API_DOMAIN 与 ADMIN_DOMAIN（需已 DNS 解析到本机）\n' "$(pwd)"
  exit 0
fi

# 必填域名校验
for v in API_DOMAIN ADMIN_DOMAIN; do
  cur="$(get_var "$v" || true)"
  case "$cur" in ""|*example.com) die "$v 尚未填写真实域名（当前: ${cur:-空}），编辑 .env 后重跑";; esac
done

# 3) 网络 + 启动
say "创建网络并启动服务"
docker network create imboy-network 2>/dev/null || true
$COMPOSE up -d

# 4) 等待后端健康（最多 120s）
say "等待后端启动 (最多 120s)"
ok=0
for _ in $(seq 1 60); do
  if $COMPOSE logs imboy_backend 2>/dev/null | grep -q "started on port"; then ok=1; break; fi
  sleep 2
done
[ "$ok" = 1 ] || die "后端 120s 内未就绪，查日志：$COMPOSE logs imboy_backend"

# 5) 部署后自检（存在才跑）
if [ -f ../scripts/sanity_check.sh ]; then
  say "部署后自检 (sanity_check)"
  bash ../scripts/sanity_check.sh || printf '\033[1;33m⚠️ sanity_check 有警告，请人工确认\033[0m\n'
fi

# 6) 完成
api="$(get_var API_DOMAIN)"; adm="$(get_var ADMIN_DOMAIN)"
graf="$(get_var GRAFANA_ADMIN_PASSWORD)"
cat <<EOF

✅ 部署完成 / Deployment done

   管理后台 / Admin : https://${adm}   （首次访问自动进入 /setup 创建超管）
   API / WebSocket  : https://${api}
   Grafana          : http://<本机IP>:3000  (admin / ${graf})

   服务状态 : $COMPOSE ps
   后端日志 : $COMPOSE logs -f imboy_backend
EOF
