#!/usr/bin/env bash
# IMBoy 一键部署 / One-command deployment.
#
# 把 README 的"五步部署"收敛成一条命令：
#   生成 .env(自动随机密钥 + RSA 密钥对) → preflight → 起服务 → 签发 TLS
#   → 等健康 → sanity → 打印访问地址
#
#   cd imboy/deploy && bash install.sh
#
# 设计：机器能自动的全自动（密钥、RSA、证书），只让人填机器不可能知道的
#      （两个域名 + 证书通知邮箱）。首次运行会在生成 .env 后停下来等人填这三项。
# 幂等：.env 已存在则不动；证书已签发则跳过；重复运行只会确保服务在跑。
set -euo pipefail
cd "$(dirname "$0")"

COMPOSE_FILE="docker-compose.prod.yml"
POLICY_FILE="docker-compose-sales-policy.yml"
COMPOSE="docker compose -f $COMPOSE_FILE -f $POLICY_FILE"

# 需要随机密钥的字段（域名与邮箱不在内，必须人工填）
# 长度约束来自 preflight.sh 与后端启动校验：
#   *_KEY / IMBOY_SOLIDIFIED_KEY 需 32 字节 → rand -hex 16 得 32 个十六进制字符
#   IMBOY_SOLIDIFIED_KEY_IV 需 16 字节      → rand -hex 8
#   LIVEKIT_API_SECRET 需 ≥32 字符          → rand -hex 24 得 48 字符，留余量
SECRET_VARS_32="POSTGRES_PASSWORD JWT_KEY POSTGRE_AES_KEY ADM_COOKIE_SECRET GRAFANA_ADMIN_PASSWORD IMBOY_SOLIDIFIED_KEY IMBOY_PASSWORD_SALT LIVEKIT_API_KEY"
SECRET_VARS_16="IMBOY_SOLIDIFIED_KEY_IV"
SECRET_VARS_48="LIVEKIT_API_SECRET"

# 必须人工填写的字段（机器无从知晓）
MANUAL_VARS="API_DOMAIN ADMIN_DOMAIN CERTBOT_EMAIL"

say()  { printf '\n\033[1;36m==> %s\033[0m\n' "$*"; }
die()  { printf '\n\033[1;31m❌ %s\033[0m\n' "$*" >&2; exit 1; }
warn() { printf '\033[1;33m⚠️  %s\033[0m\n' "$*"; }

# 替换 .env 中 KEY=... 行（按字段名精确匹配，值含特殊字符也安全；BSD/GNU 通用）。
set_var() {
  local key="$1" val="$2" tmp
  tmp="$(mktemp)"
  awk -v k="$key" -v v="$val" 'BEGIN{FS=OFS="="} $1==k{print k"="v; next} {print}' .env >"$tmp"
  mv "$tmp" .env
}
get_var() { grep -E "^$1=" .env | head -1 | cut -d= -f2-; }

# ── 0) 生产 compose 文件存在性 ────────────────────────────────────────────────
# 该文件按商务决策走单独交付渠道，不随开源仓分发（.gitignore 精确排除）。
# 不检查的话，第一次用到它是在 `$COMPOSE up -d`，docker 只会吐一句
# "no configuration file provided"，非开发者完全无从判断发生了什么。
if [ ! -f "$COMPOSE_FILE" ]; then
  die "缺少 $COMPOSE_FILE

  该文件不随开源仓分发，需通过商务交付渠道获取。
  拿到后放到本目录（$(pwd)/${COMPOSE_FILE}）再重跑本脚本。

  索取方式：leeyisoft@qq.com
  仅评估/试用可先用最小演示环境（无需该文件）：
      docker compose -f docker-compose.demo.yml up -d"
fi

if [ ! -f "$POLICY_FILE" ]; then
  die "缺少 $POLICY_FILE

  该文件负责把严格 E2EE、频道和付费频道策略传入后端容器，不能省略。
  请从仓库恢复该文件后重跑本脚本。"
fi

# ── 0b) 交付来的 compose 是否是当前版本 ───────────────────────────────────────
# 这个文件是唯一不受版本控制的部署组件（走单独交付渠道），因此仓库里没有任何
# 东西能发现"渠道发了旧版本"。而它恰恰承载着三处**装不上或不安全**的改动：
#   · IMBOY_PASSWORD_SALT 透传    —— 缺了后端 {missing_required_config} 起不来
#   · IMBOY_PAYMENT_GATEWAY_ENABLED 透传 —— 缺了支付总开关失效，回落到旧死锁
#   · 127.0.0.1 端口绑定默认值    —— 缺了 PG/后端/Grafana 直接暴露公网
# 与其让买家在容器日志里刨根，不如在这里一次性列全。
missing=""
grep -q 'IMBOY_PASSWORD_SALT' "$COMPOSE_FILE" \
  || missing="$missing\n    · IMBOY_PASSWORD_SALT 未透传（后端会以 missing_required_config 启动失败）"
grep -q 'IMBOY_PAYMENT_GATEWAY_ENABLED' "$COMPOSE_FILE" \
  || missing="$missing\n    · IMBOY_PAYMENT_GATEWAY_ENABLED 未透传（支付总开关失效，无商户凭据将无法启动）"
grep -q '127\.0\.0\.1' "$COMPOSE_FILE" \
  || missing="$missing\n    · 缺少 127.0.0.1 端口绑定默认值（PostgreSQL / 后端 / Grafana 会直接暴露到公网）"

if [ -n "$missing" ]; then
  # shellcheck disable=SC2059
  die "$(printf "%s 版本过旧，缺少以下必需项：\n%b\n\n  请向交付渠道索取最新版本后重试。\n  索取方式：leeyisoft@qq.com" "$COMPOSE_FILE" "$missing")"
fi

# ── 1) 配置 .env（必须在 preflight 之前）──────────────────────────────────────
# preflight.sh 在 .env 不存在时会直接 exit 1。此前本脚本把 preflight 放在
# .env 生成之前，导致首次运行必然 die 在 preflight，下面整段生成逻辑是死代码。
if [ ! -f .env ]; then
  say "首次部署：生成 .env、随机密钥与 RSA 登录密钥对"
  [ -f .env.example ] || die "缺少 .env.example，仓库不完整"
  cp .env.example .env
  command -v openssl >/dev/null || die "需要 openssl 生成密钥，请先安装"

  for v in $SECRET_VARS_32; do set_var "$v" "$(openssl rand -hex 16)"; done
  for v in $SECRET_VARS_16; do set_var "$v" "$(openssl rand -hex 8)"; done
  for v in $SECRET_VARS_48; do set_var "$v" "$(openssl rand -hex 24)"; done

  # RSA 登录密钥对。.env 里配的是**容器内**路径 /opt/imboy/priv_runtime/keys/，
  # compose 把宿主机 ${DATA_DIR}/backend_priv 挂到 /opt/imboy/priv_runtime，
  # 所以宿主机上要写到 ${DATA_DIR}/backend_priv/keys/。
  data_dir="$(get_var DATA_DIR)"; data_dir="${data_dir:-./data}"
  keys_dir="$data_dir/backend_priv/keys"
  mkdir -p "$keys_dir"
  openssl genrsa -out "$keys_dir/login_rsa_priv.pem" 2048 2>/dev/null \
    || die "生成 RSA 私钥失败"
  openssl rsa -in "$keys_dir/login_rsa_priv.pem" -pubout \
    -out "$keys_dir/login_rsa_pub.pem" 2>/dev/null || die "导出 RSA 公钥失败"
  chmod 600 "$keys_dir/login_rsa_priv.pem"
  printf '  RSA 密钥对已生成：%s/\n' "$keys_dir"

  printf '\n\033[1;33m⚠️  密钥已全部自动生成。还需人工填写 3 项后重跑本脚本：\033[0m\n'
  printf '    编辑 %s/.env\n' "$(pwd)"
  printf '      API_DOMAIN     后端 API 域名（需已 DNS 解析到本机）\n'
  printf '      ADMIN_DOMAIN   管理后台域名（需已 DNS 解析到本机）\n'
  printf '      CERTBOT_EMAIL  证书到期通知邮箱\n'
  printf '\n    填好后再次执行：bash install.sh\n\n'
  exit 0
fi

# 必填项校验（占位符也算没填）
for v in $MANUAL_VARS; do
  cur="$(get_var "$v" || true)"
  case "$cur" in
    ""|*example.com|*CHANGE_ME*)
      die "$v 尚未填写真实值（当前: ${cur:-空}），编辑 .env 后重跑"
      ;;
  esac
done

# ── 2) 前置检查 ──────────────────────────────────────────────────────────────
say "前置检查 (preflight)"
bash preflight.sh --docker || die "preflight 未通过，修复 ERROR 后重跑"

# ── 3) 网络 + 启动 ───────────────────────────────────────────────────────────
say "创建网络并启动服务"
docker network create imboy-network 2>/dev/null || true
$COMPOSE up -d

# ── 4) 首次 TLS 证书签发 ─────────────────────────────────────────────────────
# 此前从不调用 init-letsencrypt.sh，证书永不签发，最后却照常打印
# "部署完成" 和 https:// 地址 —— 用户点进去是连不上的。
api="$(get_var API_DOMAIN)"
data_dir="$(get_var DATA_DIR)"; data_dir="${data_dir:-./data}"
if [ -f "$data_dir/certbot/conf/live/$api/fullchain.pem" ]; then
  say "TLS 证书已存在，跳过签发"
else
  say "签发 TLS 证书 (Let's Encrypt)"
  bash nginx/init-letsencrypt.sh || die "TLS 证书签发失败。

  常见原因：
    · 域名 A 记录未指向本机（Let's Encrypt 的 HTTP-01 校验会失败）
    · 80/443 端口未对公网开放，或被其他进程占用
    · 同一域名短时间内申请次数触发 Let's Encrypt 速率限制

  修复后重跑本脚本。服务已启动但 HTTPS 不可用，请勿对外提供访问。"
fi

# ── 5) 等待后端健康（最多 120s）──────────────────────────────────────────────
say "等待后端启动 (最多 120s)"
ok=0
for _ in $(seq 1 60); do
  if $COMPOSE logs imboy_backend 2>/dev/null | grep -q "started on port"; then ok=1; break; fi
  sleep 2
done
[ "$ok" = 1 ] || die "后端 120s 内未就绪，查日志：$COMPOSE logs imboy_backend"

# ── 6) 部署后自检（存在才跑）─────────────────────────────────────────────────
if [ -f ../scripts/sanity_check.sh ]; then
  say "部署后自检 (sanity_check)"
  bash ../scripts/sanity_check.sh || warn "sanity_check 有警告，请人工确认"
fi

# ── 7) 完成 ──────────────────────────────────────────────────────────────────
adm="$(get_var ADMIN_DOMAIN)"
cat <<EOF

✅ 部署完成 / Deployment done

   管理后台 / Admin : https://${adm}   （首次访问自动进入 /setup 创建超管）
   API / WebSocket  : https://${api}
   Grafana          : http://127.0.0.1:3000  (admin / 口令见 .env 的 GRAFANA_ADMIN_PASSWORD)
                      默认仅监听本机；远程访问请走 nginx 反代或 SSH 隧道

   服务状态 : $COMPOSE ps
   后端日志 : $COMPOSE logs -f imboy_backend
EOF
