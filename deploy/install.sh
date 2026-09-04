#!/usr/bin/env bash
# IMBoy 一键部署 / One-command deployment.
#
# 把 README 的"五步部署"收敛成一条命令：
#   生成 .env(自动随机密钥 + RSA 密钥对 + Garage 凭据) → preflight → 起服务
#   → 签发 TLS → 等健康 → sanity → (可选)创建超管 → 打印访问地址 + Release Identity
#
#   cd imboy/deploy && bash install.sh
#
# 选项 / Options（--help 查看完整说明）：
#   --edition community|business  部署版本，默认 community
#   --admin-phone / --admin-password  装完后在容器内经 imboy_ctl 创建超管
#   --yes, -y                      跳过所有确认（含 Docker 安装确认）
#
# 全参数一行示例（无浏览器纯脚本部署）：
#   bash install.sh --edition community --admin-phone 13800138000 \
#          --admin-password 'S3curePass2026' --yes
#
# 设计：客户先从 .env.example 准备 .env；本脚本补齐内部密钥、RSA、回调 URL，
#      再完成 preflight、服务启动、TLS、健康检查和初始化。
# 幂等：密钥已生成（非占位符）不覆盖；证书已签发则跳过；
#      重复运行只会确保服务在跑。
set -euo pipefail
cd "$(dirname "$0")"

say()  { printf '\n\033[1;36m==> %s\033[0m\n' "$*"; }
die()  { printf '\n\033[1;31m❌ %s\033[0m\n' "$*" >&2; exit 1; }
warn() { printf '\033[1;33m⚠️  %s\033[0m\n' "$*"; }

# 确认提示：--yes 全局跳过；默认拒绝（直接回车 = N）
confirm() {
  [ "$ASSUME_YES" = 1 ] && return 0
  local answer=""
  printf '%s [y/N] ' "$1"
  read -r answer || true
  case "$answer" in y|Y|yes|YES) return 0 ;; *) return 1 ;; esac
}

usage() {
  cat <<'EOF'
IMBoy 一键部署 / Usage: bash install.sh [选项]

选项:
  --edition community|business  部署版本（默认 community）
                                community: 随仓分发的 docker-compose.community.yml，
                                  内置 Garage 对象存储（附件上传开箱可用），支付网关固定关闭
                                business : 商务交付的 docker-compose.prod.yml
                                  + sales-policy overlay（缺文件时提示商务索取）
  --admin-phone <手机号>         安装完成后在 backend 容器内经 imboy_ctl adm create
                                创建超管（无需浏览器走 /setup 向导）
  --admin-password <明文>        与 --admin-phone 成对使用；8-64 位且须含字母和数字
  --yes, -y                     跳过所有确认（含 Docker 缺失时 get.docker.com 安装确认）
  --help, -h                    显示本帮助

示例:
  bash install.sh                                          # 社区版（默认），装完走网页 /setup 建超管
  bash install.sh --edition business                       # 商务版（需已拿到 prod.yml）
  bash install.sh --admin-phone 13800138000 \
         --admin-password 'S3curePass2026'                 # 社区版 + CLI 建超管（无浏览器部署）
  bash install.sh --edition community --admin-phone 13800138000 \
         --admin-password 'S3curePass2026' --yes           # 全参数一行（自动化/CI）

客户先复制 .env.example 为 .env 并填写域名与所启用的第三方服务配置。
内部随机密钥、RSA 密钥对和支付回调 URL 由安装器自动补齐。
EOF
}

# ── 参数解析 ─────────────────────────────────────────────────────────────────
EDITION="community"
ASSUME_YES=0
ADMIN_PHONE=""
ADMIN_PASSWORD=""

while [ $# -gt 0 ]; do
  case "$1" in
    --edition)
      [ $# -ge 2 ] || die "--edition 需要值：community|business"
      EDITION="$2"; shift 2 ;;
    --edition=*)
      EDITION="${1#*=}"; shift ;;
    -y|--yes)
      ASSUME_YES=1; shift ;;
    --admin-phone)
      [ $# -ge 2 ] || die "--admin-phone 需要值（手机号）"
      ADMIN_PHONE="$2"; shift 2 ;;
    --admin-phone=*)
      ADMIN_PHONE="${1#*=}"; shift ;;
    --admin-password)
      [ $# -ge 2 ] || die "--admin-password 需要值（明文密码）"
      ADMIN_PASSWORD="$2"; shift 2 ;;
    --admin-password=*)
      ADMIN_PASSWORD="${1#*=}"; shift ;;
    -h|--help)
      usage; exit 0 ;;
    *)
      die "未知参数：$1（--help 查看用法）" ;;
  esac
done

case "$EDITION" in
  community)
    COMPOSE_FILE="docker-compose.community.yml"
    POLICY_FILE=""
    ;;
  business)
    COMPOSE_FILE="docker-compose.prod.yml"
    POLICY_FILE="docker-compose-sales-policy.yml"
    ;;
  *)
    die "--edition 仅支持 community|business（当前: ${EDITION}）" ;;
esac

# 超管参数成对 + 本地前置校验（规则与 imboy_ctl adm create 一致，尽早失败
# 而不是等整个栈起来之后才发现密码不合规）
if { [ -n "$ADMIN_PHONE" ] && [ -z "$ADMIN_PASSWORD" ]; } \
   || { [ -z "$ADMIN_PHONE" ] && [ -n "$ADMIN_PASSWORD" ]; }; then
  die "--admin-phone 与 --admin-password 必须成对使用"
fi
if [ -n "$ADMIN_PASSWORD" ]; then
  if [ "${#ADMIN_PASSWORD}" -lt 8 ] || [ "${#ADMIN_PASSWORD}" -gt 64 ] \
     || ! printf '%s' "$ADMIN_PASSWORD" | grep -q '[A-Za-z]' \
     || ! printf '%s' "$ADMIN_PASSWORD" | grep -q '[0-9]'; then
    die "--admin-password 强度不足：需 8-64 位且同时包含字母和数字"
  fi
fi

# 需要随机密钥的字段（域名与邮箱不在内，必须人工填）
# 长度约束来自 preflight.sh 与后端启动校验：
#   *_KEY / IMBOY_SOLIDIFIED_KEY 需 32 字节 → rand -hex 16 得 32 个十六进制字符
#   IMBOY_SOLIDIFIED_KEY_IV 需 16 字节      → rand -hex 8
#   LIVEKIT_API_SECRET 需 ≥32 字符          → rand -hex 24 得 48 字符，留余量
SECRET_VARS_32="POSTGRES_PASSWORD JWT_KEY POSTGRE_AES_KEY ADM_COOKIE_SECRET GRAFANA_ADMIN_PASSWORD IMBOY_SOLIDIFIED_KEY IMBOY_PASSWORD_SALT LIVEKIT_API_KEY"
SECRET_VARS_16="IMBOY_SOLIDIFIED_KEY_IV"
SECRET_VARS_48="LIVEKIT_API_SECRET"
UPTRACE_SECRET_VARS="UPTRACE_SERVICE_SECRET UPTRACE_PG_PASSWORD UPTRACE_CLICKHOUSE_PASSWORD UPTRACE_REDIS_PASSWORD UPTRACE_ADMIN_PASSWORD UPTRACE_PROJECT_TOKEN"

# 必须人工填写的字段（机器无从知晓）
MANUAL_VARS="API_DOMAIN ADMIN_DOMAIN CERTBOT_EMAIL"

# 替换 .env 中 KEY=... 行（按字段名精确匹配，值含特殊字符也安全；BSD/GNU 通用）。
set_var() {
  local key="$1" val="$2" tmp
  tmp="$(mktemp)"
  awk -v k="$key" -v v="$val" 'BEGIN{FS=OFS="="} $1==k{print k"="v; found=1; next} {print} END{if(!found) print k"="v}' .env >"$tmp"
  mv "$tmp" .env
}
get_var() { grep -E "^$1=" .env | head -1 | cut -d= -f2-; }

# 值缺失或仍是 .env.example 占位符时才写入；已有真实值不覆盖（幂等约定）。
ensure_secret() {
  local key="$1" val="$2" cur
  cur="$(get_var "$key" || true)"
  case "$cur" in
    ""|*CHANGE_ME*) set_var "$key" "$val" ;;
  esac
}

ensure_value() {
  local key="$1" val="$2" cur
  cur="$(get_var "$key" || true)"
  [ -n "$cur" ] || set_var "$key" "$val"
}

is_true() {
  case "$(printf '%s' "$1" | tr '[:upper:]' '[:lower:]' | tr -d '[:space:]')" in
    true|1) return 0 ;;
    *) return 1 ;;
  esac
}

ensure_rsa_keys() {
  local data_dir keys_dir private_generated=0
  data_dir="$(get_var DATA_DIR)"; data_dir="${data_dir:-./data}"
  keys_dir="$data_dir/backend_priv/keys"
  mkdir -p "$keys_dir"
  if [ ! -s "$keys_dir/login_rsa_priv.pem" ]; then
    openssl genrsa -out "$keys_dir/login_rsa_priv.pem" 2048 2>/dev/null \
      || die "生成 RSA 私钥失败"
    private_generated=1
  fi
  if [ "$private_generated" = 1 ] || [ ! -s "$keys_dir/login_rsa_pub.pem" ]; then
    openssl rsa -in "$keys_dir/login_rsa_priv.pem" -pubout \
      -out "$keys_dir/login_rsa_pub.pem" 2>/dev/null || die "导出 RSA 公钥失败"
  fi
  chmod 600 "$keys_dir/login_rsa_priv.pem"
}

# Garage 对象存储凭据（社区版 compose 内置 garage 核心服务）：.env 是唯一真源，
# 凭据经 GARAGE_DEFAULT_ACCESS_KEY/SECRET_KEY env 注入 garage 容器，由其
# --single-node --default-bucket 启动参数完成幂等 init（key/bucket 已存在则复用）。
# access key 用 Garage 生态惯例 GK 前缀；RPC secret 一旦产生数据不可更换。
gen_garage_secrets() {
  command -v openssl >/dev/null || die "需要 openssl 生成密钥，请先安装"
  ensure_secret GARAGE_RPC_SECRET       "$(openssl rand -hex 32)"
  ensure_secret IMBOY_GARAGE_ACCESS_KEY "GK$(openssl rand -hex 16)"
  ensure_secret IMBOY_GARAGE_SECRET_KEY "$(openssl rand -hex 32)"
}

# ── 0) Docker / Compose v2 就绪（缺失时确认式引导 get.docker.com）──────────
ensure_docker() {
  local missing_what=""
  command -v docker >/dev/null 2>&1 || missing_what="docker 命令不存在"
  if [ -z "$missing_what" ] && ! docker compose version >/dev/null 2>&1; then
    missing_what="docker compose v2 插件不可用"
  fi

  if [ -n "$missing_what" ]; then
    say "检测到 ${missing_what}，需要先安装 Docker（含 Compose v2 插件）"
    printf '  将执行 Docker 官方安装脚本：\n'
    printf '    curl -fsSL https://get.docker.com | sh\n'
    if confirm "现在安装 Docker?"; then
      curl -fsSL https://get.docker.com | sh \
        || die "Docker 安装失败，请人工排查后重跑本脚本"
    else
      die "已取消安装。请自行安装 Docker 24+ 与 Compose v2 插件后重跑本脚本"
    fi
    # 安装后复检（get.docker.com 对已有安装会补齐/升级 compose 插件）
    command -v docker >/dev/null 2>&1 \
      || die "docker 仍不可用，请人工排查"
    docker compose version >/dev/null 2>&1 \
      || die "docker compose 插件仍不可用（Debian 系可试：apt-get install -y docker-compose-plugin）"
  fi

  # daemon 运行状态（非 root 用户未入 docker 组时同样连不上，一并提示）
  docker info >/dev/null 2>&1 || die "Docker daemon 未运行或当前用户无权限。
  启动：sudo systemctl enable --now docker
  权限：sudo usermod -aG docker \$USER 后重新登录（或直接用 root 部署）"
}
ensure_docker

# ── 0a) compose 文件存在性（按 edition）────────────────────────────────────
if [ ! -f "$COMPOSE_FILE" ]; then
  if [ "$EDITION" = "business" ]; then
    # 商务版 compose 按商务决策走单独交付渠道，不随开源仓分发（.gitignore 精确排除）。
    # 不检查的话，第一次用到它是在 `$COMPOSE up -d`，docker 只会吐一句
    # "no configuration file provided"，非开发者完全无从判断发生了什么。
    die "缺少 $COMPOSE_FILE

  该文件不随开源仓分发，需通过商务交付渠道获取。
  拿到后放到本目录（$(pwd)/${COMPOSE_FILE}）再重跑本脚本。

  索取方式：leeyisoft@qq.com
  仅评估/试用可先用最小演示环境（无需该文件）：
      docker compose -f docker-compose.demo.yml up -d
  或改用随仓分发的社区版：bash install.sh --edition community"
  else
    die "缺少 $COMPOSE_FILE —— 社区版 compose 随仓分发，本目录没有说明仓库不完整，
  请重新 git clone 本仓库后重试"
  fi
fi

if [ "$EDITION" = "business" ]; then
  if [ ! -f "$POLICY_FILE" ]; then
    die "缺少 $POLICY_FILE

  该文件负责把严格 E2EE、频道和付费频道策略传入后端容器，不能省略。
  请从仓库恢复该文件后重跑本脚本。"
  fi

  # ── 0b) 交付来的 compose 是否是当前版本 ───────────────────────────────────
  # 这个文件是唯一不受版本控制的部署组件（走单独交付渠道），因此仓库里没有任何
  # 东西能发现"渠道发了旧版本"。而它恰恰承载着三处**装不上或不安全**的改动：
  #   · IMBOY_PASSWORD_SALT 透传    —— 缺了后端 {missing_required_config} 起不来
  #   · IMBOY_PAYMENT_GATEWAY_ENABLED 透传 —— 缺了支付总开关失效，回落到旧死锁
  #   · 127.0.0.1 端口绑定默认值    —— 缺了 PG/后端/Grafana 直接暴露公网
  #   · Garage、SMTP、SMS 透传       —— 缺了附件或客户第三方配置不会生效
  # 与其让买家在容器日志里刨根，不如在这里一次性列全。
  missing=""
  grep -q 'IMBOY_PASSWORD_SALT' "$COMPOSE_FILE" \
    || missing="$missing\n    · IMBOY_PASSWORD_SALT 未透传（后端会以 missing_required_config 启动失败）"
  grep -q 'IMBOY_PAYMENT_GATEWAY_ENABLED' "$COMPOSE_FILE" \
    || missing="$missing\n    · IMBOY_PAYMENT_GATEWAY_ENABLED 未透传（支付总开关失效，无商户凭据将无法启动）"
  grep -q '127\.0\.0\.1' "$COMPOSE_FILE" \
    || missing="$missing\n    · 缺少 127.0.0.1 端口绑定默认值（PostgreSQL / 后端 / Grafana 会直接暴露到公网）"
  grep -qE '^[[:space:]]+garage:' "$COMPOSE_FILE" \
    || missing="$missing\n    · 缺少内置 Garage 服务（附件上传不可用）"
  grep -q 'IMBOY_SMTP_FROM' "$COMPOSE_FILE" \
    || missing="$missing\n    · SMTP 配置未完整透传"
  grep -q 'IMBOY_SMS_PLATFORM' "$COMPOSE_FILE" \
    || missing="$missing\n    · 短信平台配置未透传"
  grep -q 'S3_UPSTREAM' "$COMPOSE_FILE" \
    || missing="$missing\n    · Nginx 未配置 Garage S3 upstream"

  if [ -n "$missing" ]; then
    # shellcheck disable=SC2059
    die "$(printf "%s 版本过旧，缺少以下必需项：\n%b\n\n  请向交付渠道索取最新版本后重试。\n  索取方式：leeyisoft@qq.com" "$COMPOSE_FILE" "$missing")"
  fi
fi

# ── 1) 配置 .env（必须在 preflight 之前）──────────────────────────────────────
if [ ! -f .env ]; then
  say "生成客户配置模板"
  [ -f .env.example ] || die "缺少 .env.example，仓库不完整"
  cp .env.example .env
  chmod 600 .env
  printf '\n\033[1;33m⚠️  已生成 .env 模板。请先填写客户配置，再执行一次安装命令：\033[0m\n'
  printf '    编辑 %s/.env\n' "$(pwd)"
  printf '      API_DOMAIN     后端 API 域名（需已 DNS 解析到本机）\n'
  printf '      ADMIN_DOMAIN   管理后台域名（需已 DNS 解析到本机）\n'
  printf '      CERTBOT_EMAIL  证书到期通知邮箱\n'
  printf '      第三方服务     仅填写实际启用的支付、短信、SMTP、Uptrace 配置\n'
  printf '\n    填好后执行：bash install.sh --edition %s\n\n' "$EDITION"
  exit 0
fi

command -v openssl >/dev/null || die "需要 openssl 生成密钥，请先安装"
chmod 600 .env

# 自动补齐全部内部密钥。客户预先 cp/edit .env 后，只需执行本脚本一次。
for v in $SECRET_VARS_32; do ensure_secret "$v" "$(openssl rand -hex 16)"; done
for v in $SECRET_VARS_16; do ensure_secret "$v" "$(openssl rand -hex 8)"; done
for v in $SECRET_VARS_48; do ensure_secret "$v" "$(openssl rand -hex 24)"; done
gen_garage_secrets
ensure_rsa_keys

UPTRACE_ENABLED_VALUE="$(get_var UPTRACE_ENABLED || true)"
if is_true "$UPTRACE_ENABLED_VALUE"; then
  [ -f docker-compose.uptrace.yml ] || die "UPTRACE_ENABLED=true 但缺少 docker-compose.uptrace.yml"
  MANUAL_VARS="$MANUAL_VARS UPTRACE_DOMAIN UPTRACE_ADMIN_EMAIL"
  for v in $UPTRACE_SECRET_VARS; do ensure_secret "$v" "$(openssl rand -hex 24)"; done
fi

# 支付开启时，回调地址跟随 API_DOMAIN；已有客户自定义值不覆盖。
if is_true "$(get_var IMBOY_PAYMENT_GATEWAY_ENABLED || true)"; then
  api_domain_for_callback="$(get_var API_DOMAIN || true)"
  case "$api_domain_for_callback" in
    ""|*example.com|*CHANGE_ME*) ;;
    *)
      ensure_value IMBOY_WECHAT_NOTIFY_URL "https://${api_domain_for_callback}/api/v1/payment/callback/wechat"
      ensure_value IMBOY_ALIPAY_NOTIFY_URL "https://${api_domain_for_callback}/api/v1/payment/callback/alipay"
      ;;
  esac
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

# Compose 组合是单一真源，后续启动、TLS 与 sanity 使用完全相同的文件集合。
COMPOSE_ARGS=(-f "$COMPOSE_FILE")
COMPOSE_FILES="$COMPOSE_FILE"
if [ -n "$POLICY_FILE" ]; then
  COMPOSE_ARGS+=(-f "$POLICY_FILE")
  COMPOSE_FILES="$COMPOSE_FILES $POLICY_FILE"
fi
if is_true "$UPTRACE_ENABLED_VALUE"; then
  COMPOSE_ARGS+=(-f docker-compose.uptrace.yml)
  COMPOSE_FILES="$COMPOSE_FILES docker-compose.uptrace.yml"
fi
compose() { docker compose "${COMPOSE_ARGS[@]}" "$@"; }
COMPOSE_DISPLAY="docker compose"
for compose_file in $COMPOSE_FILES; do COMPOSE_DISPLAY="$COMPOSE_DISPLAY -f $compose_file"; done

# ── 2) 前置检查 ──────────────────────────────────────────────────────────────
say "前置检查 (preflight, edition=$EDITION)"
bash preflight.sh --docker --edition "$EDITION" || die "preflight 未通过，修复 ERROR 后重跑"

# ── 3) 镜像 + 网络 + 启动 ────────────────────────────────────────────────────
# 先拉齐全部缺失镜像再启动，避免私有/缺失镜像导致机器进入半安装状态。
say "检查并拉取缺失镜像"
compose pull --policy missing || die "镜像拉取失败，尚未启动任何服务。

  请确认：
    · IMBoy 发布镜像标签存在且客户具备拉取权限
    · 私有 GHCR 已由客户在部署机完成 docker login ghcr.io
    · Docker Hub 网络与登录状态正常"

say "创建网络并启动服务"
docker network create imboy-network 2>/dev/null || true
compose up -d

# ── 4) 首次 TLS 证书签发 ─────────────────────────────────────────────────────
# 此前从不调用 init-letsencrypt.sh，证书永不签发，最后却照常打印
# "部署完成" 和 https:// 地址 —— 用户点进去是连不上的。
api="$(get_var API_DOMAIN)"
say "校验并签发 TLS 证书 (Let's Encrypt)"
COMPOSE_FILES="$COMPOSE_FILES" bash nginx/init-letsencrypt.sh || die "TLS 证书签发失败。

  常见原因：
    · 域名 A 记录未指向本机（Let's Encrypt 的 HTTP-01 校验会失败）
    · 80/443 端口未对公网开放，或被其他进程占用
    · 同一域名短时间内申请次数触发 Let's Encrypt 速率限制

  修复后重跑本脚本。服务已启动但 HTTPS 不可用，请勿对外提供访问。"

# ── 5) 等待后端健康（最多 120s）──────────────────────────────────────────────
# 不能以日志文案判定就绪：后端不承诺输出固定的 "started on port" 文本，
# 且端口可监听也不代表 PostgreSQL 已可服务。与 Docker healthcheck/蓝绿部署
# 保持同一语义：只有 /healthz 明确返回 status=ok 才继续。
say "等待后端健康（/healthz，最多 120s）"
ok=0
for _ in $(seq 1 60); do
  if curl -fsS --max-time 3 "http://127.0.0.1:${BACKEND_PORT:-9800}/healthz" 2>/dev/null \
      | grep -q '"status":"ok"'; then ok=1; break; fi
  sleep 2
done
[ "$ok" = 1 ] || die "后端 120s 内未通过 /healthz，查日志：$COMPOSE_DISPLAY logs imboy_backend"

# ── 6) 部署后自检（存在才跑）─────────────────────────────────────────────────
if [ -f ../scripts/sanity_check.sh ]; then
  say "部署后自检 (sanity_check)"
  SANITY_ARGS=()
  for compose_file in $COMPOSE_FILES; do SANITY_ARGS+=(--compose-file "$compose_file"); done
  bash ../scripts/sanity_check.sh "${SANITY_ARGS[@]}" \
    || die "sanity_check 未通过；部署未达到可交付状态，请按上方 ERROR 排查"
fi

# ── 7) 超管创建（可选：--admin-phone/--admin-password 传入时）───────────────
# 部署机只有 Docker 没有 Erlang，imboy_ctl（escript）在 backend 容器内执行：
# 镜像 runtime 阶段已 COPY imboy_ctl，escript 用镜像自带的 ERTS（include_erts）。
# 节点名/cookie 来自镜像 release 的 vm.args（config/vm.args：
#   -name imboy_dev@127.0.0.1 / -setcookie imboycookie）。
# 明文密码经 env 传入容器（不落宿主机命令行），容器内再作为 argv 交给 escript。
admin_id=""
if [ -n "$ADMIN_PHONE" ]; then
  say "创建超级管理员 (imboy_ctl adm create)"
  # shellcheck disable=SC2016  # 手机号/密码必须在容器内展开（不落宿主机命令行与进程表）
  ctl_out="$(IMBOY_CTL_NODE='imboy_dev@127.0.0.1' \
      IMBOY_CTL_COOKIE='imboycookie' \
      IMBOY_CTL_PHONE="$ADMIN_PHONE" \
      IMBOY_CTL_PASSWORD="$ADMIN_PASSWORD" \
      compose exec -T \
      -e IMBOY_CTL_NODE -e IMBOY_CTL_COOKIE -e IMBOY_CTL_PHONE -e IMBOY_CTL_PASSWORD \
      imboy_backend \
      sh -c 'exec /opt/imboy/erts-*/bin/escript /opt/imboy/bin/imboy_ctl adm create --phone "$IMBOY_CTL_PHONE" --password "$IMBOY_CTL_PASSWORD"' 2>&1)" \
    || die "超管创建失败（安装中止）：$ctl_out"
  admin_id="$(printf '%s\n' "$ctl_out" | sed -n 's/^ADMIN_ID=//p' | head -1)"
  printf '  超管已创建：%s\n' \
    "$(printf '%s\n' "$ctl_out" | grep -E '^(ADMIN_ID|CREATED)=' | tr '\n' ' ')"
fi

# ── 8) Release Identity 三元组 + 完成 ───────────────────────────────────────
# 核验"装的正是被验证过的那一个"（Golden Gates 计划 §3.1）。
imboy_version="$(get_var IMBOY_VERSION)"; imboy_version="${imboy_version:-unknown}"

# git SHA：部署目录来自 git clone 才能取到（商务散件交付 / 解包目录取不到）
git_sha="unknown"; git_note=""
if command -v git >/dev/null 2>&1 \
   && git -C .. rev-parse --is-inside-work-tree >/dev/null 2>&1; then
  git_sha="$(git -C .. rev-parse HEAD 2>/dev/null || echo unknown)"
else
  git_note="（部署目录非 git clone，无法取 git SHA）"
fi

# 镜像 digest：backend 运行镜像的 RepoDigest。只有从 registry 拉取的镜像才有
# RepoDigests —— 本地 docker build 的镜像没有，此时输出 unknown 并注明。
image_digest="unknown"; digest_note=""
backend_image_id="$(compose images -q imboy_backend 2>/dev/null | head -1 || true)"
if [ -n "$backend_image_id" ]; then
  repo_digest="$(docker inspect --format '{{range .RepoDigests}}{{println .}}{{end}}' \
                   "$backend_image_id" 2>/dev/null | head -1 || true)"
  case "$repo_digest" in
    *@sha256:*) image_digest="${repo_digest#*@}" ;;
    *) digest_note="（本地构建镜像无 RepoDigests，无法核验）" ;;
  esac
else
  digest_note="（取不到 backend 镜像信息）"
fi
git_line="IMBOY_GIT_SHA=${git_sha}"
if [ -n "$git_note" ]; then git_line="$git_line  $git_note"; fi
digest_line="IMBOY_IMAGE_DIGEST=${image_digest}"
if [ -n "$digest_note" ]; then digest_line="$digest_line  $digest_note"; fi

adm="$(get_var ADMIN_DOMAIN)"
if [ -n "$ADMIN_PHONE" ]; then
  admin_hint="（超管已创建，ADMIN_ID=${admin_id:-见上方输出}，可直接登录）"
else
  admin_hint="（首次访问自动进入 /setup 创建超管）"
fi

cat <<EOF

✅ 部署完成 / Deployment done（edition: ${EDITION}）

   ── Release Identity（核验装的正是被验证过的那一个）──
   IMBOY_VERSION=${imboy_version}
   ${git_line}
   ${digest_line}

   管理后台 / Admin : https://${adm}   ${admin_hint}
   API / WebSocket  : https://${api}
   LiveKit 信令     : wss://${api}/livekit  (媒体端口 TCP 7881 / UDP 50000-50200)
   Garage S3        : https://${api}/s3  (3900 不暴露公网)
EOF
if is_true "$UPTRACE_ENABLED_VALUE"; then
  cat <<EOF
   Uptrace          : https://$(get_var UPTRACE_DOMAIN)  (管理员口令见 .env)
                      仅采集 imboy_backend /metrics，不采集业务日志
EOF
else
  cat <<EOF
   Uptrace          : 未启用（在 .env 设置 UPTRACE_ENABLED=true 后重跑）
EOF
fi
if [ "$EDITION" = "business" ]; then
  cat <<EOF
   Grafana          : http://127.0.0.1:3000  (admin / 口令见 .env 的 GRAFANA_ADMIN_PASSWORD)
                      默认仅监听本机；远程访问请走 nginx 反代或 SSH 隧道
EOF
else
  cat <<EOF
   Grafana/监控栈   : 默认未启用（monitoring profile）。需要时：
                      $COMPOSE_DISPLAY --profile monitoring up -d
EOF
fi
cat <<EOF

   升级 / Upgrade   : 版本历史与每版升级说明见仓库 RELEASES.md；
                      蓝绿升级用 scripts/deploy.sh（零停机切换）

   服务状态 : $COMPOSE_DISPLAY ps
   后端日志 : $COMPOSE_DISPLAY logs -f imboy_backend
EOF
