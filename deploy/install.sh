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
#   --site-compose                使用现场 Compose 配置 / Use site Compose
#   --admin-phone / --admin-password  装完后在容器内经 imboy_ctl 创建超管
#   --yes, -y                      跳过所有确认（含 Docker 安装确认）
#
# 全参数一行示例（无浏览器纯脚本部署）：
#   bash install.sh --admin-phone 13800138000 \
#          --admin-password 'S3curePass2026' --yes
#
# 设计：客户先从 .env.example 准备 .env；本脚本补齐内部密钥、RSA、回调 URL，
#      再完成 preflight、服务启动、TLS、健康检查和初始化。
# 幂等：密钥已生成（非占位符）不覆盖；证书已签发则跳过；
#      重复运行只会确保服务在跑。
set +x
set +v
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
  --site-compose                 使用 deploy/docker-compose.prod.yml
  --init-cleanup-db              初始化所缺的清理账号（根 Compose）
  --admin-phone <手机号>         创建超管，与 --admin-password 成对使用
  --admin-password <密码>        8-64 位且须含字母和数字
  --yes, -y                     跳过确认
  --help, -h                    显示帮助

准备 .env 后运行 bash install.sh，默认使用仓根 Compose。
Prepare .env and run bash install.sh to use the root Compose file.
EOF
}

# ── 参数解析 ─────────────────────────────────────────────────────────────────
UNIFIED_COMPOSE=1
service_prefix=
ASSUME_YES=0
ADMIN_PHONE=""
ADMIN_PASSWORD=""
INIT_CLEANUP_DB=0
COMPOSE_EXTRA_FILES=()

while [ $# -gt 0 ]; do
  case "$1" in
    --unified-compose)
      UNIFIED_COMPOSE=1; shift ;;
    --site-compose)
      UNIFIED_COMPOSE=0; shift ;;
    -y|--yes)
      ASSUME_YES=1; shift ;;
    --init-cleanup-db)
      INIT_CLEANUP_DB=1; shift ;;
    --compose-file)
      [ $# -ge 2 ] || die "--compose-file 需要 overlay 路径"
      COMPOSE_EXTRA_FILES+=("$2"); shift 2 ;;
    --compose-file=*)
      COMPOSE_EXTRA_FILES+=("${1#*=}"); shift ;;
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

POLICY_DIR=""
if [ "$UNIFIED_COMPOSE" = 1 ]; then
  COMPOSE_FILE=../docker-compose.yml
  service_prefix=prod_
else
  COMPOSE_FILE=docker-compose.prod.yml
  [ "$INIT_CLEANUP_DB" = 0 ] || die "现场数据库须由 DBA 预先开通清理账号"
  [ "${#COMPOSE_EXTRA_FILES[@]}" = 0 ] || die "--compose-file 仅用于根 Compose"
fi

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

# 与下游 COMPOSE_FILES 的空格分隔合同一致；在任何安装副作用前校验。
# Community overlays keep the base file; business policy composition is unchanged.
for ((compose_extra_index=0; compose_extra_index<${#COMPOSE_EXTRA_FILES[@]}; compose_extra_index++)); do
  compose_extra="${COMPOSE_EXTRA_FILES[$compose_extra_index]}"
  case "$compose_extra" in
    ""|*[[:space:]]*) die "--compose-file 路径不能为空或包含空白" ;;
    /*) ;;
    *) compose_extra="$PWD/$compose_extra" ;;
  esac
  [ -f "$compose_extra" ] && [ -r "$compose_extra" ] \
    || die "--compose-file 文件不存在或不可读：$compose_extra"
  COMPOSE_EXTRA_FILES[compose_extra_index]="$compose_extra"
done

# 客户安装/验收仅依赖 Shell 工具；缺依赖或验收文件时在任何安装副作用前失败。
# Fail before changing configuration or installing/starting Docker.
command -v jq >/dev/null 2>&1 || die "需要 jq 校验 Compose 配置，请先安装 jq（无需 Python）"
[ -f ../scripts/sanity_check.sh ] && [ -r ../scripts/sanity_check.sh ] \
  || die "缺少或无法读取 ../scripts/sanity_check.sh；交付包不完整，服务尚未启动"
[ -r ../scripts/gen_login_rsa_keys.sh ] \
  || die "缺少登录 RSA 密钥管理脚本 / Missing login RSA key initializer"
[ -f prepare-backend-runtime.sh ] && [ -r prepare-backend-runtime.sh ] \
  || die "缺少 prepare-backend-runtime.sh；交付包不完整，服务尚未启动"
if [ "$UNIFIED_COMPOSE" = 1 ]; then
  for cleanup_file in init-cleanup-db.sh cleanup-db/bootstrap.sql cleanup-db/verify.sql; do
    [ -f "$cleanup_file" ] && [ -r "$cleanup_file" ] \
      || die "缺少或无法读取 ${cleanup_file}；交付包不完整，服务尚未启动"
  done
fi

# 需要随机密钥的字段（域名与邮箱不在内，必须人工填）
# 长度约束来自 preflight.sh 与后端启动校验：
#   *_KEY / IMBOY_SOLIDIFIED_KEY 需 32 字节 → rand -hex 16 得 32 个十六进制字符
#   IMBOY_SOLIDIFIED_KEY_IV 需 16 字节      → rand -hex 8
#   LIVEKIT_API_SECRET 需 ≥32 字符          → rand -hex 24 得 48 字符，留余量
SECRET_VARS_32="IMBOY_DIRECT_CLEANUP_PG_PASSWORD POSTGRES_PASSWORD JWT_KEY POSTGRE_AES_KEY ADM_COOKIE_SECRET IMBOY_EDGE_INTERNAL_TOKEN GRAFANA_ADMIN_PASSWORD IMBOY_SOLIDIFIED_KEY IMBOY_PASSWORD_SALT LIVEKIT_API_KEY"
SECRET_VARS_16="IMBOY_SOLIDIFIED_KEY_IV"
SECRET_VARS_48="LIVEKIT_API_SECRET"
UPTRACE_SECRET_VARS="UPTRACE_SERVICE_SECRET UPTRACE_PG_PASSWORD UPTRACE_CLICKHOUSE_PASSWORD UPTRACE_REDIS_PASSWORD UPTRACE_ADMIN_PASSWORD UPTRACE_PROJECT_TOKEN"

# 必须人工填写的字段（机器无从知晓）
# CS_WIDGET_DOMAIN 是客服托管 Widget 第三域（CSD-DEP-01）：标准部署三域必填，
# fail-closed —— 证书签发、Nginx vhost、preflight 都依赖它。
# RTC_DOMAIN/TURN_DOMAIN 是 LiveKit 两域（LK-DEP-01）：信令 WSS 与 embedded
# TURN，证书签发（init-letsencrypt.sh）与 preflight 同样强依赖。
MANUAL_VARS="API_DOMAIN ADMIN_DOMAIN CS_WIDGET_DOMAIN CERTBOT_EMAIL"

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

# 非负整数 + 区间校验（R0 容量参数用；空串/非数字/越界均假）
is_uint_in_range() { # <value> <min> <max>
  case "$1" in
    ''|*[!0-9]*) return 1 ;;
  esac
  [ "$1" -ge "$2" ] && [ "$1" -le "$3" ]
}

# ── R0 容量参数（P1-1；设计 docs/engineering/r0-capacity-remediation-design.md §6.1）──
# IMBOY_MAX_PROCESSES/PORTS/SCHEDULERS 三项确定性重算为 IMBOY_ERL_FLAGS，
# compose 以 ERL_FLAGS: ${IMBOY_ERL_FLAGS:-} 透传后端容器（实测 ERL_FLAGS
# 覆盖 args_file/vm.args，无需重建镜像即可调整）。
# 语义约束（属主批准）：非法值 die、不落 .env、不静默回退为无限制；
# IMBOY_ERL_FLAGS 每次运行由三个 MAX_* 重算（单一真源，手改会被下次
# 安装覆盖；要临时回退旧默认值即设 IMBOY_MAX_PROCESSES=20480 等）。
ensure_capacity_vars() {
  ensure_value IMBOY_MAX_PROCESSES "65536"
  ensure_value IMBOY_MAX_PORTS "65536"
  ensure_value IMBOY_MAX_SCHEDULERS ""
  ensure_value IMBOY_MAX_WS_CONNECTIONS "20000"

  local procs ports scheds ws flags
  procs="$(get_var IMBOY_MAX_PROCESSES)"
  ports="$(get_var IMBOY_MAX_PORTS)"
  scheds="$(get_var IMBOY_MAX_SCHEDULERS)"
  ws="$(get_var IMBOY_MAX_WS_CONNECTIONS)"

  # 合法域 = OTP 官方文档 +P/+Q 范围 [1024, 134217727]
  is_uint_in_range "$procs" 1024 134217727 \
    || die "IMBOY_MAX_PROCESSES=$procs 非法（需整数 ∈ [1024,134217727]，OTP +P 官方域）；修正 .env 后重跑"
  is_uint_in_range "$ports" 1024 134217727 \
    || die "IMBOY_MAX_PORTS=$ports 非法（需整数 ∈ [1024,134217727]，OTP +Q 官方域）；修正 .env 后重跑"
  is_uint_in_range "$ws" 1 1000000 \
    || die "IMBOY_MAX_WS_CONNECTIONS=$ws 非法（需整数 ∈ [1,1000000]）；后端启动期还会再做范围校验"
  case "$scheds" in
    "") ;;  # 空 = 沿用镜像 vm.args 的 +S（D3/S-B：不擅改默认调度器数）
    *) is_uint_in_range "$scheds" 1 1024 \
         || die "IMBOY_MAX_SCHEDULERS=$scheds 非法（留空或整数 ∈ [1,1024]）" ;;
  esac

  flags="+P $procs +Q $ports"
  [ -n "$scheds" ] && flags="$flags +S $scheds"
  # 值含空格，必须带引号落盘：init-letsencrypt.sh / sanity_check.sh 以
  # `set -a; . ./.env` 读回，裸值会被 bash 当作"前缀赋值 + 命令 65536"
  # （变量不生效；开了 -e 的 init-letsencrypt.sh 首签当场中断）；
  # compose 解析 .env 时会剥掉这层引号。
  set_var IMBOY_ERL_FLAGS "\"$flags\""
  say "容量参数: ERL_FLAGS=\"$flags\" IMBOY_MAX_WS_CONNECTIONS=$ws"
}

ensure_rsa_keys() {
  local data_dir
  data_dir="$(get_var DATA_DIR)"; data_dir="${data_dir:-./data}"
  bash ../scripts/gen_login_rsa_keys.sh --ensure-only "$data_dir/backend_priv/keys" \
    || die "登录 RSA 密钥检查失败，服务尚未启动 / Login RSA key setup failed"
}

# Garage 对象存储凭据（根 Compose 内置 Garage 服务）：.env 是唯一真源，
# 凭据经 GARAGE_DEFAULT_ACCESS_KEY/SECRET_KEY env 注入 garage 容器，由其
# --single-node --default-bucket 启动参数完成幂等 init（key/bucket 已存在则复用）。
# access key 用 Garage 生态惯例 GK 前缀；RPC secret 一旦产生数据不可更换。
gen_garage_secrets() {
  command -v openssl >/dev/null || die "需要 openssl 生成密钥，请先安装"
  ensure_secret GARAGE_RPC_SECRET       "$(openssl rand -hex 32)"
  ensure_secret IMBOY_GARAGE_ACCESS_KEY "GK$(openssl rand -hex 16)"
  ensure_secret IMBOY_GARAGE_SECRET_KEY "$(openssl rand -hex 32)"
}

# Timings include failed stages and expose no configuration or credential values.
install_started=$SECONDS
stage_started=$install_started
install_stage=prerequisites
install_timing() {
  local status="$1"
  printf '[install-timing] stage=%s elapsed_s=%s total_s=%s status=%s\n' \
    "$install_stage" "$((SECONDS - stage_started))" "$((SECONDS - install_started))" "$status" >&2
}
install_next_stage() {
  install_timing 0
  install_stage="$1"
  stage_started=$SECONDS
}
trap 'rc=$?; if [ -n "${POLICY_DIR:-}" ]; then rm -rf -- "$POLICY_DIR"; fi; install_timing "$rc"; exit "$rc"' EXIT

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

# ── 0a) compose 文件存在性（按 Compose 文件）────────────────────────────────────
if [ ! -f "$COMPOSE_FILE" ]; then
  if [ "$UNIFIED_COMPOSE" = 0 ]; then
    # 商务版 compose 按商务决策走单独交付渠道，不随开源仓分发（.gitignore 精确排除）。
    # 不检查的话，第一次用到它是在 `$COMPOSE up -d`，docker 只会吐一句
    # "no configuration file provided"，非开发者完全无从判断发生了什么。
    die "缺少 $COMPOSE_FILE

  该文件不随开源仓分发，需通过商务交付渠道获取。
  拿到后放到本目录（$(pwd)/${COMPOSE_FILE}）再重跑本脚本。

  索取方式：leeyisoft@qq.com
  源码开发环境使用根目录统一入口（独立安装包不包含）：
      bash scripts/dev_setup.sh
"
  else
    die "缺少 $COMPOSE_FILE —— 根 Compose 随仓分发，本地副本可能不完整，
  请重新 git clone 本仓库后重试"
  fi
fi

if [ "$UNIFIED_COMPOSE" = 0 ]; then
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
  # CSD-DEP-01：客服 Widget 第三域增量（渠道发旧版时在此拦下，不留半配置）
  grep -q 'imboy_widget' "$COMPOSE_FILE" \
    || missing="$missing\n    · 缺少 imboy_widget 静态服务（客服 Widget 不可用）"
  grep -q 'CS_WIDGET_DOMAIN' "$COMPOSE_FILE" \
    || missing="$missing\n    · Nginx 未配置客服 Widget 第三域（CS_WIDGET_DOMAIN）"

  if [ -n "$missing" ]; then
    # shellcheck disable=SC2059
    die "$(printf "%s 版本过旧，缺少以下必需项：\n%b\n\n  请向交付渠道索取最新版本后重试。\n  索取方式：leeyisoft@qq.com" "$COMPOSE_FILE" "$missing")"
  fi
fi

install_next_stage configuration
# ── 1) 配置 .env（必须在 preflight 之前）──────────────────────────────────────
if [ ! -f .env ]; then
  say "生成客户配置模板"
  [ -f .env.example ] || die "缺少 .env.example，仓库不完整"
  cp .env.example .env
  chmod 600 .env
  if [ "$UNIFIED_COMPOSE" = 0 ] || [ "$ASSUME_YES" = 1 ] || [ ! -t 0 ]; then
    printf '\n已生成 %s/.env。填写现场配置后重试 / Complete the generated .env and retry.\n' "$(pwd)"
    if [ "$UNIFIED_COMPOSE" = 1 ]; then exit 2; fi
    exit 0
  fi
fi

# The site Compose keeps its existing certificate contract.
export IMBOY_DEPLOY_CHECK_RTC=1 IMBOY_DEPLOY_CHECK_TURN=1
if [ "$UNIFIED_COMPOSE" = 1 ]; then
  IMBOY_DEPLOY_CHECK_RTC=0
  IMBOY_DEPLOY_CHECK_TURN=0
  if is_true "$(get_var LIVEKIT_ENABLED || true)"; then
    IMBOY_DEPLOY_CHECK_RTC=1
  fi
  if is_true "$(get_var LIVEKIT_TURN_ENABLED || true)"; then
    IMBOY_DEPLOY_CHECK_RTC=1
    IMBOY_DEPLOY_CHECK_TURN=1
  fi
fi
[ "$IMBOY_DEPLOY_CHECK_RTC" = 0 ] || MANUAL_VARS="$MANUAL_VARS RTC_DOMAIN"
[ "$IMBOY_DEPLOY_CHECK_TURN" = 0 ] || MANUAL_VARS="$MANUAL_VARS TURN_DOMAIN"
# Override inactive domain placeholders during Compose interpolation.
if [ "$IMBOY_DEPLOY_CHECK_RTC" = 0 ]; then
  export RTC_DOMAIN="" IMBOY_LIVEKIT_WS_URL=""
fi
if [ "$IMBOY_DEPLOY_CHECK_TURN" = 0 ]; then
  export TURN_DOMAIN=""
fi

if [ "$UNIFIED_COMPOSE" = 1 ] && [ "$ASSUME_YES" = 0 ] && [ -t 0 ]; then
  for key in $MANUAL_VARS; do
    current="$(get_var "$key" || true)"
    case "$current" in
      ""|*example.com|*CHANGE_ME*) ;;
      *) continue ;;
    esac
    printf '  %s: ' "$key" >&2
    IFS= read -r value || die "输入中断。请重试 / Run the installer again"
    case "$key:$value" in
      CERTBOT_EMAIL:*)
        [[ "$value" =~ ^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+$ ]] \
          || die "CERTBOT_EMAIL 格式无效 / Invalid certificate email" ;;
      *)
        [[ "$value" =~ ^[A-Za-z0-9.-]+$ ]] \
          || die "$key 需要纯域名 / Enter a hostname for $key" ;;
    esac
    case "$value" in
      *example.com|*CHANGE_ME*) die "$key 需要现场值 / Enter the actual value for $key" ;;
    esac
    set_var "$key" "$value"
  done
  if [ "$INIT_CLEANUP_DB" = 1 ]; then
    ensure_value IMBOY_DIRECT_CLEANUP_PG_USER imboy_cleanup
  elif [ -z "$(get_var IMBOY_DIRECT_CLEANUP_PG_USER || true)" ] \
      || [ -z "$(get_var IMBOY_DIRECT_CLEANUP_PG_PASSWORD || true)" ]; then
    die "新数据库可使用 --init-cleanup-db；已有清理账号请在 .env 填写其用户名和密码 / Use --init-cleanup-db for a new database, or configure the existing cleanup role in .env"
  fi
fi

command -v openssl >/dev/null || die "需要 openssl 生成密钥，请先安装"
chmod 600 .env

# 自动补齐全部内部密钥。客户预先 cp/edit .env 后，只需执行本脚本一次。
for v in $SECRET_VARS_32; do ensure_secret "$v" "$(openssl rand -hex 16)"; done
for v in $SECRET_VARS_16; do ensure_secret "$v" "$(openssl rand -hex 8)"; done
for v in $SECRET_VARS_48; do ensure_secret "$v" "$(openssl rand -hex 24)"; done
gen_garage_secrets
ensure_rsa_keys
ensure_capacity_vars

UPTRACE_ENABLED_VALUE="$(get_var UPTRACE_ENABLED || true)"
if is_true "$UPTRACE_ENABLED_VALUE"; then
  [ "$UNIFIED_COMPOSE" = 1 ] || die "Uptrace 需要根 Compose / Uptrace requires root Compose"
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
TURN_ENABLED_VALUE="$(get_var LIVEKIT_TURN_ENABLED || true)"
SNI_ENABLED_VALUE="$(get_var LIVEKIT_SNI_ENABLED || true)"
nginx_suffix=
if is_true "$SNI_ENABLED_VALUE"; then
  [ "$UNIFIED_COMPOSE" = 1 ] && is_true "$TURN_ENABLED_VALUE" \
    || die "LIVEKIT_SNI_ENABLED requires root Compose and LIVEKIT_TURN_ENABLED=true"
fi
unset IMBOY_BOOTSTRAP_TURN_CERT
COMPOSE_ARGS=(-f "$COMPOSE_FILE")
COMPOSE_FILES="$COMPOSE_FILE"
if [ "$UNIFIED_COMPOSE" = 1 ]; then
  production_data="$(get_var DATA_DIR || true)"
  production_data="${production_data:-./data}"
  case "$production_data" in
    /*) ;;
    *) production_data="$(pwd -P)/${production_data#./}" ;;
  esac
  export IMBOY_PROD_DATA_DIR="$production_data"
  IMBOY_DEPLOY_PROFILE=prod
  if is_true "$TURN_ENABLED_VALUE"; then
    IMBOY_DEPLOY_PROFILE=prod-turn
    nginx_suffix=_turn
    if is_true "$SNI_ENABLED_VALUE"; then
      IMBOY_DEPLOY_PROFILE=prod-sni
      nginx_suffix=_sni
    fi
    turn_cert_dir="$(get_var LIVEKIT_TURN_CERT_DIR || true)"
    turn_cert_dir="${turn_cert_dir:-$production_data/certbot/conf/live/$(get_var TURN_DOMAIN)}"
    case "$turn_cert_dir" in
      /*) ;;
      *) turn_cert_dir="$PWD/$turn_cert_dir" ;;
    esac
    export IMBOY_PROD_TURN_CERT_DIR="$turn_cert_dir"
    if [ "$turn_cert_dir" = "$production_data/certbot/conf/live/$(get_var TURN_DOMAIN)" ] \
        && { [ ! -s "$turn_cert_dir/fullchain.pem" ] || [ ! -s "$turn_cert_dir/privkey.pem" ]; }; then
      export IMBOY_BOOTSTRAP_TURN_CERT=1
    fi
  fi
  export IMBOY_DEPLOY_PROFILE
  # Only add compatible optional profiles. The installer selects the RTC variant.
  profile_list="${COMPOSE_PROFILES:-$(get_var COMPOSE_PROFILES || true)}"
  case "$profile_list" in
    ""|monitoring|enterprise-direct|monitoring,enterprise-direct|enterprise-direct,monitoring) ;;
    *) die "根 Compose 可额外启用 monitoring 和 enterprise-direct / Invalid additional production profiles" ;;
  esac
  if [ "$IMBOY_DEPLOY_PROFILE" = prod ] && is_true "$(get_var LIVEKIT_ENABLED || true)"; then
    profile_list="${profile_list:+$profile_list,}rtc"
  fi
  if is_true "$UPTRACE_ENABLED_VALUE"; then
    profile_list="${profile_list:+$profile_list,}uptrace"
    export UPTRACE_ENABLED=true
    trace_token="$(get_var UPTRACE_PROJECT_TOKEN)"
    export IMBOY_UPTRACE_DSN="${IMBOY_UPTRACE_DSN:-http://$trace_token@uptrace/1}"
    export IMBOY_UPTRACE_OTLP_ENDPOINT="${IMBOY_UPTRACE_OTLP_ENDPOINT:-http://uptrace:80}"
    unset trace_token
  fi
  export COMPOSE_PROFILES="$profile_list"
  gateway_args=()
  export IMBOY_PROD_DEFAULT_GATEWAY_PROFILE=prod
  if [ "$IMBOY_DEPLOY_PROFILE" != prod ]; then
    export IMBOY_PROD_DEFAULT_GATEWAY_PROFILE=prod-default-gateway
    gateway_args=(--profile "$IMBOY_DEPLOY_PROFILE")
  fi
  COMPOSE_ARGS=(--project-name "${IMBOY_DEPLOY_PROJECT:-imboy}" --profile prod "${gateway_args[@]}" --env-file .env "${COMPOSE_ARGS[@]}")
fi
if [ "$UNIFIED_COMPOSE" = 0 ]; then
  POLICY_DIR="$(mktemp -d)"
  POLICY_FILE="$POLICY_DIR/business-policy.yml"
  bash render-compose-extension.sh imboy-business-policy > "$POLICY_FILE" || die "无法读取根 Compose 商务策略"
  COMPOSE_ARGS+=(-f "$POLICY_FILE")
  COMPOSE_FILES="$COMPOSE_FILES $POLICY_FILE"
  if is_true "$(get_var IMBOY_BUSINESS_WIDGET_ENABLED || true)"; then
    widget_file="$POLICY_DIR/business-widget.yml"
    bash render-compose-extension.sh imboy-business-widget > "$widget_file" || die "无法读取根 Compose 商务 Widget 配置"
    COMPOSE_ARGS+=(-f "$widget_file")
    COMPOSE_FILES="$COMPOSE_FILES $widget_file"
  fi
fi
# ── 1a) LiveKit embedded TURN overlay（LK-DEP-01，LIVEKIT_TURN_ENABLED=true）──
# W5 终态前置门在 preflight（旧 TURN 须已退场 + 证书须就绪），这里只做装配：
# 叠加 docker-compose.livekit-turn.yml 并把证书目录默认值展开写入 .env
# （.env 不做嵌套变量展开，必须由安装器算成绝对可用的字面路径）。
if [ "$UNIFIED_COMPOSE" = 0 ] && is_true "$TURN_ENABLED_VALUE"; then
  [ -f docker-compose.livekit-turn.yml ] \
    || die "LIVEKIT_TURN_ENABLED=true 但缺少 docker-compose.livekit-turn.yml —— 仓库不完整"
  turn_data_dir="$(get_var DATA_DIR || true)"; turn_data_dir="${turn_data_dir:-./data}"
  ensure_value LIVEKIT_TURN_CERT_DIR \
    "${turn_data_dir}/certbot/conf/live/$(get_var TURN_DOMAIN)"
  COMPOSE_ARGS+=(-f docker-compose.livekit-turn.yml)
  COMPOSE_FILES="$COMPOSE_FILES docker-compose.livekit-turn.yml"
fi
for compose_file in ${COMPOSE_EXTRA_FILES[@]+"${COMPOSE_EXTRA_FILES[@]}"}; do
  COMPOSE_ARGS+=(-f "$compose_file")
  COMPOSE_FILES="$COMPOSE_FILES $compose_file"
done
compose() { docker compose "${COMPOSE_ARGS[@]}" "$@"; }
printf -v COMPOSE_DISPLAY '%q ' docker compose "${COMPOSE_ARGS[@]}"
COMPOSE_DISPLAY="${COMPOSE_DISPLAY% }"
if [ "$UNIFIED_COMPOSE" = 1 ]; then
  COMPOSE_DISPLAY="IMBOY_PROD_DEFAULT_GATEWAY_PROFILE=$IMBOY_PROD_DEFAULT_GATEWAY_PROFILE $COMPOSE_DISPLAY"
  IFS=, read -r -a display_profiles <<< "$COMPOSE_PROFILES"
  for profile in "${display_profiles[@]}"; do
    [ -n "$profile" ] && COMPOSE_DISPLAY="$COMPOSE_DISPLAY --profile $profile"
  done
fi

# ── 2) 前置检查 ──────────────────────────────────────────────────────────────
install_next_stage preflight
say "前置检查 / Preflight"
if [ "$UNIFIED_COMPOSE" = 1 ]; then
  # 先确认 Compose 成功，再按白名单读取字面赋值；不执行其输出。
  # Capture status before parsing; never source/eval Compose output.
  resolved_environment="$(compose config --environment)" \
    || die "Compose 环境解析失败；服务尚未启动"
  resolved_gateway="$(compose config --format json | jq -ers 'select(length == 1) | .[0].services.prod_imboy_backend.environment.IMBOY_PAYMENT_GATEWAY_ENABLED | strings' 2>/dev/null)" \
    || die "Compose 密钥校验失败：无法读取所选部署的支付配置 / Cannot inspect payment gateway configuration"
  (
    while IFS= read -r line; do
      case "$line" in
        API_DOMAIN=*|ADMIN_DOMAIN=*|CS_WIDGET_DOMAIN=*|RTC_DOMAIN=*|TURN_DOMAIN=*)
          export "${line?}" || die "Compose 域名赋值失败" ;;
      esac
    done <<< "$resolved_environment"
    domains=(API_DOMAIN ADMIN_DOMAIN CS_WIDGET_DOMAIN)
    [ "$IMBOY_DEPLOY_CHECK_RTC" = 0 ] || domains+=(RTC_DOMAIN)
    [ "$IMBOY_DEPLOY_CHECK_TURN" = 0 ] || domains+=(TURN_DOMAIN)
    for domain in "${domains[@]}"; do
      [ -n "${!domain:-}" ] || die "Compose 域名配置不完整"
    done
    preflight_args=(--docker --dns)
    case "$resolved_gateway" in
      false|0) preflight_args+=(--without-payment) ;;
      true|1) export IMBOY_PAYMENT_GATEWAY_ENABLED=true ;;
      *) die "无效的支付开关 / Invalid payment gateway flag" ;;
    esac
    IMBOY_BASE_URL="https://$API_DOMAIN" IMBOY_PREFLIGHT_RESOLVED_DOMAINS=1 \
      bash preflight.sh "${preflight_args[@]}"
  ) || die "preflight 未通过，修复 ERROR 后重跑"
  unset resolved_environment
else
  bash preflight.sh --docker || die "preflight 未通过，修复 ERROR 后重跑"
fi

# Compose 环境覆盖 .env；仅检查实际传入服务的值，不输出密钥。
if [ "$UNIFIED_COMPOSE" = 1 ]; then
  # -s 要求恰好一个 JSON 文档；-e 与 pipefail 同时传播解析/生产者失败。
  # Suppress jq diagnostics because invalid values may contain secrets.
  compose config --format json | jq -es --arg prefix "$service_prefix" --arg nginx_suffix "$nginx_suffix" --arg rtc "$IMBOY_DEPLOY_CHECK_RTC" '
    def valid($minimum):
      if type == "string" then
        length >= $minimum and (contains("CHANGE_ME") | not)
        and (ascii_downcase | contains("example") | not)
      else false end;
    if length != 1 then false else
      .[0].services as $services |
      $services[$prefix + "imboy_backend"].environment as $backend |
      $services[$prefix + "imboy_edge"].environment as $edge |
      $services[$prefix + "imboy_nginx" + $nginx_suffix].environment as $nginx |
      $services[$prefix + "imboy_pg18"].environment as $pg |
      [$backend.IMBOY_EDGE_INTERNAL_TOKEN, $backend.IMBOY_JWT_KEY,
       $backend.IMBOY_POSTGRE_AES_KEY, $backend.IMBOY_ADM_COOKIE_SECRET] as $core |
      ($services | type == "object") and ($backend | type == "object")
      and ($edge | type == "object") and ($nginx | type == "object")
      and ($pg | type == "object")
      and ($nginx.API_DOMAIN | type == "string")
      and ($nginx.CS_WIDGET_DOMAIN | type == "string")
      and ($backend.IMBOY_BASE_URL == "https://" + $nginx.API_DOMAIN)
      and ($backend.IMBOY_WIDGET_BASE_URL == "https://" + $nginx.CS_WIDGET_DOMAIN)
      and ($backend.IMBOY_WIDGET_DOMAIN == $nginx.CS_WIDGET_DOMAIN)
      and all($core[]; valid(32)) and ($core | unique | length == 4)
      and ($edge.EDGE_INTERNAL_TOKEN == $backend.IMBOY_EDGE_INTERNAL_TOKEN)
      and all(["IMBOY_PG_USER", "IMBOY_PG_PASSWORD", "IMBOY_SOLIDIFIED_KEY",
               "IMBOY_SOLIDIFIED_KEY_IV", "IMBOY_PASSWORD_SALT"][];
              . as $key | $backend[$key] | valid(1))
      and (if $rtc == "1" then
             ($backend.IMBOY_LIVEKIT_API_KEY | valid(1)) and ($backend.IMBOY_LIVEKIT_API_SECRET | valid(32))
           else true end)
      and ($pg.POSTGRES_USER == $backend.IMBOY_PG_USER)
      and ($pg.POSTGRES_PASSWORD == $backend.IMBOY_PG_PASSWORD)
      and (if $services | has($prefix + "imboy_grafana") then
             $services[$prefix + "imboy_grafana"].environment.GF_SECURITY_ADMIN_PASSWORD | valid(1)
           else true end)
      and ($backend.IMBOY_DIRECT_CLEANUP_PG_USER | valid(1))
      and ($backend.IMBOY_DIRECT_CLEANUP_PG_PASSWORD | valid(32))
      and ($backend.IMBOY_DIRECT_CLEANUP_PG_USER != $backend.IMBOY_PG_USER)
      and ($backend.IMBOY_DIRECT_CLEANUP_PG_PASSWORD != $backend.IMBOY_PG_PASSWORD)
    end
  ' >/dev/null 2>&1 || die "Compose 密钥校验失败：密钥或清理账号无效、不独立或服务间不一致；服务尚未启动"
fi

if [ "$UNIFIED_COMPOSE" = 1 ]; then
  # Profiles are selected after interpolation. Gate required production values
  # here, so the dev profile can still parse without a production env file.
  compose config --format json | jq -es '
    def present:
      type == "string" and length > 0 and (contains("CHANGE_ME") | not);
    if length != 1 then false else
      .[0].services as $services |
      ($services.prod_garage.environment // {}) as $garage |
      ($services.prod_imboy_backend.environment // {}) as $backend |
      ($garage.GARAGE_DEFAULT_ACCESS_KEY | present) and
      ($garage.GARAGE_DEFAULT_SECRET_KEY | present) and
      ($garage.GARAGE_DEFAULT_ACCESS_KEY == $backend.IMBOY_GARAGE_ACCESS_KEY) and
      ($garage.GARAGE_DEFAULT_SECRET_KEY == $backend.IMBOY_GARAGE_SECRET_KEY) and
      all($services[]; .image | present and (endswith(":unset") | not))
    end
  ' >/dev/null 2>&1 || die "根 Compose 缺少发布版本或有效 Garage 凭据 / Missing production image version or Garage credentials"
  # Existing containers can share the project name but have older service labels.
  # Verify ownership up front, before pulling images or starting any container.
  planned_containers="$(compose config --format json | jq -er '
    .services | to_entries[] | select(.value.container_name != null) |
    [.value.container_name, .key] | @tsv
  ' 2>/dev/null)" || die "无法解析生产容器清单 / Cannot resolve production container names"
  existing_containers="$(docker ps -a --format '{{.Names}}{{"\t"}}{{.Label "com.docker.compose.project"}}{{"\t"}}{{.Label "com.docker.compose.service"}}')" \
    || die "无法检查现有容器 / Cannot inspect existing containers"
  while IFS=$'\t' read -r container service; do
    binding="$(awk -F '\t' -v name="$container" '$1 == name { print $2 "/" $3 }' <<< "$existing_containers")"
    case "$binding" in
      ""|"${IMBOY_DEPLOY_PROJECT:-imboy}/$service") ;;
      *) die "容器 $container 已属于另一套部署或 TURN 模式。请先完成迁移 / Container $container belongs to another deployment or TURN mode. Migration is required." ;;
    esac
  done <<< "$planned_containers"
fi

# ── 3) 镜像 + 网络 + 启动 ────────────────────────────────────────────────────
# 先拉齐全部缺失镜像再启动，避免私有/缺失镜像导致机器进入半安装状态。
install_next_stage image_pull
say "检查并拉取缺失镜像"
compose pull --policy missing || die "镜像拉取失败，尚未启动任何服务。

  请确认：
    · IMBoy 发布镜像标签存在且客户具备拉取权限
    · 私有 GHCR 已由客户在部署机完成 docker login ghcr.io
    · Docker Hub 网络与登录状态正常"

# Resolve exactly the images Compose will use, including environment overrides.
# The JSON stays in the pipe; only image references leave the parser.
resolved_edge_images="$(compose config --format json | jq -ers --arg prefix "$service_prefix" '
  if length != 1 or (.[0].services | type) != "object" then
    error("invalid services")
  else
    [.[0].services[$prefix + "imboy_backend"].image, .[0].services[$prefix + "imboy_edge"].image] as $images |
    if all($images[]; if type == "string" then
                         length > 0 and (contains("\n") | not)
                       else false end) then
      $images[0], (if $images[1] != $images[0] then $images[1] else empty end)
    else error("invalid images") end
  end
' 2>/dev/null)" || die "Compose 镜像解析失败；服务尚未启动"
[ -n "$resolved_edge_images" ] || die "Compose 镜像为空；服务尚未启动"
# Keep exited probe containers for diagnosis; they have no credentials or network.
while IFS= read -r edge_image; do
  edge_probe_id="$(docker create --network none --entrypoint /bin/sh "$edge_image" -c '
    test -r /opt/imboy/bin/start_download_edge.sh || exit 1
    for p in /opt/imboy/erts-*/bin/escript; do test -x "$p" && break; done
    test -x "$p" || exit 1
    for p in /opt/imboy/lib/imboy-*/priv/workers/download_edge.escript; do test -r "$p" && exit 0; done
    exit 1
  ')" || die "无法创建 edge 镜像检查容器；服务尚未启动"
  docker start -a "$edge_probe_id" >/dev/null || die "所选后端镜像缺少可运行的 edge 文件；服务尚未启动（检查容器: ${edge_probe_id}）"
  say "edge 镜像文件检查通过（检查容器: ${edge_probe_id}）"
done <<< "$resolved_edge_images"

install_next_stage runtime_permissions
say "准备后端非 root 运行目录权限"
bash prepare-backend-runtime.sh "${COMPOSE_ARGS[@]}" \
  || die "后端运行目录权限准备失败；服务尚未启动"

install_next_stage deployment_network
say "准备部署网络"
deployment_network=imboy-network
if [ "$UNIFIED_COMPOSE" = 1 ]; then
  deployment_network="${IMBOY_DEPLOY_NETWORK:-imboy-network}"
  export IMBOY_DEPLOY_NETWORK="$deployment_network"
fi
if ! docker network inspect "$deployment_network" >/dev/null 2>&1; then
  docker network create "$deployment_network" >/dev/null 2>&1 \
    || docker network inspect "$deployment_network" >/dev/null 2>&1 \
    || die "无法访问部署网络 / Cannot access deployment network: $deployment_network"
fi

if [ "$UNIFIED_COMPOSE" = 0 ]; then
  install_next_stage service_start
  say "创建网络并启动服务"
  compose up -d
fi

# ── 4) 首次 TLS 证书签发 ─────────────────────────────────────────────────────
# 此前从不调用 init-letsencrypt.sh，证书永不签发，最后却照常打印
# "部署完成" 和 https:// 地址 —— 用户点进去是连不上的。
api="$(get_var API_DOMAIN)"
install_next_stage tls
say "校验并签发 TLS 证书 (Let's Encrypt)"
COMPOSE_FILES="$COMPOSE_FILES" bash nginx/init-letsencrypt.sh || die "TLS 证书签发失败。

  常见原因：
    · 域名 A 记录未指向本机（Let's Encrypt 的 HTTP-01 校验会失败）
    · 80/443 端口未对公网开放，或被其他进程占用
    · 同一域名短时间内申请次数触发 Let's Encrypt 速率限制

  修复后重跑本脚本。服务已启动但 HTTPS 不可用，请勿对外提供访问。"

# TLS bootstrap starts Nginx and its dependencies after the temporary certificates exist.
# Start the complete profile after issuance, including any optional services and HAProxy.
if [ "$UNIFIED_COMPOSE" = 1 ]; then
  install_next_stage service_start
  say "启动所选生产服务 / Start the selected production services"
  compose up -d
fi

# ── 5) 等待后端健康（最多 120s）──────────────────────────────────────────────
# 不能以日志文案判定就绪：后端不承诺输出固定的 "started on port" 文本，
# 且端口可监听也不代表 PostgreSQL 已可服务。与 Docker healthcheck/蓝绿部署
# 保持同一语义：只有 /healthz 明确返回 status=ok 才继续。
install_next_stage backend_health
say "等待后端健康（/healthz，最多 120s）"
ok=0
for _ in $(seq 1 60); do
  if curl -fsS --max-time 3 "http://127.0.0.1:${BACKEND_PORT:-9800}/healthz" 2>/dev/null \
      | grep -q '"status":"ok"'; then ok=1; break; fi
  sleep 2
done
[ "$ok" = 1 ] || die "后端 120s 内未通过 /healthz，查日志：$COMPOSE_DISPLAY logs ${service_prefix}imboy_backend"

# The helper compares the live release ledger; health alone is insufficient.
if [ "$UNIFIED_COMPOSE" = 1 ]; then
  say "验证独立清理数据库账号（迁移后；已有账号不修改）"
  CLEANUP_ARGS=()
  [ "$INIT_CLEANUP_DB" = 0 ] || CLEANUP_ARGS+=(--create)
  COMPOSE_FILES="$COMPOSE_FILES" bash init-cleanup-db.sh ${CLEANUP_ARGS[@]+"${CLEANUP_ARGS[@]}"} \
    || die "清理数据库账号未就绪；部署未达到可交付状态。保留 .env 和数据，按手册修复后重跑"
fi

# ── 6) 部署后自检（客户交付必跑 / mandatory delivery check）─────────────────
install_next_stage sanity
say "部署后自检 (sanity_check)"
SANITY_ARGS=()
for compose_file in $COMPOSE_FILES; do SANITY_ARGS+=(--compose-file "$compose_file"); done
bash ../scripts/sanity_check.sh "${SANITY_ARGS[@]}" \
  || die "sanity_check 未通过；部署未达到可交付状态，请按上方 ERROR 排查"

install_next_stage admin_setup
# ── 7) 超管创建（可选：--admin-phone/--admin-password 传入时）───────────────
# 部署机只有 Docker 没有 Erlang，imboy_ctl（escript）在 backend 容器内执行：
# 镜像 runtime 阶段已 COPY imboy_ctl，escript 用镜像自带的 ERTS（include_erts）。
# 节点名/cookie 来自镜像 release 的 vm.args；cookie 严禁在仓库写死（W6-A02）：
# 部署时必须经 env IMBOY_CTL_COOKIE 注入（与镜像 vm.args 的 -setcookie 一致），
# 未设置即中止（fail-closed）。
# 明文密码经 env 传入容器（不落宿主机命令行），容器内再作为 argv 交给 escript。
admin_id=""
if [ -n "$ADMIN_PHONE" ]; then
  say "创建超级管理员 (imboy_ctl adm create)"
  [ -n "${IMBOY_CTL_COOKIE:-}" ] \
    || die "未设置 IMBOY_CTL_COOKIE，需与后端镜像 vm.args 的 -setcookie 一致"
  # shellcheck disable=SC2016  # 手机号/密码必须在容器内展开（不落宿主机命令行与进程表）
  ctl_out="$(IMBOY_CTL_NODE="${IMBOY_CTL_NODE:-imboy@127.0.0.1}" \
      IMBOY_CTL_COOKIE="$IMBOY_CTL_COOKIE" \
      IMBOY_CTL_PHONE="$ADMIN_PHONE" \
      IMBOY_CTL_PASSWORD="$ADMIN_PASSWORD" \
      compose exec -T \
      -e IMBOY_CTL_NODE -e IMBOY_CTL_COOKIE -e IMBOY_CTL_PHONE -e IMBOY_CTL_PASSWORD \
      "${service_prefix}imboy_backend" \
      sh -c 'exec /opt/imboy/erts-*/bin/escript /opt/imboy/bin/imboy_ctl adm create --phone "$IMBOY_CTL_PHONE" --password "$IMBOY_CTL_PASSWORD"' 2>&1)" \
    || die "超管创建失败（安装中止）：$ctl_out"
  admin_id="$(printf '%s\n' "$ctl_out" | sed -n 's/^ADMIN_ID=//p' | head -1)"
  printf '  超管已创建：%s\n' \
    "$(printf '%s\n' "$ctl_out" | grep -E '^(ADMIN_ID|CREATED)=' | tr '\n' ' ')"
fi

install_next_stage release_identity
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
backend_image_id="$(compose images -q "${service_prefix}imboy_backend" 2>/dev/null | head -1 || true)"
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
cs="$(get_var CS_WIDGET_DOMAIN)"
rtc="$(get_var RTC_DOMAIN || true)"
turn="$(get_var TURN_DOMAIN || true)"
if [ -n "$ADMIN_PHONE" ]; then
  admin_hint="（超管已创建，ADMIN_ID=${admin_id:-见上方输出}，可直接登录）"
else
  admin_hint="（首次访问自动进入 /setup 创建超管）"
fi

cat <<EOF

✅ 部署完成 / Deployment done

   ── Release Identity（核验装的正是被验证过的那一个）──
   IMBOY_VERSION=${imboy_version}
   ${git_line}
   ${digest_line}

   管理后台 / Admin : https://${adm}   ${admin_hint}
   API / WebSocket  : https://${api}
   客服 Widget      : https://${cs}  (snippet: /v1/loader.js + data-widget-id)
   LiveKit 信令     : $(if [ "$IMBOY_DEPLOY_CHECK_RTC" = 1 ]; then printf "wss://%s" "$rtc"; else printf "未启用 / Disabled"; fi)
   LiveKit TURN     : $(is_true "$TURN_ENABLED_VALUE" && printf 'turn(s):%s:3478|%s  (UDP 3478 / TLS / relay 50201-50500)' "$turn" "$(is_true "$SNI_ENABLED_VALUE" && printf 443 || printf 5349)" || printf '未启用（LIVEKIT_TURN_ENABLED=true 开启，前置条件见 .env.example）')
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
if [ "$UNIFIED_COMPOSE" = 0 ]; then
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
                      蓝绿升级用 bash scripts/imboy-deploy.sh api

   服务状态 : $COMPOSE_DISPLAY ps
   后端日志 : $COMPOSE_DISPLAY logs -f ${service_prefix}imboy_backend
EOF
