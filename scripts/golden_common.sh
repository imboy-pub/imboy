#!/usr/bin/env bash
# golden_common.sh — Golden Gates 门禁公共函数库（P2-G1 / P2-U1 共用）
#
# 仅可被 source，不可直接执行。使用方（在 source 之前设置 GOLDEN_TAG）：
#   scripts/golden_install.sh  → GOLDEN_TAG=GOLDEN_INSTALL（金安装门禁，§4）
#   scripts/golden_upgrade.sh  → GOLDEN_TAG=GOLDEN_UPGRADE（升级门禁，§5）
#
# 契约（本库引用、由调用方提供的全局变量；die/warn 在缺省下失败安全）：
#   GOLDEN_TAG    FAIL 总结行前缀（source 前必须设置）
#   HOSTS_FILE    hosts 注入目标文件，默认 /etc/hosts（干跑可经 GOLDEN_HOSTS_FILE 重定向）
#   DEPLOY_DIR    clone 副本的 deploy/ 目录（.env 读写与证书预置的作用目标）
#   COMMUNITY_YML community compose 文件绝对路径（wrapper/执行 compose 的目标）
#   COMPOSE / COMPOSE_O  compose 命令前缀（均含 runtime overlay；后者再叠加 dev）
#   RUNTIME_YML  release vm.args 路径与 CI Certbot 网络隔离（安装阶段仍为 pro）
#   OVERRIDE_YML  golden override compose 输出路径
#   CLONE_DIR     clone 目录（imboy_ctl wrapper 替换目标）
#   BIN_DIR       运行期生成 bin（psql wrapper）目录
#   RUN_DIR       本次 run 的日志/状态目录；INSTALL_LOG 安装打点日志
#   BASE_URL      后端冒烟基址（wait_healthz / run_smoke8）
#   WS_URL        WebSocket 冒烟基址（run_smoke8）
#   WORKDIR       cleanroom 残留断言用工作目录
#   ADMIN_PHONE / ADMIN_PASSWORD / GIT_REF  run_smoke8 透传超管凭据与 ref
#   API_DOMAIN / ADMIN_DOMAIN / CS_WIDGET_DOMAIN / RTC_DOMAIN / TURN_DOMAIN
#                 五域；host 显式提供，ci 使用独立假域名
#   CURRENT_STAGE / STAGE_HINT / KEEP / PROFILE  die() 排查提示与输出字段（可选）
# 可选测试口（干跑/资源门槛覆盖，生产勿设）：
#   GOLDEN_HOSTS_FILE / GOLDEN_MIN_MEM_GB / GOLDEN_MIN_DISK_GB
#   SMOKE_C2C_FROM / SMOKE_C2C_TO  本轮创建并确认好友关系的 C2C 双方 uid

# ── 颜色与日志（与 deploy/install.sh 同款风格）──────────────────────────────
say()  { printf '\n\033[1;36m==> %s\033[0m\n' "$*"; }
warn() { printf '\033[1;33m⚠️  %s\033[0m\n' "$*"; }
ok()   { printf '\033[0;32m✅ %s\033[0m\n' "$*"; }
die()  {
  trap - ERR
  printf '\n\033[1;31m❌ %s\033[0m\n' "$*" >&2
  if [ -n "${CURRENT_STAGE:-}" ]; then
    printf '排查提示 / Next steps（环节: %s）:\n%s\n' "$CURRENT_STAGE" "${STAGE_HINT:-（见上方输出）}" >&2
  fi
  if [ "${KEEP:-0}" = 1 ]; then
    printf '现场已保留（--keep）: clone=%s 日志=%s\n' "${CLONE_DIR:-?}" "${RUN_DIR:-?}" >&2
  fi
  printf '%s=FAIL STAGE=%s PROFILE=%s\n' "${GOLDEN_TAG:-GOLDEN_GATE}" "${CURRENT_STAGE:-arg}" "${PROFILE:-?}" >&2
  exit 1
}

now() { date +%s; }

# ── .env 读写（与 install.sh set_var/get_var 同款 awk 手法，作用于 clone 副本）──
env_set_var() { # <key> <val>：键存在则替换；不存在则追加。
  # .env.example 里 BACKEND_IMAGE 等键默认是注释行 # KEY=…，仅替换语义会让
  # 写入静默丢失（该缺陷最先在 P2-U1 干跑暴露，G1 同源逻辑一并修复）。
  local key="$1" val="$2" tmp
  # Golden writes only literal assignments: production helpers still read .env
  # as shell input. Reject unsupported/newline bytes without echoing the value.
  [[ "$key" =~ ^[A-Z][A-Z0-9_]*$ && "$val" =~ ^[A-Za-z0-9_./:@%+=-]+$ ]] \
    || die "Golden .env 输入不能安全写为字面值（字段 $key；值未输出）"
  tmp="$(mktemp)"
  # 值经环境传给 awk，不放进 argv，也不把反斜杠当 awk 转义处理。
  # Never expose a generated credential in awk arguments or interpret its bytes.
  GOLDEN_ENV_VALUE="$val" awk -v k="$key" '
    BEGIN { v=ENVIRON["GOLDEN_ENV_VALUE"] }
    index($0, k "=")==1 { print k "=" v; found=1; next }
    { print }
    END { if (!found) print k "=" v }
  ' "$DEPLOY_DIR/.env" > "$tmp"
  chmod 600 "$tmp"
  mv "$tmp" "$DEPLOY_DIR/.env"
}
env_get_var() { grep -E "^$1=" "$DEPLOY_DIR/.env" 2>/dev/null | head -1 | cut -d= -f2- || true; }

configure_golden_domains() {
  if [ "$PROFILE" = ci ]; then
    API_DOMAIN="${API_DOMAIN:-api.golden-ci.imboy.internal}"
    ADMIN_DOMAIN="${ADMIN_DOMAIN:-admin.golden-ci.imboy.internal}"
    CS_WIDGET_DOMAIN="${CS_WIDGET_DOMAIN:-cs.golden-ci.imboy.internal}"
    RTC_DOMAIN="${RTC_DOMAIN:-rtc.golden-ci.imboy.internal}"
    TURN_DOMAIN="${TURN_DOMAIN:-turn.golden-ci.imboy.internal}"
    CERTBOT_EMAIL="${CERTBOT_EMAIL:-golden-ci@imboy.invalid}"
  fi
  local key domain seen="|"
  for key in API_DOMAIN ADMIN_DOMAIN CS_WIDGET_DOMAIN RTC_DOMAIN TURN_DOMAIN; do
    domain="${!key:-}"
    # 与 preflight 的纯域名合同一致；先验证，避免证书路径/主题含任意输入。
    [[ "$domain" =~ ^[A-Za-z0-9]([A-Za-z0-9.-]*[A-Za-z0-9])$ ]] \
      && [[ "$domain" == *.* && "$domain" != *..* ]] \
      || die "$key 必须提供有效纯域名；host 模式须显式提供全部五域"
    case "$domain" in *example.com*|*CHANGE_ME*) die "$key 仍为占位符" ;; esac
    domain="$(printf '%s' "$domain" | tr '[:upper:]' '[:lower:]')"
    case "$seen" in *"|$domain|"*) die "五域必须互不相同（$key 重复）" ;; esac
    seen="$seen$domain|"
  done
  [[ "${CERTBOT_EMAIL:-}" =~ ^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}$ ]] \
    || die "必须提供有效 --certbot-email"
  case "$CERTBOT_EMAIL" in *example.com*|*CHANGE_ME*) die "CERTBOT_EMAIL 仍为占位符" ;; esac
}

prepare_golden_cleanup() {
  # cleanroom 专用：默认身份只加入现有 v5_worker，由安装器受控创建并验证。
  # Preserve supplied credentials; never reset an existing database account.
  local pg_user pg_password
  pg_user="${POSTGRES_USER:-$(env_get_var POSTGRES_USER)}"
  pg_password="${POSTGRES_PASSWORD:-$(env_get_var POSTGRES_PASSWORD)}"
  IMBOY_DIRECT_CLEANUP_PG_USER="${IMBOY_DIRECT_CLEANUP_PG_USER:-$(env_get_var IMBOY_DIRECT_CLEANUP_PG_USER)}"
  IMBOY_DIRECT_CLEANUP_PG_USER="${IMBOY_DIRECT_CLEANUP_PG_USER:-imboy_golden_cleanup}"
  [[ "$IMBOY_DIRECT_CLEANUP_PG_USER" =~ ^[a-z_][a-z0-9_]{0,62}$ ]] \
    && [[ "$IMBOY_DIRECT_CLEANUP_PG_USER" != pg_* && "$IMBOY_DIRECT_CLEANUP_PG_USER" != v5_worker \
       && "$IMBOY_DIRECT_CLEANUP_PG_USER" != "$pg_user" ]] \
    || die "Golden 清理用户名无效或与应用/内置角色不独立"
  IMBOY_DIRECT_CLEANUP_PG_PASSWORD="${IMBOY_DIRECT_CLEANUP_PG_PASSWORD:-$(env_get_var IMBOY_DIRECT_CLEANUP_PG_PASSWORD)}"
  if [ -z "$IMBOY_DIRECT_CLEANUP_PG_PASSWORD" ]; then
    IMBOY_DIRECT_CLEANUP_PG_PASSWORD="$(openssl rand -hex 32)"
  fi
  # The production TLS helper reads .env as shell input. Golden accepts only
  # literal-safe credentials here; reject supplied invalid values, never rewrite.
  [[ "$IMBOY_DIRECT_CLEANUP_PG_PASSWORD" =~ ^[A-Za-z0-9_-]{32,}$ ]] \
    && [ "$IMBOY_DIRECT_CLEANUP_PG_PASSWORD" != "$pg_password" ] \
    || die "Golden 清理密码须独立、至少 32 位且仅含字母数字及 _-（值未输出）"
  case "$(printf '%s' "$IMBOY_DIRECT_CLEANUP_PG_PASSWORD" | tr '[:upper:]' '[:lower:]')" in
    *change_me*|*example*) die "Golden 清理密码仍为占位符，拒绝覆盖已提供值" ;;
  esac
  env_set_var IMBOY_DIRECT_CLEANUP_PG_USER "$IMBOY_DIRECT_CLEANUP_PG_USER"
  env_set_var IMBOY_DIRECT_CLEANUP_PG_PASSWORD "$IMBOY_DIRECT_CLEANUP_PG_PASSWORD"
  # Keep Compose's process environment and .env in agreement, including overrides.
  export IMBOY_DIRECT_CLEANUP_PG_USER IMBOY_DIRECT_CLEANUP_PG_PASSWORD
}

prepare_golden_control() {
  local source="$CLONE_DIR/config/vm.args" node data_dir target tmp
  [ -r "$source" ] || die "固定源码缺 config/vm.args，不能推断 release 节点身份"
  node="$(awk '$1=="-name" {print $2; n++} END {if(n!=1) exit 1}' "$source")" \
    || die "release vm.args 必须恰含一个 -name"
  IMBOY_CTL_NODE="${IMBOY_CTL_NODE:-$node}"
  [[ "$IMBOY_CTL_NODE" =~ ^[A-Za-z0-9_.-]+@[A-Za-z0-9_.-]+$ ]] \
    || die "IMBOY_CTL_NODE 必须为完整 release 节点名"
  IMBOY_CTL_COOKIE="${IMBOY_CTL_COOKIE:-$(openssl rand -hex 32)}"
  [[ "$IMBOY_CTL_COOKIE" =~ ^[A-Za-z0-9_-]{32,255}$ ]] \
    || die "IMBOY_CTL_COOKIE 须为 32–255 位字母、数字、下划线或连字符（值未输出）"
  data_dir="$(resolve_data_dir)"
  target="$data_dir/backend_priv/golden-vm.args"
  mkdir -p "$(dirname "$target")"
  tmp="$(mktemp "${target}.XXXXXX")"
  # relx reads VMARGS_PATH for foreground and ping/eval; changing only the CLI
  # cookie would leave the server using the baked-in development value.
  if ! GOLDEN_CTL_NODE="$IMBOY_CTL_NODE" GOLDEN_CTL_COOKIE="$IMBOY_CTL_COOKIE" awk '
    $1=="-name" {print "-name " ENVIRON["GOLDEN_CTL_NODE"]; next}
    $1=="-setcookie" {print "-setcookie " ENVIRON["GOLDEN_CTL_COOKIE"]; n++; next}
    {print}
    END {if(n!=1) exit 1}
  ' "$source" > "$tmp"; then
    rm -f "$tmp"
    die "release vm.args 必须恰含一个 -setcookie"
  fi
  chmod 600 "$tmp"
  mv "$tmp" "$target"
  cat > "$RUNTIME_YML" <<'EOF'
# Golden cleanroom runtime identity; the installation still starts in pro mode.
# The existing backend_priv read-only mount supplies this file to relx.
services:
  imboy_backend:
    environment:
      VMARGS_PATH: /opt/imboy/priv_runtime/golden-vm.args
EOF
  if [ "$PROFILE" = ci ]; then
    # No accidental public ACME traffic in CI, including the renewal sidecar.
    cat >> "$RUNTIME_YML" <<'EOF'
  imboy_certbot:
    network_mode: none
EOF
  fi
  export IMBOY_CTL_NODE IMBOY_CTL_COOKIE
}

prepare_golden_install() { # <backend image ref>; shared before either first pull
  local backend_ref="$1" help key
  help="$(bash "$DEPLOY_DIR/install.sh" --help)" \
    || die "无法读取固定源码安装器的输入合同"
  [[ "$help" == *--compose-file* && "$help" == *--init-cleanup-db* ]] \
    || die "所选源码安装器不支持 runtime overlay/受控清理账号初始化；不能安全执行该 Golden 基线"
  if [ ! -f "$DEPLOY_DIR/.env" ]; then
    bash "$DEPLOY_DIR/install.sh" --edition community --yes >/dev/null
  fi
  [ -f "$DEPLOY_DIR/.env" ] || die "install.sh 第一段未生成 .env"
  for key in API_DOMAIN ADMIN_DOMAIN CS_WIDGET_DOMAIN RTC_DOMAIN TURN_DOMAIN CERTBOT_EMAIL; do
    env_set_var "$key" "${!key}"
  done
  env_set_var IMBOY_BASE_URL "https://$API_DOMAIN"
  env_set_var BACKEND_IMAGE "$backend_ref"
  if [ -n "$ADMIN_IMAGE_REF" ]; then env_set_var ADMIN_IMAGE "$ADMIN_IMAGE_REF"; fi
  if [ -n "$PG_IMAGE_REF" ]; then env_set_var PG_IMAGE "$PG_IMAGE_REF"; fi
  if [ -n "$WIDGET_IMAGE_REF" ]; then env_set_var IMBOY_WIDGET_IMAGE "$WIDGET_IMAGE_REF"; fi
  prepare_golden_cleanup
  prepare_golden_control
  if [ "$PROFILE" = ci ]; then preset_ci_certs; fi
  write_compose_override
  install_ctl_wrapper
  make_psql_wrapper
}

# 解析 DATA_DIR 为绝对路径（证书预置/数据目录断言共用；相对路径基于 DEPLOY_DIR）
resolve_data_dir() {
  local d
  d="$(env_get_var DATA_DIR)"; d="${d:-./data}"
  case "$d" in
    /*) printf '%s' "$d" ;;
    *)  printf '%s/%s' "$DEPLOY_DIR" "${d#./}" ;;
  esac
}

# ── hosts 注入（presign URL host=IMBOY_GARAGE_ENDPOINT 默认 garage:3900）─────
hosts_set_garage() { # 解析 garage 容器 IP 并写入 hosts（幂等：先清旧行）
  local mark="golden-${GOLDEN_MARK:-install}"
  local ip tmp
  ip="$(docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' imboy_garage 2>/dev/null || true)"
  [ -n "$ip" ] || { STAGE_HINT="docker inspect imboy_garage 无 IP。检查: docker ps --filter name=imboy_garage 是否在跑。"; return 1; }
  tmp="$(mktemp)"
  grep -v "# ${mark} garage$" "$HOSTS_FILE" > "$tmp" 2>/dev/null || true
  printf '%s garage # %s garage\n' "$ip" "$mark" >> "$tmp"
  cat "$tmp" > "$HOSTS_FILE"
  rm -f "$tmp"
  printf '  hosts: %s garage -> %s（presign URL host=IMBOY_GARAGE_ENDPOINT 默认 garage:3900）\n' "$HOSTS_FILE" "$ip"
}
hosts_del_garage() {
  local mark="golden-${GOLDEN_MARK:-install}"
  local tmp
  tmp="$(mktemp)"
  grep -v "# ${mark} garage$" "$HOSTS_FILE" > "$tmp" 2>/dev/null || true
  cat "$tmp" > "$HOSTS_FILE"
  rm -f "$tmp"
}

wait_healthz() { # <max_seconds> <label>
  local deadline=$(( $(now) + $1 )) body=""
  printf '  等待后端就绪 %s（最多 %ss）…\n' "${2:-}" "$1"
  while [ "$(now)" -lt "$deadline" ]; do
    body="$(curl -sS -m 5 "$BASE_URL/healthz" 2>/dev/null || true)"
    if printf '%s' "$body" | grep -q '"status":"ok"'; then return 0; fi
    sleep 2
  done
  STAGE_HINT="curl $BASE_URL/healthz 复现；看日志: ${COMPOSE_O:-docker compose} logs --tail=100 imboy_backend"
  return 1
}

# ── 安装执行（输出逐行打时间戳，供分环节计时提取）──────────────────────────
# 锚点缺失是正常分支（如 ci 模式无"签发 TLS 证书"行），必须失败安全：
# pipefail 下 grep 无匹配的 rc=1 若传出，会触发 set -e 误杀调用方赋值语句
anchor_ts() { grep -F "$1" "$INSTALL_LOG" 2>/dev/null | head -1 | cut -d' ' -f1 || true; }

capture_backend_failure_log() {
  # 失败制品只收集明确列出的诊断日志，绝不打包 RUN_DIR/.env/数据目录，
  # 以免把安装期生成的凭据或私钥带出 cleanroom。
  {
    printf '%s\n' '== docker compose ps =='
    $COMPOSE ps
    printf '%s\n' '== imboy_backend runtime state =='
    docker inspect -f 'status={{.State.Status}} exit={{.State.ExitCode}} error={{.State.Error}} started={{.State.StartedAt}} finished={{.State.FinishedAt}}' imboy_backend || true
    printf '%s\n' '== imboy_pg18 logs (tail 300) =='
    $COMPOSE logs --tail=300 imboy_pg18
    printf '%s\n' '== imboy_backend logs (tail 300) =='
    $COMPOSE logs --tail=300 imboy_backend
  } > "$RUN_DIR/backend_failure.log" 2>&1 || true
}

run_install_phase2() {
  local rc=0
  printf '\n'
  bash "$DEPLOY_DIR/install.sh" \
    --edition community --yes \
    --compose-file "$RUNTIME_YML" --init-cleanup-db \
    --admin-phone "$ADMIN_PHONE" --admin-password "$ADMIN_PASSWORD" \
    2>&1 | while IFS= read -r line; do
        printf '%s\n' "$line"
        printf '%s %s\n' "$(now)" "$line" >&3
      done 3>>"$INSTALL_LOG" || rc=$?
  [ "$rc" -eq 0 ] || { capture_backend_failure_log; STAGE_HINT="install.sh 退出码 ${rc}。完整日志: $INSTALL_LOG
      容器状态: $COMPOSE ps
      后端日志: $RUN_DIR/backend_failure.log"; return "$rc"; }
}

verify_golden_cleanup() {
  # The controlled helper requires this exact base filename and changes into
  # deploy/ itself. Overlay paths remain absolute and validation is read-only.
  COMPOSE_FILES="docker-compose.community.yml $RUNTIME_YML $OVERRIDE_YML" \
    bash "$DEPLOY_DIR/init-cleanup-db.sh"
}

# 从打点日志提取 install.sh 内部环节（say() 锚点行；缺失则记 0 并注明）
# 结果写入调用方可见的 SEC_STACK/SEC_TLS/SEC_INITTAIL/TLS_NOTE
# shellcheck disable=SC2034  # 计时变量由调用方（golden_install.sh report 等）读取
parse_install_stages() {
  local a_stack a_tls a_tls_skip a_wait install_end
  a_stack="$(anchor_ts '创建网络并启动服务')"
  a_tls="$(anchor_ts '校验并签发 TLS 证书')"
  [ -n "$a_tls" ] || a_tls="$(anchor_ts '签发 TLS 证书')"
  a_tls_skip="$(anchor_ts 'TLS 证书已存在')"
  a_wait="$(anchor_ts '等待后端健康')"
  [ -n "$a_wait" ] || a_wait="$(anchor_ts '等待后端启动')"
  install_end="${1:-0}"

  if [ -n "$a_stack" ] && { [ -n "$a_tls" ] || [ -n "$a_tls_skip" ]; }; then
    if [ -n "$a_tls" ]; then
      SEC_STACK=$(( a_tls - a_stack ))
      if [ "${PROFILE:-host}" = ci ]; then
        TLS_NOTE="CI 自签 fixture：本地 HTTP-01/nginx 校验，未验证公网 TLS"
      else
        TLS_NOTE="证书校验/按需签发（Let's Encrypt）"
      fi
    else
      SEC_STACK=$(( a_tls_skip - a_stack ))
      TLS_NOTE="SKIPPED（ci 预置自签证书，install.sh 幂等跳过）"
    fi
    SEC_TLS=0
    if [ -n "$a_tls" ] && [ -n "$a_wait" ]; then
      SEC_TLS=$(( a_wait - a_tls ))
    fi
  else
    SEC_STACK=0
    TLS_NOTE="锚点缺失（install.sh 输出格式变化？看 ${INSTALL_LOG}）"
  fi
  if [ -n "$a_wait" ] && [ "$install_end" -gt 0 ]; then
    SEC_INITTAIL=$(( install_end - a_wait ))
  fi
}

# ── 等价自检（sanity_check.sh 硬编码 prod.yml，community 口径在此等价实现）──
sanity_equivalent() {
  local svc okc=0
  for svc in imboy_pg18 imboy_garage imboy_backend imboy_admin imboy_nginx imboy_certbot imboy_livekit; do
    if docker ps --filter "name=^${svc}\$" --format '{{.Names}}' 2>/dev/null | grep -q .; then
      okc=$((okc+1))
    else
      warn "核心容器未运行: ${svc}（$COMPOSE_O ps）"
    fi
  done
  [ "$okc" -eq 7 ] || { STAGE_HINT="$COMPOSE_O ps 查看未起的容器；日志 $COMPOSE_O logs <svc>"; return 1; }
  ok "7/7 核心容器运行中（pg/garage/backend/admin/nginx/certbot/livekit）"

  wait_healthz 120 "（sanity）" || return 1
  ok "/healthz status=ok"

  local rows
  rows="$(PGPASSWORD="$(env_get_var POSTGRES_PASSWORD)" PGUSER="$(env_get_var POSTGRES_USER)" \
          PGDATABASE="$(env_get_var POSTGRES_DB)" \
          "$BIN_DIR/psql" -At -c 'SELECT count(*) FROM schema_migrations' 2>/dev/null || true)"
  [ -n "$rows" ] && [ "$rows" -gt 0 ] 2>/dev/null \
    || { STAGE_HINT="schema_migrations 不可查/为空（迁移未跑？容器内: $COMPOSE exec -T imboy_pg18 psql -U $(env_get_var POSTGRES_USER) -l"; return 1; }
  ok "数据库迁移已执行（schema_migrations=${rows}）"

  if [ "${PROFILE:-ci}" = "host" ]; then
    # host 模式入口验证：真 DNS + 真 TLS + nginx 反代链路（§4.1 流程的 TLS 面）
    curl -sS -m 10 "https://${API_DOMAIN}/healthz" 2>/dev/null | grep -q '"status":"ok"' \
      || { STAGE_HINT="curl https://${API_DOMAIN}/healthz 复现；查 nginx: $COMPOSE logs --tail=50 imboy_nginx
      证书: $COMPOSE exec -T imboy_nginx nginx -t"; return 1; }
    ok "入口验证：https://${API_DOMAIN}/healthz 经 nginx+TLS 返回 ok"
  else
    warn "ci 模式：nginx 443 公网链路未验证（无公网域名，客观约束）"
  fi
}

# ── cleanroom 冒烟身份：通过真实 API 注册、登录并建立双向好友 ──────────────
golden_fixture_http() { # <method> <url> [curl 参数…]；响应仅落私有 run 目录
  local method="$1" url="$2"; shift 2
  curl -sS -m 15 -o "$RUN_DIR/golden_fixture.json" -w '%{http_code}' \
    -X "$method" "$@" "$url" 2>/dev/null
}

golden_fixture_ok() {
  [ "$1" = 200 ] && jq -e '.code == 0' "$RUN_DIR/golden_fixture.json" >/dev/null 2>&1
}

golden_json_id() { # <response file> <path key>…；JSON integer/string → 无损十进制 ID
  python3 - "$@" <<'PY'
import json, re, sys
try:
    with open(sys.argv[1], encoding="utf-8") as response:
        value = json.load(response)
    for key in sys.argv[2:]:
        value = value[key]
    if type(value) not in (int, str) or not re.fullmatch(r"[1-9][0-9]*", str(value)):
        sys.exit(1)
    print(value)
except (OSError, ValueError, TypeError, KeyError):
    sys.exit(1)
PY
}

golden_register_login() { # <label>；输出 GOLDEN_USER_*，不覆盖调用方 TOKEN/UID
  local label="$1" code
  GOLDEN_USER_EMAIL="golden-${label}-$(date +%s)-$(openssl rand -hex 6)@smoke.local"
  GOLDEN_USER_PASSWORD="Golden1-$(openssl rand -hex 12)"
  GOLDEN_USER_UID=''; GOLDEN_USER_TOKEN=''
  { code="$(golden_fixture_http POST "$BASE_URL/api/v1/passport/signup" \
    -H 'Content-Type: application/x-www-form-urlencoded' \
    --data-urlencode 'type=email' --data-urlencode "account=$GOLDEN_USER_EMAIL" \
    --data-urlencode "pwd=$GOLDEN_USER_PASSWORD" \
    --data-urlencode "code=${GOLDEN_SMOKE_CODE:?Golden smoke code is required}" \
    --data-urlencode 'rsa_encrypt=0' --data-urlencode "nickname=golden-$label")" \
    && golden_fixture_ok "$code"; } \
    || { STAGE_HINT="Golden 用户 $label 注册失败（HTTP ${code:-000}；响应未输出）"; return 1; }
  { code="$(golden_fixture_http POST "$BASE_URL/api/v1/passport/login" \
    -H 'Content-Type: application/x-www-form-urlencoded' \
    --data-urlencode 'type=email' --data-urlencode "account=$GOLDEN_USER_EMAIL" \
    --data-urlencode "pwd=$GOLDEN_USER_PASSWORD" --data-urlencode 'rsa_encrypt=0')" \
    && golden_fixture_ok "$code"; } \
    || { STAGE_HINT="Golden 用户 $label 登录失败（HTTP ${code:-000}；响应未输出）"; return 1; }
  # 当前 API 返回 bigint 数字，兼容字符串；Python 保留整数精度，避免 jq 舍入 UID。
  GOLDEN_USER_UID="$(golden_json_id "$RUN_DIR/golden_fixture.json" payload uid)" \
    || { STAGE_HINT="Golden 用户 $label 登录缺少有效 UID（响应未输出）"; return 1; }
  GOLDEN_USER_TOKEN="$(jq -er '.payload.token | select(type == "string" and length > 0)' "$RUN_DIR/golden_fixture.json")" \
    || { STAGE_HINT="Golden 用户 $label 登录缺少有效 UID/token（响应未输出）"; return 1; }
}

golden_friend_has() { # <token> <peer uid>
  local code
  { code="$(golden_fixture_http GET "$BASE_URL/api/v1/friend/list" -H "Authorization: Bearer $1")" \
    && golden_fixture_ok "$code"; } || return 1
  # friend/list 返回对方用户公开资料，主键是 id（不是关系表的 from/to_user_id）。
  python3 - "$RUN_DIR/golden_fixture.json" "$2" <<'PY'
import json, sys
try:
    with open(sys.argv[1], encoding="utf-8") as response:
        friends = json.load(response)["payload"]["friend"]
    if not isinstance(friends, list):
        sys.exit(1)
    found = any(isinstance(friend, dict) and type(friend.get("id")) in (int, str)
                and str(friend["id"]) == sys.argv[2] for friend in friends)
    sys.exit(0 if found else 1)
except (OSError, ValueError, TypeError, KeyError):
    sys.exit(1)
PY
}

golden_friend_make() { # <tokenA> <uidA> <tokenB> <uidB>
  local token_a="$1" uid_a="$2" token_b="$3" uid_b="$4" code payload
  payload='{"from":{"source":"golden"},"msg":"golden cleanroom gate"}'
  { code="$(golden_fixture_http POST "$BASE_URL/api/v1/friend/add" \
    -H "Authorization: Bearer $token_a" -H 'Content-Type: application/x-www-form-urlencoded' \
    --data-urlencode "to=$uid_b" --data-urlencode "payload=$payload" \
    --data-urlencode "created_at=$(( $(date +%s) * 1000 ))")" \
    && golden_fixture_ok "$code"; } \
    || { STAGE_HINT="Golden 好友申请失败（HTTP ${code:-000}）"; return 1; }
  { code="$(golden_fixture_http POST "$BASE_URL/api/v1/friend/confirm" \
    -H "Authorization: Bearer $token_b" -H 'Content-Type: application/x-www-form-urlencoded' \
    --data-urlencode "from=$uid_a" --data-urlencode "to=$uid_b" --data-urlencode "payload=$payload")" \
    && golden_fixture_ok "$code"; } \
    || { STAGE_HINT="Golden 好友确认失败（HTTP ${code:-000}）"; return 1; }
  { golden_friend_has "$token_a" "$uid_b" && golden_friend_has "$token_b" "$uid_a"; } \
    || { STAGE_HINT="Golden 双向好友列表尚未确认，不能生成 C2C 数据"; return 1; }
}

prepare_golden_smoke_users() {
  local uid_a token_a uid_b token_b
  golden_register_login smoke-a || return 1
  uid_a="$GOLDEN_USER_UID"; token_a="$GOLDEN_USER_TOKEN"
  golden_register_login smoke-b || return 1
  uid_b="$GOLDEN_USER_UID"; token_b="$GOLDEN_USER_TOKEN"
  [ "$uid_a" != "$uid_b" ] \
    || { STAGE_HINT="Golden C2C 双方 UID 必须不同"; return 1; }
  golden_friend_make "$token_a" "$uid_a" "$token_b" "$uid_b" || return 1
  SMOKE_C2C_FROM="$uid_a"; SMOKE_C2C_TO="$uid_b"
  ok "Golden C2C 双向好友已创建（$uid_a → $uid_b）"
}

# ── 8 步冒烟链执行（P3-C4 的 smoke_8step.sh；ENV_PASSTHROUGH 见函数体）───────
run_smoke8() {
  local rc=0
  [[ "${SMOKE_C2C_FROM:-}" =~ ^[1-9][0-9]*$ && "${SMOKE_C2C_TO:-}" =~ ^[1-9][0-9]*$ \
    && "$SMOKE_C2C_FROM" != "$SMOKE_C2C_TO" ]] \
    || { STAGE_HINT="Golden C2C 必须提供两个不同的有效用户 UID"; return 1; }
  printf '\n'
  ( cd "$CLONE_DIR" && \
    PATH="$BIN_DIR:$PATH" \
    BASE_URL="$BASE_URL" \
    WS_URL="${WS_URL:-ws://127.0.0.1:9800/api/v1/ws}" \
    SMOKE_ADMIN_ACCOUNT="$ADMIN_PHONE" \
    SMOKE_ADMIN_PASSWORD="$ADMIN_PASSWORD" \
    SMOKE_MASTER_CODE="${GOLDEN_SMOKE_CODE:?Golden smoke code is required}" \
    SMOKE_C2C_FROM="${SMOKE_C2C_FROM:?Golden sender fixture is required}" \
    SMOKE_C2C_TO="${SMOKE_C2C_TO:?Golden recipient fixture is required}" \
    PGHOST=127.0.0.1 PGPORT=5432 \
    PGUSER="$(env_get_var POSTGRES_USER)" \
    PGDATABASE="$(env_get_var POSTGRES_DB)" \
    PGPASSWORD="$(env_get_var POSTGRES_PASSWORD)" \
    bash scripts/smoke_8step.sh ) || rc=$?
  [ "$rc" -eq 0 ] || { STAGE_HINT="smoke_8step 退出码 ${rc}（上方有分步 FAIL 与提示）。
      后端日志: $COMPOSE_O logs --tail=100 imboy_backend
      容器内手跑: $COMPOSE_O exec -T imboy_backend /opt/imboy/erts-*/bin/escript /opt/imboy/bin/imboy_ctl node status"; return "$rc"; }
}

# ── 现场适配：override / wrapper / 证书预置 ──────────────────────────────────
write_compose_override() {
  # 每个 cleanroom 一次生成；同一轮注册、restart 与升级共用，生产起栈不注入。
  GOLDEN_SMOKE_CODE="$(openssl rand -hex 16)" || return 1
  export GOLDEN_SMOKE_CODE
  : > "$OVERRIDE_YML"
  chmod 600 "$OVERRIDE_YML"
  {
    cat <<EOF
# ${GOLDEN_TAG:-golden} 自动生成（冒烟自动化前提，仅本 clone 副本内生效）。
# backend IMBOYENV 覆盖为 dev：万能验证码(verification_master_code)与 admin
# captcha=1234 仅 local/dev/test 放行（verification_code_ds:is_master_code /
# adm_passport_handler:admin_test_captcha_enabled），cleanroom 无法人工介入
# 真实验证码。生产 fail-fast 语义已由 install.sh 原生 pro 起栈阶段验证。
services:
  imboy_backend:
    environment:
      IMBOYENV: dev
      IMBOY_VERIFICATION_MASTER_CODE: "$GOLDEN_SMOKE_CODE"
EOF
    if [ "${PROFILE:-host}" = "ci" ]; then
      cat <<EOF
      # ci 模式假域名无公网解析、证书为预置自签（curl 校验必挂）：presign 公网
      # endpoint 回落内网直连（garage:3900 经 hosts 注入可达）。host 模式不覆盖，
      # 保持默认 https://\${API_DOMAIN}/s3 走真实 nginx /s3/ 反代 + 真 TLS 链路。
      IMBOY_GARAGE_PUBLIC_ENDPOINT: http://garage:3900
EOF
    fi
  } > "$OVERRIDE_YML"
  # --keep/失败后可在新 shell 直接 compose logs/down；该 smoke 私有文件不得上传。
}

install_ctl_wrapper() {
  # 替换 clone 副本内的 scripts/imboy_ctl 为容器内转发 wrapper。
  # smoke_8step.sh 以绝对路径引用该文件（PATH 注入无效），故只能就地替换；
  # 原文件保留为 imboy_ctl.golden-real。
  if [ -f "$CLONE_DIR/scripts/imboy_ctl.golden-real" ]; then
    mv -f "$CLONE_DIR/scripts/imboy_ctl.golden-real" "$CLONE_DIR/scripts/imboy_ctl"
  fi
  mv "$CLONE_DIR/scripts/imboy_ctl" "$CLONE_DIR/scripts/imboy_ctl.golden-real"
  cat > "$CLONE_DIR/scripts/imboy_ctl" <<'EOF'
#!/usr/bin/env bash
# Golden 容器内转发 wrapper（原实现: imboy_ctl.golden-real）。
# 原因：release 节点 vm.args 为 -kernel inet_dist_use_interface {127,0,0,1}，
# 宿主机 escript 无法直连容器节点；与 install.sh 超管创建同款手法：容器内
# ERTS 执行（镜像已 COPY /opt/imboy/bin/imboy_ctl）。
set +x
set +v
set -euo pipefail
: "${IMBOY_CTL_NODE:?Golden release node identity is required}"
: "${IMBOY_CTL_COOKIE:?Golden release cookie is required}"
EOF
  printf 'exec docker compose -f %q -f %q exec -T \\\n' \
    "$COMMUNITY_YML" "$RUNTIME_YML" >> "$CLONE_DIR/scripts/imboy_ctl"
  cat >> "$CLONE_DIR/scripts/imboy_ctl" <<'EOF'
  -e IMBOY_CTL_NODE -e IMBOY_CTL_COOKIE \
  imboy_backend \
  sh -c 'exec /opt/imboy/erts-*/bin/escript /opt/imboy/bin/imboy_ctl "$@"' golden "$@"
EOF
  chmod +x "$CLONE_DIR/scripts/imboy_ctl"
}

make_psql_wrapper() {
  # smoke_8step.sh 的 C2C 落库断言用宿主机 psql；cleanroom 不强制安装
  # postgresql-client，改为转发到 pg 容器内执行（-e 从调用方 env 取值）。
  cat > "$BIN_DIR/psql" <<'EOF'
#!/usr/bin/env bash
# Golden: psql -> docker compose exec imboy_pg18 psql
EOF
  printf 'exec docker compose -f %q -f %q exec -T \\\n' \
    "$COMMUNITY_YML" "$RUNTIME_YML" >> "$BIN_DIR/psql"
  cat >> "$BIN_DIR/psql" <<'EOF'
  -e PGPASSWORD -e PGUSER -e PGDATABASE imboy_pg18 psql "$@"
EOF
  chmod +x "$BIN_DIR/psql"
}

preset_ci_certs() {
  # CI 专用 fixture：无生产 bootstrap marker / ACME lineage，保留本地 HTTP-01
  # 与 nginx 校验；不修改生产首签判定，不代表公网 TLS/RTC 已验证。
  [ "$PROFILE" = ci ] || die "自签 fixture 仅允许在 --profile ci 使用"
  local base d live
  local domains=("$API_DOMAIN" "$ADMIN_DOMAIN" "$CS_WIDGET_DOMAIN" "$RTC_DOMAIN" "$TURN_DOMAIN")
  base="$(resolve_data_dir)/certbot/conf"
  case "$(env_get_var UPTRACE_ENABLED | tr '[:upper:]' '[:lower:]')" in
    true|1)
      d="$(env_get_var UPTRACE_DOMAIN)"
      [[ "$d" =~ ^[A-Za-z0-9]([A-Za-z0-9.-]*[A-Za-z0-9])$ && "$d" == *.* && "$d" != *..* ]] \
        || die "UPTRACE_ENABLED 要求有效 UPTRACE_DOMAIN"
      case "$d" in *example.com*|*CHANGE_ME*) die "UPTRACE_DOMAIN 仍为占位符" ;; esac
      domains+=("$d") ;;
  esac
  for d in "${domains[@]}"; do
    live="$base/live/$d"
    [ ! -e "$live/.imboy-selfsigned" ] && [ ! -e "$base/archive/$d" ] \
      && [ ! -e "$base/renewal/$d.conf" ] \
      || die "CI 证书目录含生产 bootstrap/ACME 状态，拒绝覆盖：$d"
    if [ -s "$live/fullchain.pem" ] && [ -s "$live/privkey.pem" ]; then
      continue
    fi
    [ ! -e "$live/fullchain.pem" ] && [ ! -e "$live/privkey.pem" ] \
      || die "CI 证书 fixture 不完整，拒绝覆盖：$d"
    mkdir -p "$live"
    openssl req -x509 -nodes -newkey rsa:2048 -days 1 \
      -keyout "$live/privkey.pem" -out "$live/fullchain.pem" \
      -subj "/CN=$d" -addext "subjectAltName=DNS:$d" 2>/dev/null \
      || die "CI 证书 fixture 生成失败：$d"
    chmod 600 "$live/privkey.pem"
    [ -s "$live/fullchain.pem" ] && [ -s "$live/privkey.pem" ] \
      || die "CI 证书 fixture 文件缺失：$d"
  done
  printf '  ci 模式：%s 域自签 fixture 就绪；保留本地 HTTP-01/nginx 校验，未验证公网 TLS\n' "${#domains[@]}"
}

# ── cleanroom 断言（防脏环境假绿灯；各子断言 <50 行，行为与 G1 交付一致）────
assert_cleanroom() {
  command -v docker >/dev/null 2>&1 || die "cleanroom 断言失败：docker 命令不存在"
  docker info >/dev/null 2>&1 || die "cleanroom 断言失败：docker daemon 未运行或当前用户无权限"
  docker compose version >/dev/null 2>&1 || die "cleanroom 断言失败：docker compose v2 插件不可用"
  ok "docker + compose v2 可用"
  assert_cleanroom_leftovers
  assert_cleanroom_resources
  assert_cleanroom_deps
  assert_cleanroom_hosts
  # 幂等安全：clone 目录已存在 = 上次 --keep 残留
  if [ -e "$CLONE_DIR" ]; then
    die "clone 目录已存在: ${CLONE_DIR}（上次 --keep 残留？）。
  请手工清理后重跑，或换 --workdir"
  fi
}

assert_cleanroom_leftovers() {
  # 无既有 imboy 容器 / 卷 / 网络（门禁是 cleanroom 工具，不是升级/共存工具）
  local dirty=""
  if docker ps -a --format '{{.Names}}' 2>/dev/null | grep -q '^imboy_'; then
    docker ps -a --format '{{.Names}}' 2>/dev/null | grep '^imboy_' | head -5 | sed 's/^/      · /' >&2
    dirty="已有 imboy_* 容器"
  fi
  if docker volume ls -q 2>/dev/null | grep -q '^imboy_'; then
    docker volume ls -q 2>/dev/null | grep '^imboy_' | head -5 | sed 's/^/      · /' >&2
    dirty="${dirty:+${dirty}；}已有 imboy_* 卷"
  fi
  if docker network ls --format '{{.Name}}' 2>/dev/null | grep -qx 'imboy-network'; then
    dirty="${dirty:+${dirty}；}已存在 imboy-network 网络"
  fi
  [ -z "$dirty" ] || die "cleanroom 断言失败：${dirty}。
  本脚本要求干净环境（不留前置状态）。请先清理:
      docker ps -a --format '{{.Names}}' | grep '^imboy_' | xargs -r docker rm -f
      docker volume ls -q | grep '^imboy_' | xargs -r docker volume rm
      docker network rm imboy-network"
}

assert_cleanroom_resources() {
  # 资源门槛（与 deploy/preflight.sh 同口径：内存 ≥8GB，磁盘 ≥20GB；
  # GOLDEN_MIN_MEM_GB / GOLDEN_MIN_DISK_GB 为干跑测试口，生产勿改）
  local mem_kb=0 mem_gb=0 disk_kb=0 disk_gb=0
  if [ -f /proc/meminfo ]; then
    mem_kb="$(awk '/^MemTotal:/ {print $2}' /proc/meminfo)"
  else
    mem_kb="$(sysctl -n hw.memsize 2>/dev/null | awk '{print int($1/1024)}' || echo 0)"
  fi
  mem_gb=$(( mem_kb / 1024 / 1024 ))
  disk_kb="$(df -k "$WORKDIR" 2>/dev/null | tail -1 | awk '{print $4}')"
  disk_gb=$(( disk_kb / 1024 / 1024 ))
  MIN_MEM_GB="${GOLDEN_MIN_MEM_GB:-8}"
  MIN_DISK_GB="${GOLDEN_MIN_DISK_GB:-20}"
  if [ "$mem_gb" -lt "$MIN_MEM_GB" ] || [ "$disk_gb" -lt "$MIN_DISK_GB" ]; then
    die "cleanroom 断言失败：资源不足（内存 ${mem_gb}GB < ${MIN_MEM_GB}GB 或 可用磁盘 ${disk_gb}GB < ${MIN_DISK_GB}GB，
  与 preflight.sh 门槛一致）"
  fi
  ok "资源门槛：内存 ${mem_gb}GB ≥ ${MIN_MEM_GB}GB，可用磁盘 ${disk_gb}GB ≥ ${MIN_DISK_GB}GB"
}

assert_cleanroom_deps() {
  # 冒烟链依赖（curl/jq/openssl/python3+websockets；psql 与 escript 不需要——
  # 分别由 psql wrapper 与容器内 imboy_ctl wrapper 承接）
  local bin
  for bin in git curl jq openssl python3; do
    command -v "$bin" >/dev/null 2>&1 || die "cleanroom 断言失败：缺少依赖 ${bin}（Debian: apt-get install -y ${bin}）"
  done
  python3 -c 'import websockets' >/dev/null 2>&1 \
    || die "cleanroom 断言失败：python3 缺 websockets 包（冒烟步骤 6 需要）。
  安装: pip3 install websockets 或 apt-get install -y python3-websockets"
  ok "冒烟依赖：git curl jq openssl python3(+websockets) 就绪"
}

assert_cleanroom_hosts() {
  # hosts 可写（presign URL host 解析适配；干跑可经 GOLDEN_HOSTS_FILE 重定向）
  if ! { [ -w "$HOSTS_FILE" ] || touch "$HOSTS_FILE" >/dev/null 2>&1; }; then
    die "cleanroom 断言失败：$HOSTS_FILE 不可写（hosts 注入 garage 解析需要 root）"
  fi
  ok "hosts 可写: $HOSTS_FILE"
}
