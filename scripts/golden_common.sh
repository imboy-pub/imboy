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
#   COMPOSE / COMPOSE_O  compose 命令前缀（后者叠加 golden override）
#   OVERRIDE_YML  golden override compose 输出路径
#   CLONE_DIR     clone 目录（imboy_ctl wrapper 替换目标）
#   BIN_DIR       运行期生成 bin（psql wrapper）目录
#   RUN_DIR       本次 run 的日志/状态目录；INSTALL_LOG 安装打点日志
#   BASE_URL      后端冒烟基址（wait_healthz / run_smoke8）
#   WS_URL        WebSocket 冒烟基址（run_smoke8）
#   WORKDIR       cleanroom 残留断言用工作目录
#   ADMIN_PHONE / ADMIN_PASSWORD / GIT_REF  run_smoke8 透传超管凭据与 ref
#   API_DOMAIN / ADMIN_DOMAIN  ci 模式证书预置域名
#   CURRENT_STAGE / STAGE_HINT / KEEP / PROFILE  die() 排查提示与输出字段（可选）
# 可选测试口（干跑/资源门槛覆盖，生产勿设）：
#   GOLDEN_HOSTS_FILE / GOLDEN_MIN_MEM_GB / GOLDEN_MIN_DISK_GB
#   SMOKE_C2C_FROM / SMOKE_C2C_TO  run_smoke8 的 C2C 双方 uid 覆盖（默认走
#   smoke_8step.sh 内置 1000000051→1000000056）

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
  tmp="$(mktemp)"
  if grep -qE "^${key}=" "$DEPLOY_DIR/.env" 2>/dev/null; then
    awk -v k="$key" -v v="$val" 'BEGIN{FS=OFS="="} $1==k{print k"="v; next} {print}' \
        "$DEPLOY_DIR/.env" > "$tmp"
  else
    awk -v k="$key" -v v="$val" '{print} END{print k"="v}' "$DEPLOY_DIR/.env" > "$tmp"
  fi
  mv "$tmp" "$DEPLOY_DIR/.env"
}
env_get_var() { grep -E "^$1=" "$DEPLOY_DIR/.env" 2>/dev/null | head -1 | cut -d= -f2- || true; }

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
    --admin-phone "$ADMIN_PHONE" --admin-password "$ADMIN_PASSWORD" \
    2>&1 | while IFS= read -r line; do
        printf '%s\n' "$line"
        printf '%s %s\n' "$(now)" "$line" >&3
      done 3>>"$INSTALL_LOG" || rc=$?
  [ "$rc" -eq 0 ] || { capture_backend_failure_log; STAGE_HINT="install.sh 退出码 ${rc}。完整日志: $INSTALL_LOG
      容器状态: $COMPOSE ps
      后端日志: $RUN_DIR/backend_failure.log"; return "$rc"; }
}

# 从打点日志提取 install.sh 内部环节（say() 锚点行；缺失则记 0 并注明）
# 结果写入调用方可见的 SEC_STACK/SEC_TLS/SEC_INITTAIL/TLS_NOTE
# shellcheck disable=SC2034  # 计时变量由调用方（golden_install.sh report 等）读取
parse_install_stages() {
  local a_stack a_tls a_tls_skip a_wait install_end
  a_stack="$(anchor_ts '创建网络并启动服务')"
  a_tls="$(anchor_ts '签发 TLS 证书')"
  a_tls_skip="$(anchor_ts 'TLS 证书已存在')"
  a_wait="$(anchor_ts '等待后端启动')"
  install_end="${1:-0}"

  if [ -n "$a_stack" ] && { [ -n "$a_tls" ] || [ -n "$a_tls_skip" ]; }; then
    if [ -n "$a_tls" ]; then
      SEC_STACK=$(( a_tls - a_stack ))
      TLS_NOTE="正式签发（Let's Encrypt）"
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

# ── 8 步冒烟链执行（P3-C4 的 smoke_8step.sh；ENV_PASSTHROUGH 见函数体）───────
run_smoke8() {
  local rc=0
  printf '\n'
  ( cd "$CLONE_DIR" && \
    PATH="$BIN_DIR:$PATH" \
    BASE_URL="$BASE_URL" \
    WS_URL="${WS_URL:-ws://127.0.0.1:9800/api/v1/ws}" \
    SMOKE_ADMIN_ACCOUNT="$ADMIN_PHONE" \
    SMOKE_ADMIN_PASSWORD="$ADMIN_PASSWORD" \
    SMOKE_C2C_FROM="${SMOKE_C2C_FROM:-}" \
    SMOKE_C2C_TO="${SMOKE_C2C_TO:-}" \
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
}

install_ctl_wrapper() {
  # 替换 clone 副本内的 scripts/imboy_ctl 为容器内转发 wrapper。
  # smoke_8step.sh 以绝对路径引用该文件（PATH 注入无效），故只能就地替换；
  # 原文件保留为 imboy_ctl.golden-real。
  if [ -f "$CLONE_DIR/scripts/imboy_ctl.golden-real" ]; then
    mv -f "$CLONE_DIR/scripts/imboy_ctl.golden-real" "$CLONE_DIR/scripts/imboy_ctl"
  fi
  mv "$CLONE_DIR/scripts/imboy_ctl" "$CLONE_DIR/scripts/imboy_ctl.golden-real"
  cat > "$CLONE_DIR/scripts/imboy_ctl" <<EOF
#!/usr/bin/env bash
# ${GOLDEN_TAG:-golden} 生成的容器内转发 wrapper（原实现: imboy_ctl.golden-real）。
# 原因：release 节点 vm.args 为 -kernel inet_dist_use_interface {127,0,0,1}，
# 宿主机 escript 无法直连容器节点；与 install.sh 超管创建同款手法：容器内
# ERTS 执行（镜像已 COPY /opt/imboy/bin/imboy_ctl）。
exec docker compose -f '$COMMUNITY_YML' exec -T \\
  -e IMBOY_CTL_NODE='imboy_dev@127.0.0.1' \\
  -e IMBOY_CTL_COOKIE='imboycookie' \\
  imboy_backend \\
  sh -c 'exec /opt/imboy/erts-*/bin/escript /opt/imboy/bin/imboy_ctl "\$@"' golden "\$@"
EOF
  chmod +x "$CLONE_DIR/scripts/imboy_ctl"
}

make_psql_wrapper() {
  # smoke_8step.sh 的 C2C 落库断言用宿主机 psql；cleanroom 不强制安装
  # postgresql-client，改为转发到 pg 容器内执行（-e 从调用方 env 取值）。
  cat > "$BIN_DIR/psql" <<EOF
#!/usr/bin/env bash
# ${GOLDEN_TAG:-golden} 生成：psql -> docker compose exec imboy_pg18 psql
exec docker compose -f '$COMMUNITY_YML' exec -T \\
  -e PGPASSWORD -e PGUSER -e PGDATABASE imboy_pg18 psql "\$@"
EOF
  chmod +x "$BIN_DIR/psql"
}

preset_ci_certs() {
  # ci 模式 TLS bypass：为两个假域名预置 1 天期自签证书，让 install.sh 走
  # "TLS 证书已存在，跳过签发" 的幂等分支（不触发 certbot 向公网申请）。
  # nginx 引用 /etc/letsencrypt/live/<domain>/{fullchain,privkey}.pem，两域名都要。
  local data_dir base d
  data_dir="$(resolve_data_dir)"
  base="$data_dir"
  for d in "$API_DOMAIN" "$ADMIN_DOMAIN"; do
    mkdir -p "$base/certbot/conf/live/$d"
    openssl req -x509 -nodes -newkey rsa:2048 -days 1 \
      -keyout "$base/certbot/conf/live/$d/privkey.pem" \
      -out    "$base/certbot/conf/live/$d/fullchain.pem" \
      -subj "/CN=$d" 2>/dev/null
  done
  printf '  ci 模式：已预置自签证书（%s/certbot/conf/live/{%s,%s}）\n' \
    "$base" "$API_DOMAIN" "$ADMIN_DOMAIN"
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
