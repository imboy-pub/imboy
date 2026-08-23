#!/usr/bin/env bash
# Golden Install 金安装门禁 / Golden Install release gate（Golden Gates 计划 §4，P2-G1）
#
# 在 cleanroom（干净 Debian 13 + Docker，不留前置状态）里完整走一遍发布安装并计时：
#
#   git clone（candidate 版本）→ install.sh --edition community（BACKEND_IMAGE 钉
#   candidate digest，不重新 build，§3.1）→ 8 步应用层冒烟链全绿 → restart 幂等
#   验收（§3.3）→ 分环节计时报告 + PASS/FAIL 判定
#
# 计时口径（计划 §4.2 钉死）：
#   T0 = 执行 git clone 前；T1 = attachment download 冒烟 PASS（8 步链全绿）
#   PASS 条件 = (T1 - T0) ≤ 900s（--budget 可调），每次 run 单独判定，不看平均
#   分环节计时：clone / 拉镜像 / 起栈 / TLS 签发 / init / 自检 —— 定位超时点
#
# 两种模式（TLS 是差异点）：
#   --profile host（默认）云主机快照：真域名 + certbot 真签发（install.sh 原路径）
#   --profile ci            CI 容器：无公网域名（客观约束）——假域名 + 预置自签
#                           证书走 install.sh 的"证书已存在，跳过签发"幂等分支，
#                           冒烟直连 127.0.0.1:9800（nginx 443 公网链路不在该模式
#                           验证范围）。差异在报告中显式标注。
#
# 冒烟链自动化的三处环境适配（均为运行现场适配，不改发布物；报告中透明披露）：
#   1) compose override 把 backend IMBOYENV 覆盖为 dev（仅冒烟阶段）：万能验证码
#      与 admin captcha=1234 仅 local/dev/test 放行（pro 需真实验证码，cleanroom
#      无法人工介入）。生产 fail-fast 语义已在 install.sh 原生 pro 起栈阶段验证。
#   2) clone 副本内 scripts/imboy_ctl 替换为容器内转发 wrapper：release 节点
#      vm.args 绑定 inet_dist_use_interface {127,0.0.1}，宿主机 escript 无法直连
#      容器节点；冒烟的 C2C/WS 步骤经 docker compose exec 在容器内执行（与
#      install.sh 超管创建同款手法）。
#   3) /etc/hosts 注入 garage 容器 IP：presign put_url/view_url 的 host 来自
#      IMBOY_GARAGE_ENDPOINT（默认 http://garage:3900，仅 compose 网络内可解析）。
#      Linux Docker 宿主机可直连 bridge 网络容器 IP，hosts 注入让宿主机冒烟脚本
#      同时可达 backend 与 garage。
#
# 用法：
#   bash scripts/golden_install.sh --help
#   make golden-install GOLDEN_ARGS="--image-ref … --git-ref …"
#
# 示例（CI / 无公网域名）：
#   bash scripts/golden_install.sh \
#     --image-ref ghcr.io/imboy-pub/imboy-backend@sha256:abc… \
#     --git-ref v1.0.0-rc.1 --profile ci \
#     --admin-phone 13800138000 --admin-password 'S3curePass2026'
#
# 示例（云主机 / 真域名真签发）：
#   bash scripts/golden_install.sh \
#     --image-ref ghcr.io/imboy-pub/imboy-backend@sha256:abc… \
#     --git-ref v1.0.0-rc.1 --profile host \
#     --api-domain api.example.com --admin-domain admin.example.com \
#     --certbot-email ops@example.com \
#     --admin-phone 13800138000 --admin-password 'S3curePass2026'
#
# 输出（供 CI 消费的机器可解析总结行，最后一行）：
#   GOLDEN_INSTALL=PASS TOTAL=723s BUDGET=900s PROFILE=ci GIT_REF=v1.0.0-rc.1 \
#   IMAGE_DIGEST=sha256:… RESTART_IDEMPOTENT=PASS
#
# 注意：
#   · 脚本须在 cleanroom 内执行；cleanroom 的准备（CI 容器/云主机快照）由调用方
#     负责，脚本开头做 cleanroom 断言，脏环境直接拒绝（防假绿灯）。
#   · host 模式 TLS 走正式签发（init-letsencrypt.sh 当前不支持 --staging）。
#   · --keep 保留现场（clone 目录 + compose 栈 + 日志）便于排查；默认清理。
# -E(errtrace)：ERR trap 穿透函数，任何环节意外失败都能产出 FAIL 总结行
#
# 公共函数库（2026-08-22 P2-U1 抽出）：日志/env 读写/hosts 注入/健康等待/
# wrapper 生成/证书预置/cleanroom 断言/install 执行/8 步链执行统一在
# scripts/golden_common.sh，本脚本与 golden_upgrade.sh 共用（GOLDEN_TAG 区分）。
set -euo pipefail -E

# shellcheck disable=SC2034  # GOLDEN_TAG/GOLDEN_MARK/CURRENT_STAGE/STAGE_HINT 为跨文件接口变量，由 golden_common.sh 的 die()/hosts_* 消费
GOLDEN_TAG=GOLDEN_INSTALL
# shellcheck disable=SC2034  # 同上：跨文件接口变量（golden_common.sh hosts_* 消费）
GOLDEN_MARK=install
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck disable=SC1091  # source 目标含变量路径，shellcheck 无法跟随（文件真实存在）
. "$SCRIPT_DIR/golden_common.sh"

trap 'die "意外失败（line $LINENO, rc=$?）"' ERR

usage() {
  cat <<'EOF'
Golden Install 金安装门禁 / Usage: bash scripts/golden_install.sh --image-ref <digest 引用> --git-ref <tag|sha> [选项]

必填:
  --image-ref <ref>       backend 镜像 digest 引用，必须形如
                          ghcr.io/<owner>/imboy-backend@sha256:<hex>
                          （candidate digest，脚本不重新 build，§3.1）
  --git-ref <tag|sha>     clone 的 git tag 或 40 位 commit sha（与镜像同源版本；
                          digest 引用不含 tag 信息，故必须显式给出）

模式:
  --profile host|ci       host=云主机快照（默认）：真域名 + certbot 真签发
                          ci=CI 容器：假域名 + 预置自签证书（跳过签发），冒烟
                          直连 127.0.0.1:9800（无公网域名的客观约束）

host 模式必填（ci 模式自动生成假域名，可覆盖）:
  --api-domain <domain>   后端 API 域名（A 记录须已指向本机）
  --admin-domain <domain> 管理后台域名
  --certbot-email <email> 证书到期通知邮箱

超管（透传 install.sh，冒烟链步骤 4 需要凭据，必填成对）:
  --admin-phone <手机号>
  --admin-password <明文>  8-64 位且同时包含字母和数字

可选:
  --admin-image-ref <ref> admin 镜像 digest 引用（形如 ghcr.io/<owner>/imboy-admin@sha256:…）。
                          提供则钉入 ADMIN_IMAGE（CI 由 build-admin-candidate 供给）；
                          缺省时 compose 回落默认值 imboy/imboy-admin:<IMBOY_VERSION>
                          ——该镜像须已发布到 registry，否则起栈拉取失败
  --budget <seconds>      计时预算，默认 900（计划 §4.2 钉死值）
  --repo <url>            clone 用的 git 仓库，默认 https://github.com/imboy-pub/imboy.git
  --workdir <dir>         工作目录（clone/日志所在），默认 ./.golden-install
  --keep                  结束后保留现场（clone 目录 + compose 栈 + 日志）
  -h, --help              显示本帮助

环境变量（干跑/测试口）:
  GOLDEN_HOSTS_FILE       hosts 注入目标文件，默认 /etc/hosts
EOF
}

# ── 参数解析（风格与 deploy/install.sh 一致：--opt value 与 --opt=value 皆可）──
IMAGE_REF=""
ADMIN_IMAGE_REF=""
GIT_REF=""
PROFILE="host"
API_DOMAIN=""
ADMIN_DOMAIN=""
CERTBOT_EMAIL=""
ADMIN_PHONE=""
ADMIN_PASSWORD=""
BUDGET="900"
REPO="https://github.com/imboy-pub/imboy.git"
WORKDIR=""
KEEP=0

while [ $# -gt 0 ]; do
  case "$1" in
    --image-ref)        [ $# -ge 2 ] || die "--image-ref 需要值（digest 引用）"; IMAGE_REF="$2"; shift 2 ;;
    --image-ref=*)      IMAGE_REF="${1#*=}"; shift ;;
    --admin-image-ref)  [ $# -ge 2 ] || die "--admin-image-ref 需要值（digest 引用）"; ADMIN_IMAGE_REF="$2"; shift 2 ;;
    --admin-image-ref=*) ADMIN_IMAGE_REF="${1#*=}"; shift ;;
    --git-ref)          [ $# -ge 2 ] || die "--git-ref 需要值（tag 或 40 位 sha）"; GIT_REF="$2"; shift 2 ;;
    --git-ref=*)        GIT_REF="${1#*=}"; shift ;;
    --profile)          [ $# -ge 2 ] || die "--profile 需要值：host|ci"; PROFILE="$2"; shift 2 ;;
    --profile=*)        PROFILE="${1#*=}"; shift ;;
    --api-domain)       [ $# -ge 2 ] || die "--api-domain 需要值"; API_DOMAIN="$2"; shift 2 ;;
    --api-domain=*)     API_DOMAIN="${1#*=}"; shift ;;
    --admin-domain)     [ $# -ge 2 ] || die "--admin-domain 需要值"; ADMIN_DOMAIN="$2"; shift 2 ;;
    --admin-domain=*)   ADMIN_DOMAIN="${1#*=}"; shift ;;
    --certbot-email)    [ $# -ge 2 ] || die "--certbot-email 需要值"; CERTBOT_EMAIL="$2"; shift 2 ;;
    --certbot-email=*)  CERTBOT_EMAIL="${1#*=}"; shift ;;
    --admin-phone)      [ $# -ge 2 ] || die "--admin-phone 需要值（手机号）"; ADMIN_PHONE="$2"; shift 2 ;;
    --admin-phone=*)    ADMIN_PHONE="${1#*=}"; shift ;;
    --admin-password)   [ $# -ge 2 ] || die "--admin-password 需要值（明文密码）"; ADMIN_PASSWORD="$2"; shift 2 ;;
    --admin-password=*) ADMIN_PASSWORD="${1#*=}"; shift ;;
    --budget)           [ $# -ge 2 ] || die "--budget 需要值（秒）"; BUDGET="$2"; shift 2 ;;
    --budget=*)         BUDGET="${1#*=}"; shift ;;
    --repo)             [ $# -ge 2 ] || die "--repo 需要值（git URL）"; REPO="$2"; shift 2 ;;
    --repo=*)           REPO="${1#*=}"; shift ;;
    --workdir)          [ $# -ge 2 ] || die "--workdir 需要值（目录）"; WORKDIR="$2"; shift 2 ;;
    --workdir=*)        WORKDIR="${1#*=}"; shift ;;
    --keep)             KEEP=1; shift ;;
    -h|--help)          usage; exit 0 ;;
    *)                  die "未知参数：$1（--help 查看用法）" ;;
  esac
done

# ── 参数校验 ─────────────────────────────────────────────────────────────────
CURRENT_STAGE="arg"

[ -n "$IMAGE_REF" ] || die "缺少 --image-ref（candidate digest 引用，形如 ghcr.io/<owner>/imboy-backend@sha256:…）"
case "$IMAGE_REF" in
  *@sha256:*) ;;
  *) die "--image-ref 必须是 digest 引用（含 @sha256:…）：当前 ${IMAGE_REF}
  candidate 的不可变 digest 引用可从 release workflow 的 IMAGE_DIGEST_REF output 获取" ;;
esac
EXPECTED_DIGEST="${IMAGE_REF#*@}"

# admin 镜像可选钉入：缺省时回落 compose 默认（要求该 tag 已发布到 registry）
if [ -n "$ADMIN_IMAGE_REF" ]; then
  case "$ADMIN_IMAGE_REF" in
    *@sha256:*) ;;
    *) die "--admin-image-ref 必须是 digest 引用（含 @sha256:…）：当前 ${ADMIN_IMAGE_REF}" ;;
  esac
fi

[ -n "$GIT_REF" ] || die "缺少 --git-ref（clone 哪个版本）
  digest 引用不含 tag 信息，无法自动推导；请传入与镜像同源的 git tag（如 v1.0.0-rc.1）或 40 位 commit sha"

case "$PROFILE" in
  host|ci) ;;
  *) die "--profile 仅支持 host|ci（当前: ${PROFILE}）" ;;
esac

{ [ -n "$ADMIN_PHONE" ] && [ -n "$ADMIN_PASSWORD" ]; } \
  || die "--admin-phone 与 --admin-password 必须成对提供（冒烟链步骤 4 的超管凭据，golden 强制）"
if [ "${#ADMIN_PASSWORD}" -lt 8 ] || [ "${#ADMIN_PASSWORD}" -gt 64 ] \
   || ! printf '%s' "$ADMIN_PASSWORD" | grep -q '[A-Za-z]' \
   || ! printf '%s' "$ADMIN_PASSWORD" | grep -q '[0-9]'; then
  die "--admin-password 强度不足：需 8-64 位且同时包含字母和数字（与 install.sh 口径一致）"
fi

case "$BUDGET" in
  ''|*[!0-9]*) die "--budget 必须是正整数秒（当前: ${BUDGET}）" ;;
esac

if [ "$PROFILE" = "host" ]; then
  { [ -n "$API_DOMAIN" ] && [ -n "$ADMIN_DOMAIN" ] && [ -n "$CERTBOT_EMAIL" ]; } \
    || die "host 模式必须提供 --api-domain / --admin-domain / --certbot-email（真域名真签发）
  （CI 无公网域名场景请用 --profile ci）"
else
  # ci 模式：假域名（.internal 保留 TLD 不可能被公网解析，防误伤真实域名）
  API_DOMAIN="${API_DOMAIN:-api.golden-ci.imboy.internal}"
  ADMIN_DOMAIN="${ADMIN_DOMAIN:-admin.golden-ci.imboy.internal}"
  CERTBOT_EMAIL="${CERTBOT_EMAIL:-golden-ci@imboy.invalid}"
fi

# ── 目录与全局状态 ───────────────────────────────────────────────────────────
HOSTS_FILE="${GOLDEN_HOSTS_FILE:-/etc/hosts}"
WORKDIR="${WORKDIR:-$PWD/.golden-install}"
CLONE_DIR="$WORKDIR/imboy"
DEPLOY_DIR="$CLONE_DIR/deploy"
COMMUNITY_YML="$DEPLOY_DIR/docker-compose.community.yml"
OVERRIDE_YML="$DEPLOY_DIR/docker-compose.golden-override.yml"
RUN_DIR="$WORKDIR/run-$(date +%Y%m%d%H%M%S)"
INSTALL_LOG="$RUN_DIR/install_run.log"
BIN_DIR="$RUN_DIR/bin"
mkdir -p "$WORKDIR" "$RUN_DIR" "$BIN_DIR"
: > "$INSTALL_LOG"

COMPOSE="docker compose -f $COMMUNITY_YML"
COMPOSE_O="docker compose -f $COMMUNITY_YML -f $OVERRIDE_YML"
BASE_URL="http://127.0.0.1:9800"

CURRENT_STAGE=""
STAGE_HINT=""

# 分环节秒数（计划 §4.2 六环节 + golden 编排细分；bash 3.2 兼容，不用关联数组）
SEC_CLONE=0; SEC_ENVINIT=0; SEC_PULL=0; SEC_STACK=0; SEC_TLS=0; SEC_INITTAIL=0
SEC_SANITY=0; SEC_SMOKE8=0; SEC_IDEM=0
TLS_NOTE=""

# ── 通用 helper：见 golden_common.sh（env_set_var/env_get_var/hosts_*/wait_healthz）──

# ── cleanroom 断言与现场适配：见 golden_common.sh（assert_cleanroom/
#    write_compose_override/install_ctl_wrapper/make_psql_wrapper/preset_ci_certs）

# ── 安装执行与 8 步链：见 golden_common.sh（anchor_ts/run_install_phase2/
#    parse_install_stages/sanity_equivalent/run_smoke8）──

# ── restart 幂等验收（§3.3：install→upload→restart→download 成功）──────────
idem_body="$RUN_DIR/idem.txt"
IDEM_CONTENT="GOLDEN-IDEM-ROUNDTRIP"

idem_http() { # <method> <url> [curl 参数…] → HTTP 码；body 落 $RUN_DIR/idem_body
  local method="$1" url="$2"; shift 2
  curl -sS -m 15 -o "$idem_body" -w '%{http_code}' -X "$method" "$@" "$url" 2>/dev/null || printf '000'
}
idem_json() { jq -r "$1" "$idem_body" 2>/dev/null || true; }

idem_upload() { # restart 前：register→login→presign→PUT→confirm，凭据落盘
  local email pwd code token put_url obj_key md5 size
  email="golden-idem-$(date +%s)@smoke.local"
  pwd="Golden1d-$(openssl rand -hex 4)"
  code="$(idem_http POST "$BASE_URL/api/v1/passport/signup" \
    -H 'Content-Type: application/x-www-form-urlencoded' \
    --data-urlencode 'type=email' --data-urlencode "account=$email" \
    --data-urlencode "pwd=$pwd" --data-urlencode 'code=abc12345' \
    --data-urlencode 'rsa_encrypt=0' --data-urlencode 'nickname=golden-idem')"
  [ "$code" = "200" ] && [ "$(idem_json '.code')" = "0" ] \
    || { STAGE_HINT="幂等段 register HTTP $code body=$(head -c 200 "$idem_body")"; return 1; }
  code="$(idem_http POST "$BASE_URL/api/v1/passport/login" \
    -H 'Content-Type: application/x-www-form-urlencoded' \
    --data-urlencode 'type=email' --data-urlencode "account=$email" \
    --data-urlencode "pwd=$pwd" --data-urlencode 'rsa_encrypt=0')"
  token="$(idem_json '.payload.token // empty')"
  [ "$code" = "200" ] && [ -n "$token" ] \
    || { STAGE_HINT="幂等段 login HTTP $code"; return 1; }
  code="$(idem_http GET "$BASE_URL/api/v1/attachment/presign?filename=golden-idem.txt&mime_type=text/plain" \
    -H "Authorization: Bearer $token")"
  put_url="$(idem_json '.payload.put_url // .data.put_url // empty')"
  obj_key="$(idem_json '.payload.object_key // .data.object_key // empty')"
  [ "$code" = "200" ] && [ -n "$put_url" ] && [ -n "$obj_key" ] \
    || { STAGE_HINT="幂等段 presign HTTP $code body=$(head -c 200 "$idem_body")"; return 1; }
  printf '%s' "$IDEM_CONTENT" > "$RUN_DIR/idem_payload.txt"
  code="$(idem_http PUT "$put_url" -H 'Content-Type: text/plain' --data-binary @"$RUN_DIR/idem_payload.txt")"
  { [ "$code" = "200" ] || [ "$code" = "204" ]; } \
    || { STAGE_HINT="幂等段 S3 PUT HTTP ${code}（hosts 的 garage 解析是否生效: grep garage ${HOSTS_FILE}）"; return 1; }
  md5="$(openssl md5 -r "$RUN_DIR/idem_payload.txt" | awk '{print $1}')"
  size="$(wc -c < "$RUN_DIR/idem_payload.txt" | tr -d ' ')"
  code="$(idem_http POST "$BASE_URL/api/v1/attachment/confirm" \
    -H "Authorization: Bearer $token" -H 'Content-Type: application/json' \
    -d "{\"object_key\":\"$obj_key\",\"md5\":\"$md5\",\"mime_type\":\"text/plain\",\"size\":$size}")"
  [ "$code" = "200" ] && [ "$(idem_json '.code')" = "0" ] \
    || { STAGE_HINT="幂等段 confirm HTTP $code body=$(head -c 200 "$idem_body")"; return 1; }
  printf 'IDEM_TOKEN=%q\nIDEM_OBJ_KEY=%q\n' "$token" "$obj_key" > "$RUN_DIR/idem.vars"
  ok "幂等段 upload 完成（restart 前恰有一次完整上传，§3.3 顺序）"
}

idem_download() { # restart 后：view_url→GET→逐字节比对
  local token obj_key code vurl
  # shellcheck disable=SC1090,SC1091  # 凭据为本脚本 idem_upload 落盘（运行时生成）
  . "$RUN_DIR/idem.vars"
  code="$(idem_http GET "$BASE_URL/api/v1/attachment/view_url?object_key=$IDEM_OBJ_KEY" \
    -H "Authorization: Bearer $IDEM_TOKEN")"
  vurl="$(idem_json '.payload.url // .data.url // empty')"
  [ "$code" = "200" ] && [ -n "$vurl" ] \
    || { STAGE_HINT="幂等段 view_url HTTP $code body=$(head -c 200 "$idem_body")"; return 1; }
  code="$(idem_http GET "$vurl")"
  { [ "$code" = "200" ] && cmp -s "$RUN_DIR/idem_payload.txt" "$idem_body"; } \
    || { STAGE_HINT="幂等段 download HTTP ${code}（Garage 数据跨 restart 保留失败——§3.3 不合格。
      卷: docker volume ls | grep imboy；garage 日志: $COMPOSE_O logs --tail=50 imboy_garage）"; return 1; }
  ok "幂等段 download 成功：restart 后内容逐字节一致（${#IDEM_CONTENT}B round-trip）"
}

# ── 清理 ─────────────────────────────────────────────────────────────────────
cleanup() {
  hosts_del_garage 2>/dev/null || true
  if [ "$KEEP" = 0 ]; then
    say "清理现场（--keep 可保留）"
    $COMPOSE_O down --remove-orphans -v >/dev/null 2>&1 || true
    docker network rm imboy-network >/dev/null 2>&1 || true
    rm -rf "$CLONE_DIR"
    ok "已清理：compose 栈 / imboy-network / clone 目录 / hosts 行（日志保留 ${RUN_DIR}）"
  else
    say "现场保留（--keep）"
    printf '  clone=%s\n  日志=%s\n  栈状态: %s ps\n' "$CLONE_DIR" "$RUN_DIR" "$COMPOSE_O"
    warn "hosts 里的 golden-install garage 行请用完手工删除: grep -v '# golden-install garage' $HOSTS_FILE"
  fi
}

# ── 报告 ─────────────────────────────────────────────────────────────────────
report() { # <verdict> <total> <idem: PASS|FAIL>
  local verdict="$1" total="$2" idem="$3"
  printf '\n'
  printf '\033[1;36m════════════ Golden Install 分环节计时（§4.2）════════════\033[0m\n'
  printf '%-34s %8s  %s\n' '环节' '秒' '备注'
  printf '%-34s %8s  %s\n' 'clone'                    "$SEC_CLONE"    'git clone（--branch/--depth 1）'
  printf '%-34s %8s  %s\n' 'init（env 配置）'          "$SEC_ENVINIT"  '.env 生成+3 变量+BACKEND_IMAGE+证书预置+wrapper'
  printf '%-34s %8s  %s\n' '拉镜像（pull）'            "$SEC_PULL"     'docker compose pull（7 核心镜像）'
  printf '%-34s %8s  %s\n' '起栈（stack up）'          "$SEC_STACK"    'install.sh: 创建网络并启动服务'
  printf '%-34s %8s  %s\n' 'TLS 签发'                  "$SEC_TLS"      "${TLS_NOTE:-—}"
  printf '%-34s %8s  %s\n' 'init（安装收尾）'           "$SEC_INITTAIL" '等健康+sanity+超管创建（install.sh 内）'
  printf '%-34s %8s  %s\n' '自检（sanity 等价）'        "$SEC_SANITY"   'community 口径 8 项等价自检'
  printf '%-34s %8s  %s\n' '自检（8 步链 smoke）'       "$SEC_SMOKE8"   '冒烟准备+8 步链全绿（T1 截止）'
  printf '%-34s %8s  %s\n' '幂等段（restart 验收）'     "$SEC_IDEM"     '§3.3：T1 之后，不计入 T0-T1 预算'
  printf '\n'
  printf '  T0=%s（clone 前） T1=attachment download 冒烟 PASS\n' "$T0"
  printf '  总耗时 TOTAL(T1-T0)=%ss  预算 BUDGET=%ss  → %s\n' \
    "$total" "$BUDGET" "$verdict"
  printf '\n  ── Release Identity（install.sh 输出捕获）──\n'
  printf '  IMBOY_VERSION=%s\n' "${INST_VERSION:-unknown}"
  printf '  IMBOY_GIT_SHA=%s（--git-ref=%s）\n' "${INST_GIT_SHA:-unknown}" "$GIT_REF"
  printf '  IMBOY_IMAGE_DIGEST=%s\n' "${INST_DIGEST:-unknown}"
  printf '  digest 一致性: %s（--image-ref=%s）\n' "${DIGEST_CHECK:-?}" "$IMAGE_REF"
  printf '  restart 幂等（§3.3）: %s\n' "$idem"
  if [ "$PROFILE" = "ci" ]; then
    printf '\n  ── ci 模式与 §4.1 的差异（客观约束，透明披露）──\n'
    printf '  · 无公网域名：假域名 %s/%s + 预置自签证书（install.sh 幂等跳过签发）\n' "$API_DOMAIN" "$ADMIN_DOMAIN"
    printf '  · 冒烟直连 %s（nginx 443 公网链路未验证；admin login 走 BASE_URL 等价路径）\n' "$BASE_URL"
    printf '  · backend IMBOYENV=dev（override，仅冒烟阶段）：万能码/captcha 自动化前提\n'
  fi
  printf '\n'
  printf 'GOLDEN_INSTALL=%s TOTAL=%ss BUDGET=%ss PROFILE=%s GIT_REF=%s IMAGE_DIGEST=%s RESTART_IDEMPOTENT=%s\n' \
    "$verdict" "$total" "$BUDGET" "$PROFILE" "$GIT_REF" "$EXPECTED_DIGEST" "$idem"
}

# ═════════════════════════════════════════════════════════════════════════════
# 主流程
# ═════════════════════════════════════════════════════════════════════════════

say "Golden Install（profile=$PROFILE budget=${BUDGET}s）"
printf '  image-ref: %s\n  git-ref:   %s\n  repo:      %s\n  workdir:   %s\n' \
  "$IMAGE_REF" "$GIT_REF" "$REPO" "$WORKDIR"

# 1) cleanroom 断言（T0 之前，不计时）
CURRENT_STAGE="cleanroom-assert"
STAGE_HINT="按上方第一条 [ERROR]/❌ 提示修复环境后重跑。"
say "cleanroom 断言"
assert_cleanroom

# 2) T0 → clone
CURRENT_STAGE="clone"
STAGE_HINT="网络/权限: git ls-remote ${REPO}；tag 是否存在: git ls-remote --tags $REPO | grep $GIT_REF"
T0=$(now)
say "[clone] git clone（--branch ${GIT_REF}）"
t=$(now)
if printf '%s' "$GIT_REF" | grep -Eq '^[0-9a-f]{40}$'; then
  git init -q "$CLONE_DIR"
  git -C "$CLONE_DIR" remote add origin "$REPO"
  git -C "$CLONE_DIR" fetch -q --depth 1 origin "$GIT_REF"
  git -C "$CLONE_DIR" checkout -q FETCH_HEAD
else
  git clone -q --depth 1 --branch "$GIT_REF" "$REPO" "$CLONE_DIR"
fi
[ -f "$DEPLOY_DIR/install.sh" ] || die "clone 完成但缺少 deploy/install.sh（仓库不完整？）"
SEC_CLONE=$(( $(now) - t ))

# 3) init（env 配置）：两段式 install.sh 第一段生成 .env → 填变量 → 预置 → wrapper
CURRENT_STAGE="env-init"
STAGE_HINT="第一段 install.sh 失败看上方输出；常见: openssl 缺失 / .env.example 缺失"
say "[init] 生成 .env 并配置（install.sh 两段式第一段）"
t=$(now)
bash "$DEPLOY_DIR/install.sh" --edition community --yes >/dev/null
[ -f "$DEPLOY_DIR/.env" ] || die "install.sh 第一段未生成 .env"
env_set_var API_DOMAIN "$API_DOMAIN"
env_set_var ADMIN_DOMAIN "$ADMIN_DOMAIN"
env_set_var CERTBOT_EMAIL "$CERTBOT_EMAIL"
env_set_var BACKEND_IMAGE "$IMAGE_REF"
if [ -n "$ADMIN_IMAGE_REF" ]; then env_set_var ADMIN_IMAGE "$ADMIN_IMAGE_REF"; fi
if [ "$PROFILE" = "ci" ]; then preset_ci_certs; fi
write_compose_override
install_ctl_wrapper
make_psql_wrapper
SEC_ENVINIT=$(( $(now) - t ))

# 4) 拉镜像（独立环节，install.sh 的 up -d 复用本地镜像）
CURRENT_STAGE="pull"
STAGE_HINT="拉取失败: registry 凭据（ghcr.io 公开镜像无需凭据）/ digest 是否已推送（release workflow 的 push 步骤）"
say "[pull] docker compose pull（digest 钉住的 candidate，不重新 build）"
t=$(now)
( cd "$DEPLOY_DIR" && $COMPOSE pull ) || die "docker compose pull 失败"
SEC_PULL=$(( $(now) - t ))

# 5) 安装（install.sh 两段式第二段：起栈→TLS→等健康→sanity→超管→三元组）
CURRENT_STAGE="install"
STAGE_HINT="install.sh 失败。完整日志: $INSTALL_LOG"
say "[install] bash install.sh --edition community --yes（透传超管参数）"
t=$(now)
run_install_phase2
INSTALL_END=$(now)

# 6) install 后置检查：三元组捕获 + digest 一致性断言（§3.1）
CURRENT_STAGE="post-install-check"
INST_VERSION="$(grep -o 'IMBOY_VERSION=[^ ]*' "$INSTALL_LOG" | head -1 | cut -d= -f2 || true)"
INST_GIT_SHA="$(grep -o 'IMBOY_GIT_SHA=[0-9a-f]*' "$INSTALL_LOG" | head -1 | cut -d= -f2 || true)"
INST_DIGEST="$(grep -o 'IMBOY_IMAGE_DIGEST=sha256:[0-9a-f]*' "$INSTALL_LOG" | head -1 | cut -d= -f2 || true)"
if [ "$INST_DIGEST" = "$EXPECTED_DIGEST" ]; then
  DIGEST_CHECK="PASS（装的就是被验证的 candidate）"
else
  DIGEST_CHECK="FAIL"
  die "digest 一致性断言失败（§3.1）: install 运行=${INST_DIGEST:-未捕获} vs --image-ref=${EXPECTED_DIGEST}
  可能原因: BACKEND_IMAGE 未生效（.env 被 install.sh 覆盖？）/ 镜像源不一致"
fi
ok "三元组: VERSION=${INST_VERSION:-?} SHA=${INST_GIT_SHA:-?} DIGEST=${INST_DIGEST:-?}"
parse_install_stages "$INSTALL_END"

# 7) 自检（sanity 等价，community 口径）
CURRENT_STAGE="sanity"
STAGE_HINT="见 sanity 等价自检的 [WARN]/❌ 项"
say "[sanity] 等价自检（community 口径；sanity_check.sh 硬编码 prod.yml 不适用，见报告）"
t=$(now)
sanity_equivalent || die "sanity 等价自检未通过"
SEC_SANITY=$(( $(now) - t ))

# 8) 自检（8 步链）：冒烟准备（IMBOYENV=dev 重建 backend + hosts garage）→ smoke → T1
CURRENT_STAGE="smoke8"
# shellcheck disable=SC2034  # 跨文件接口变量（golden_common.sh die() 消费）
STAGE_HINT="见 run_smoke8 的输出与提示"
say "[smoke8] 冒烟准备（override IMBOYENV=dev 重建 backend + hosts 注入 garage）"
t=$(now)
( cd "$DEPLOY_DIR" && $COMPOSE_O up -d ) || die "override up 失败（backend IMBOYENV=dev 重建）"
wait_healthz 180 "（backend 已按冒烟配置重建）" || die "backend 冒烟配置重建后未就绪"
hosts_set_garage || die "hosts 注入 garage 解析失败"
say "[smoke8] 8 步链（Health→Register→Login→Admin Login→C2C→WebSocket→Upload→Download）"
run_smoke8 || die "8 步冒烟链未全绿"
T1=$(now)
SEC_SMOKE8=$(( T1 - t ))
ok "T1 打点：attachment download 冒烟 PASS（T1-T0=$((T1 - T0))s）"

# 9) restart 幂等验收（§3.3；T1 之后，不计入预算）
CURRENT_STAGE="idempotency"
say "[idempotency] restart 幂等验收：upload → down/up → download（§3.3）"
t=$(now)
idem_upload || die "幂等段 upload（restart 前）失败"
say "[idempotency] docker compose down && up -d（restart stack）"
( cd "$DEPLOY_DIR" && $COMPOSE_O down ) || die "幂等段 compose down 失败"
( cd "$DEPLOY_DIR" && $COMPOSE_O up -d ) || die "幂等段 compose up 失败"
wait_healthz 180 "（restart 后）" || die "restart 后 backend 未就绪"
hosts_set_garage || die "restart 后 hosts 更新 garage 失败（容器 IP 已变化）"
idem_download || die "幂等段 download（restart 后）失败——§3.3 不合格"
IDEM_RESULT="PASS"
SEC_IDEM=$(( $(now) - t ))

# 10) 判定 + 报告 + 清理
# shellcheck disable=SC2034  # 跨文件接口变量（golden_common.sh die() 消费）
CURRENT_STAGE="verdict"
TOTAL=$(( T1 - T0 ))
if [ "$TOTAL" -le "$BUDGET" ]; then
  VERDICT="PASS"
else
  VERDICT="FAIL"
fi
report "$VERDICT" "$TOTAL" "$IDEM_RESULT"

cleanup

[ "$VERDICT" = "PASS" ] || exit 1
exit 0
