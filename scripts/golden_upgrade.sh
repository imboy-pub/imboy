#!/usr/bin/env bash
# Golden Upgrade 升级门禁 / Golden Upgrade release gate（Golden Gates 计划 §5，P2-U1）
#
# 在 cleanroom（干净 Debian 13 + Docker，不留前置状态）完整走一遍 vN → vN+1 升级
# 并断言历史数据保留（§5 钉死流程）：安装 vN（BACKEND_IMAGE 钉基线镜像）→ 产生
# 真实数据（账号 A/B、N 条 C2C 消息、附件 round-trip、建群+群消息、好友）→
# 升级 vN+1（--to-image candidate digest + --to-ref checkout + compose up）→
# auto_migrate 观察（日志锚点 + schema_migrations 权威断言）→ 8 步冒烟链全绿
# → 数据保留断言（账号可登录/消息可读且内容一致/附件可下载且 md5 一致/群存在
# 且群消息可读/好友仍在/pg+garage 数据卷未变）→ PASS。
#
# 计时：报告总耗时与分环节耗时，**无 900s 预算**——计划 §4.2 的 T0/T1≤900s 仅
# 钉死 Golden Install；Golden Upgrade 含 vN 全量安装+数据生成+迁移，PASS 判定
# 不含时限（总耗时报给质量债务总账做长期追踪），故不设 --budget 参数。
#
# 与 golden_install.sh（P2-G1）共用 scripts/golden_common.sh（cleanroom 断言、
# 现场适配 wrapper、install 执行、8 步链执行；GOLDEN_TAG=GOLDEN_UPGRADE）。
# 群消息发送器：scripts/smoke/ws_c2g_send.py（本脚本所在检出的副本）。
#
# 数据生成/断言使用的 API（均从 src/ 源码逐一核实，禁止臆造）：
#   signup|login            src/api/passport_handler.erl
#   msg/history             src/api/msg_handler.erl history → messaging_logic:history/5
#                           （chat_type=c2c|c2g；msg_archive_enabled 默认 true）
#   attachment/presign|confirm|view_url   src/api/attach_handler.erl
#   group/add|detail        src/api/group_handler.erl（member_uids 须 JSON 字符串数组）
#   friend/add|confirm|list  src/api/friend_handler.erl（add 的 to/payload/created_at 必填）
#   C2C 发送                无 REST：imboy_ctl msg send（RPC，同 8 步链）
#   C2G 发送                WS JSON 帧：scripts/smoke/ws_c2g_send.py
#                           （websocket_handler → message_router_logic → msg_c2g_logic）
#   auto_migrate 锚点       imboy_app:maybe_migrate → imboy_migrate:migrate：
#                           "running migrations from" / "all migrations applied" /
#                           "automatic migrations disabled"
#
# 注意：
#   · vN 基线对上述端点的兼容性属假设（本地只有当前工作区代码）；数据生成段在
#     vN 上就地验证每个端点，不支持即 FAIL 并指明端点（升级门禁语境下"基线调
#     不通"本身就是结论；不臆造、不静默跳过）。
#   · 与 G1 相同的三处现场适配（IMBOYENV=dev override / imboy_ctl 容器内
#     wrapper / hosts 注入 garage），报告中透明披露。
#   · --keep 保留现场；默认清理（down -v 不删 bind mount 数据目录，但删 clone）。
# -E(errtrace)：ERR trap 穿透函数，任何环节意外失败都能产出 FAIL 总结行
set +x
set +v
set -euo pipefail -E

# shellcheck disable=SC2034  # GOLDEN_TAG/GOLDEN_MARK/CURRENT_STAGE/STAGE_HINT 为跨文件接口变量，由 golden_common.sh 的 die()/hosts_* 消费
GOLDEN_TAG=GOLDEN_UPGRADE
# shellcheck disable=SC2034  # 同上：跨文件接口变量（golden_common.sh hosts_* 消费）
GOLDEN_MARK=upgrade
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck disable=SC1091  # source 目标含变量路径，shellcheck 无法跟随（文件真实存在）
. "$SCRIPT_DIR/golden_common.sh"

trap 'die "意外失败（line $LINENO, rc=$?）"' ERR

usage() {
  cat <<'EOF'
Golden Upgrade 升级门禁 / Usage: bash scripts/golden_upgrade.sh --to-image <digest 引用> --to-ref <tag|sha> [选项]

必填（升级目标）:
  --to-image <ref>     vN+1 candidate digest 引用：ghcr.io/<owner>/imboy-backend@sha256:<hex>
                       （§3.1 不可变；tag 引用会漂移，不能作为门禁输入）
  --to-ref <tag|sha>   升级目标代码 git tag 或 40 位 sha（checkout 该版本的
                       deploy/ 与 priv/migrations；与镜像同源）

基线 vN（二选一）:
  --from <vN tag>      上一稳定 tag；基线镜像自动推导为与 --to-image 同
                       registry/owner 的 imboy-backend:<version>（v1.0.0 → :1.0.0，仅去 v）
                       Keep the Git tag; remove only its leading v for the image tag.
  --from-image <ref> + --from-ref <tag|sha>
                       显式指定基线镜像与代码（首个正式版发布前无上一稳定 tag 时
                       用；两者必须同时给出）。与 --from 互斥。

模式:  --profile ci|host   ci=CI 容器（默认）：假域名+预置自签证书，冒烟直连
                           127.0.0.1:9800；host=云主机：真域名+certbot 真签发
host 模式必填:  --api-domain / --admin-domain / --cs-widget-domain / --rtc-domain
               / --turn-domain <各自域名>（五域互异） / --certbot-email <邮箱>
超管（vN 安装与升级后 8 步链步骤 4 共用，必填成对）:
  --admin-phone <手机号> / --admin-password <明文>（8-64 位含字母和数字）
可选:  --admin-image-ref <ref>（admin 镜像 digest 引用；提供则在 vN 安装与升级后
                            起栈时都钉入 ADMIN_IMAGE——基线版本的 admin 镜像可能
                            从未发布过，CI 由 build-admin-candidate 供给）
       --pg-image-ref <ref>（PostgreSQL 18 镜像 digest 引用；提供则在 vN 安装与
                            升级后都钉入 PG_IMAGE，避免依赖未发布的 Docker Hub 镜像）
       --widget-image-ref <ref>（Widget 完整 sha256 digest 引用；两个阶段均钉入 IMBOY_WIDGET_IMAGE）
                               Pin the same Widget candidate in both stages; omission keeps defaults.
       --repo <url>（默认 https://github.com/imboy-pub/imboy.git）
       --workdir <dir>（默认 ./.golden-upgrade） / --keep（保留现场） / -h

Golden cleanroom 准备与 Install 共用：五域、独立清理凭据、随机 release cookie。
IMBOY_DIRECT_CLEANUP_PG_USER / IMBOY_DIRECT_CLEANUP_PG_PASSWORD 和
IMBOY_CTL_NODE / IMBOY_CTL_COOKIE 可提供有效值。清理密码至少 32 位，cookie
为 32–255 位，均仅含字母、数字、下划线或连字符；清理密码不接受占位值，升级时不换。
Both source refs must support the installer's --compose-file / --init-cleanup-db contract.

环境变量（干跑/测试口）:  GOLDEN_HOSTS_FILE（hosts 注入目标，默认 /etc/hosts）
                         GOLDEN_UP_MSG_COUNT（数据生成段 C2C 消息条数，默认 3，1-10）
计时：报告总耗时与分环节耗时；无预算判定（§4.2 的 900s 仅钉死 Golden Install）。
EOF
}

FROM=""
FROM_IMAGE=""
FROM_REF=""
TO_IMAGE=""
TO_REF=""
ADMIN_IMAGE_REF=""
PG_IMAGE_REF=""
WIDGET_IMAGE_REF=""
PROFILE="ci"
API_DOMAIN=""
ADMIN_DOMAIN=""
CS_WIDGET_DOMAIN=""
RTC_DOMAIN=""
TURN_DOMAIN=""
CERTBOT_EMAIL=""
ADMIN_PHONE=""
ADMIN_PASSWORD=""
REPO="https://github.com/imboy-pub/imboy.git"
WORKDIR=""
KEEP=0

while [ $# -gt 0 ]; do
  case "$1" in
    --from)           [ $# -ge 2 ] || die "--from 需要值（上一稳定 tag）"; FROM="$2"; shift 2 ;;
    --from=*)         FROM="${1#*=}"; shift ;;
    --from-image)     [ $# -ge 2 ] || die "--from-image 需要值（基线镜像引用）"; FROM_IMAGE="$2"; shift 2 ;;
    --from-image=*)   FROM_IMAGE="${1#*=}"; shift ;;
    --from-ref)       [ $# -ge 2 ] || die "--from-ref 需要值（基线代码 tag|sha）"; FROM_REF="$2"; shift 2 ;;
    --from-ref=*)     FROM_REF="${1#*=}"; shift ;;
    --to-image)       [ $# -ge 2 ] || die "--to-image 需要值（candidate digest 引用）"; TO_IMAGE="$2"; shift 2 ;;
    --to-image=*)     TO_IMAGE="${1#*=}"; shift ;;
    --admin-image-ref)  [ $# -ge 2 ] || die "--admin-image-ref 需要值（digest 引用）"; ADMIN_IMAGE_REF="$2"; shift 2 ;;
    --admin-image-ref=*) ADMIN_IMAGE_REF="${1#*=}"; shift ;;
    --pg-image-ref)     [ $# -ge 2 ] || die "--pg-image-ref 需要值（digest 引用）"; PG_IMAGE_REF="$2"; shift 2 ;;
    --pg-image-ref=*)   PG_IMAGE_REF="${1#*=}"; shift ;;
    --widget-image-ref) [ $# -ge 2 ] && [ -n "$2" ] || die "--widget-image-ref 需要值（digest 引用）"; WIDGET_IMAGE_REF="$2"; shift 2 ;;
    --widget-image-ref=*) WIDGET_IMAGE_REF="${1#*=}"; [ -n "$WIDGET_IMAGE_REF" ] || die "--widget-image-ref 需要值（digest 引用）"; shift ;;
    --to-ref)         [ $# -ge 2 ] || die "--to-ref 需要值（升级目标 tag|sha）"; TO_REF="$2"; shift 2 ;;
    --to-ref=*)       TO_REF="${1#*=}"; shift ;;
    --profile)        [ $# -ge 2 ] || die "--profile 需要值：ci|host"; PROFILE="$2"; shift 2 ;;
    --profile=*)      PROFILE="${1#*=}"; shift ;;
    --api-domain)     [ $# -ge 2 ] || die "--api-domain 需要值"; API_DOMAIN="$2"; shift 2 ;;
    --api-domain=*)   API_DOMAIN="${1#*=}"; shift ;;
    --admin-domain)   [ $# -ge 2 ] || die "--admin-domain 需要值"; ADMIN_DOMAIN="$2"; shift 2 ;;
    --admin-domain=*) ADMIN_DOMAIN="${1#*=}"; shift ;;
    --cs-widget-domain) [ $# -ge 2 ] || die "--cs-widget-domain 需要值"; CS_WIDGET_DOMAIN="$2"; shift 2 ;;
    --cs-widget-domain=*) CS_WIDGET_DOMAIN="${1#*=}"; shift ;;
    --rtc-domain)      [ $# -ge 2 ] || die "--rtc-domain 需要值"; RTC_DOMAIN="$2"; shift 2 ;;
    --rtc-domain=*)    RTC_DOMAIN="${1#*=}"; shift ;;
    --turn-domain)     [ $# -ge 2 ] || die "--turn-domain 需要值"; TURN_DOMAIN="$2"; shift 2 ;;
    --turn-domain=*)   TURN_DOMAIN="${1#*=}"; shift ;;
    --certbot-email)  [ $# -ge 2 ] || die "--certbot-email 需要值"; CERTBOT_EMAIL="$2"; shift 2 ;;
    --certbot-email=*)
      # shellcheck disable=SC2034 # consumed by golden_common.sh's shared preparation
      CERTBOT_EMAIL="${1#*=}"; shift ;;
    --admin-phone)    [ $# -ge 2 ] || die "--admin-phone 需要值（手机号）"; ADMIN_PHONE="$2"; shift 2 ;;
    --admin-phone=*)  ADMIN_PHONE="${1#*=}"; shift ;;
    --admin-password) [ $# -ge 2 ] || die "--admin-password 需要值（明文密码）"; ADMIN_PASSWORD="$2"; shift 2 ;;
    --admin-password=*) ADMIN_PASSWORD="${1#*=}"; shift ;;
    --repo)           [ $# -ge 2 ] || die "--repo 需要值（git URL）"; REPO="$2"; shift 2 ;;
    --repo=*)         REPO="${1#*=}"; shift ;;
    --workdir)        [ $# -ge 2 ] || die "--workdir 需要值（目录）"; WORKDIR="$2"; shift 2 ;;
    --workdir=*)      WORKDIR="${1#*=}"; shift ;;
    --keep)           KEEP=1; shift ;;
    -h|--help)        usage; exit 0 ;;
    *)                die "未知参数：$1（--help 查看用法）" ;;
  esac
done

# ── 参数校验 ─────────────────────────────────────────────────────────────────
CURRENT_STAGE="arg"

[ -n "$TO_IMAGE" ] || die "缺少 --to-image（candidate digest 引用，形如 ghcr.io/<owner>/imboy-backend@sha256:…）"
case "$TO_IMAGE" in
  *@sha256:*) ;;
  *) die "--to-image 必须是 digest 引用（含 @sha256:…）：当前 ${TO_IMAGE}
  升级目标必须不可变（§3.1）；tag 引用会漂移，不能作为门禁输入" ;;
esac
EXPECTED_TO_DIGEST="${TO_IMAGE#*@}"

if [ -n "$ADMIN_IMAGE_REF" ]; then
  case "$ADMIN_IMAGE_REF" in
    *@sha256:*) ;;
    *) die "--admin-image-ref 必须是 digest 引用（含 @sha256:…）：当前 ${ADMIN_IMAGE_REF}" ;;
  esac
fi
if [ -n "$PG_IMAGE_REF" ]; then
  case "$PG_IMAGE_REF" in
    *@sha256:*) ;;
    *) die "--pg-image-ref 必须是 digest 引用（含 @sha256:…）：当前 ${PG_IMAGE_REF}" ;;
  esac
fi
if [ -n "$WIDGET_IMAGE_REF" ]; then
  [[ "$WIDGET_IMAGE_REF" =~ ^[^[:space:]@]+@sha256:[0-9a-f]{64}$ ]] \
    || die "--widget-image-ref 必须是完整 sha256 digest 引用 / a full SHA-256 digest reference is required"
fi

[ -n "$TO_REF" ] || die "缺少 --to-ref（升级目标代码 tag 或 40 位 sha）
  升级需 checkout vN+1 的 deploy/ 与 priv/migrations；digest 不含 tag 信息，必须显式给出"

if [ -n "$FROM" ] && { [ -n "$FROM_IMAGE" ] || [ -n "$FROM_REF" ]; }; then
  die "--from 与 --from-image/--from-ref 互斥（二选一）：上一稳定 tag 已发布用 --from <tag>；
  首个正式版前 / 需精确钉基线镜像用 --from-image <ref> --from-ref <tag|sha>"
fi
if [ -z "$FROM" ]; then
  { [ -n "$FROM_IMAGE" ] && [ -n "$FROM_REF" ]; } \
    || die "基线二选一：--from <vN tag> 或 --from-image <ref> + --from-ref <tag|sha>（当前两者都未提供）"
else
  FROM_REF="$FROM"
  # 基线镜像与升级目标同 registry/owner（从 --to-image 推导，如 ghcr.io/<owner>）；
  # Git tag 保持 v<version>；镜像仅去掉前导 v，保留完整版本后缀。
  # Preserve the Git ref; image tags use the version without its leading v.
  FROM_IMAGE="$(printf '%s' "$TO_IMAGE" | cut -d/ -f1-2)/imboy-backend:${FROM#v}"
fi
EXPECTED_FROM_DIGEST=""
case "$FROM_IMAGE" in *@sha256:*) EXPECTED_FROM_DIGEST="${FROM_IMAGE#*@}" ;; esac

case "$PROFILE" in
  host|ci) ;;
  *) die "--profile 仅支持 host|ci（当前: ${PROFILE}）" ;;
esac

{ [ -n "$ADMIN_PHONE" ] && [ -n "$ADMIN_PASSWORD" ]; } \
  || die "--admin-phone 与 --admin-password 必须成对提供（vN 安装与升级后 8 步链步骤 4 均需要，golden 强制）"
if [ "${#ADMIN_PASSWORD}" -lt 8 ] || [ "${#ADMIN_PASSWORD}" -gt 64 ] \
   || ! printf '%s' "$ADMIN_PASSWORD" | grep -q '[A-Za-z]' \
   || ! printf '%s' "$ADMIN_PASSWORD" | grep -q '[0-9]'; then
  die "--admin-password 强度不足：需 8-64 位且同时包含字母和数字（与 install.sh 口径一致）"
fi

MSG_COUNT="${GOLDEN_UP_MSG_COUNT:-3}"
case "$MSG_COUNT" in
  ''|*[!0-9]*) die "GOLDEN_UP_MSG_COUNT 必须是正整数（当前: ${MSG_COUNT}）" ;;
esac
{ [ "$MSG_COUNT" -ge 1 ] && [ "$MSG_COUNT" -le 10 ]; } \
  || die "GOLDEN_UP_MSG_COUNT 需在 1-10（当前: ${MSG_COUNT}）"

configure_golden_domains

# ── 目录与全局状态 ───────────────────────────────────────────────────────────
HOSTS_FILE="${GOLDEN_HOSTS_FILE:-/etc/hosts}"
WORKDIR="${WORKDIR:-$PWD/.golden-upgrade}"
CLONE_DIR="$WORKDIR/imboy"
DEPLOY_DIR="$CLONE_DIR/deploy"
COMMUNITY_YML="$DEPLOY_DIR/docker-compose.community.yml"
RUNTIME_YML="$DEPLOY_DIR/docker-compose.golden-runtime.yml"
OVERRIDE_YML="$DEPLOY_DIR/docker-compose.golden-override.yml"
RUN_DIR="$WORKDIR/run-$(date +%Y%m%d%H%M%S)"
INSTALL_LOG="$RUN_DIR/install_vn.log"
BIN_DIR="$RUN_DIR/bin"
STATE_FILE="$RUN_DIR/upgrade_state.json"
UP_BODY="$RUN_DIR/up_body"
HISTORY_MAP="$RUN_DIR/history_map.tsv"
MOUNTS_BEFORE="$RUN_DIR/mounts_before.txt"
MOUNTS_AFTER="$RUN_DIR/mounts_after.txt"
ATTACH_FILE="$RUN_DIR/datagen_attach.txt"
WS_TOOL="$SCRIPT_DIR/smoke/ws_c2g_send.py"
mkdir -p "$WORKDIR" "$RUN_DIR" "$BIN_DIR"
chmod 700 "$RUN_DIR"
: > "$INSTALL_LOG"

COMPOSE="docker compose -f $COMMUNITY_YML -f $RUNTIME_YML"
COMPOSE_O="$COMPOSE -f $OVERRIDE_YML"
BASE_URL="http://127.0.0.1:9800"
WS_URL="ws://127.0.0.1:9800/api/v1/ws"
DATA_DIR_RESOLVED=""

CURRENT_STAGE=""
STAGE_HINT=""

# 分环节秒数与断言结果（bash 3.2 兼容，不用关联数组）
SEC_CLONE=0; SEC_ENVINIT=0; SEC_PULL=0; SEC_INSTALLVN=0; SEC_SANITY=0
SEC_DATAGEN=0; SEC_UPGRADE=0; SEC_SMOKE8=0; SEC_VERIFY=0
RET_FAIL_COUNT=0; RET_FAILURES=""; MIG_NOTE="未观察"; MIG_APPLIED_DELTA=unknown; MIGRATION_RESULT=NOT_RUN
SCHEMA_BEFORE_ROWS=0; SCHEMA_BEFORE_VER=0; SCHEMA_AFTER_ROWS=0; SCHEMA_AFTER_VER=0; MIG_FILES=0
HISTORY_BEFORE_ROWS=0; HISTORY_AFTER_ROWS=0; HISTORY_BEFORE_VERSIONS='[]'

# ── HTTP / 状态文件 / DB helper ──────────────────────────────────────────────
up_http() { # <method> <url> [curl 参数…] → HTTP 码；body 落 $UP_BODY
  local method="$1" url="$2"; shift 2
  curl -sS -m 15 -o "$UP_BODY" -w '%{http_code}' -X "$method" "$@" "$url" 2>/dev/null || printf '000'
}
up_jq() { jq -r "$1" "$UP_BODY" 2>/dev/null || true; }

state_set_json() { # <key> <合法 JSON 文本>
  jq --arg k "$1" --argjson v "$2" '.[$k] = $v' "$STATE_FILE" > "${STATE_FILE}.t" \
    && mv "${STATE_FILE}.t" "$STATE_FILE"
}
state_str() { jq -r --arg p "$1" 'getpath($p | split(".")) // empty' "$STATE_FILE" 2>/dev/null || true; }
up_pg() { # <sql> → 单值（经 psql wrapper → pg 容器）
  PGPASSWORD="$(env_get_var POSTGRES_PASSWORD)" PGUSER="$(env_get_var POSTGRES_USER)" \
    PGDATABASE="$(env_get_var POSTGRES_DB)" \
    "$BIN_DIR/psql" -X -v ON_ERROR_STOP=1 -At -c "$1" 2>/dev/null
}

ret_fail() { # <断言名> <说明>：登记数据保留断言失败（跑完全部断言再判，不立即退出）
  RET_FAIL_COUNT=$(( RET_FAIL_COUNT + 1 ))
  RET_FAILURES="${RET_FAILURES:+${RET_FAILURES}; }$1"
  printf '\033[0;31m❌ 数据保留断言失败 [%s] %s\033[0m\n' "$1" "$2"
}

# ── 数据生成段：账号 / 消息 / 附件 / 群 / 好友（vN 上执行并就地验证）─────────
gu_register_login() { # <label>：成功后 UP_TOKEN/UP_UID，凭据入 state user_<label>
  local label="$1"
  golden_register_login "$label" || return 1
  UP_TOKEN="$GOLDEN_USER_TOKEN"; UP_UID="$GOLDEN_USER_UID"
  state_set_json "user_$label" \
    "$(jq -nc --arg e "$GOLDEN_USER_EMAIL" --arg p "$GOLDEN_USER_PASSWORD" --arg u "$UP_UID" '{email:$e,pwd:$p,uid:$u}')"
  ok "账号 ${label}：注册+登录成功（uid=${UP_UID}）"
}

gu_send_c2c() { # <from-uid> <to-uid> <text> → stdout 输出 msg_id
  local out mid
  if ! out="$("$CLONE_DIR/scripts/imboy_ctl" msg send "$1" "$2" -T "$3" 2>&1)"; then
    STAGE_HINT="imboy_ctl msg send 失败: $(printf '%s' "$out" | head -c 300)（encrypted_message_required=E2EE 强制档）"
    return 1
  fi
  mid="$(printf '%s' "$out" | awk -F= '/^MSG_ID=/ {print $2}' | tr -d '[:space:]')"
  [ -n "$mid" ] || { STAGE_HINT="msg send 输出无 MSG_ID: $(printf '%s' "$out" | head -c 200)"; return 1; }
  printf '%s' "$mid"
}

gu_history_get() { # <token> <chat_type> <peer_id>：0=成功（UP_BODY 为响应）
  local code
  code="$(up_http GET "$BASE_URL/api/v1/msg/history?chat_type=$2&peer_id=$3&after_seq=0&limit=100" \
    -H "Authorization: Bearer $1")"
  { [ "$code" = "200" ] && [ "$(up_jq '.code')" = "0" ]; } \
    || { STAGE_HINT="msg/history($2,$3) HTTP $code body=$(head -c 200 "$UP_BODY" 2>/dev/null)（msg_archive_enabled 需 true）"; return 1; }
}

gu_history_dump() { # UP_BODY → ${HISTORY_MAP}（msg_id<TAB>text 两列；payload 兼容 string|object）
  jq -r '(.payload.messages // .data.messages // [])[] | [ .msg_id,
      ((.payload | (if type == "string" then (fromjson? // {}) else . end)) | .text // .body // "")
    ] | @tsv' "$UP_BODY" > "$HISTORY_MAP" 2>/dev/null || : > "$HISTORY_MAP"
}
gu_hist_has() { awk -F'\t' -v m="$1" '$1 == m { f = 1 } END { exit !f }' "$HISTORY_MAP"; }
gu_hist_text() { awk -F'\t' -v m="$1" '$1 == m { print $2; exit }' "$HISTORY_MAP"; }

gu_wait_msg() { # <token> <chat_type> <peer_id> <msg_id> <timeout_s>：0=归档可见
  local deadline=$(( $(now) + $5 ))
  while [ "$(now)" -lt "$deadline" ]; do
    if gu_history_get "$1" "$2" "$3"; then
      gu_history_dump
      gu_hist_has "$4" && return 0
    fi
    sleep 2
  done
  return 1
}

gu_upload_attachment() { # <token>：成功后 UP_OBJ_KEY/UP_OBJ_MD5
  local token="$1" code put_url obj_key md5 size
  printf 'GOLDEN-UPGRADE-ATTACH-%s-%s' "$(date +%s)" "$(openssl rand -hex 8)" > "$ATTACH_FILE"
  code="$(up_http GET "$BASE_URL/api/v1/attachment/presign?filename=golden-up.txt&mime_type=text/plain" \
    -H "Authorization: Bearer $token")"
  put_url="$(up_jq '.payload.put_url // .data.put_url // empty')"
  obj_key="$(up_jq '.payload.object_key // .data.object_key // empty')"
  { [ "$code" = "200" ] && [ -n "$put_url" ] && [ -n "$obj_key" ]; } \
    || { STAGE_HINT="presign HTTP $code body=$(head -c 200 "$UP_BODY" 2>/dev/null)"; return 1; }
  code="$(up_http PUT "$put_url" -H 'Content-Type: text/plain' --data-binary @"$ATTACH_FILE")"
  { [ "$code" = "200" ] || [ "$code" = "204" ]; } \
    || { STAGE_HINT="S3 PUT HTTP ${code}（hosts 的 garage 解析是否生效: grep garage ${HOSTS_FILE}）"; return 1; }
  md5="$(openssl md5 -r "$ATTACH_FILE" | awk '{print $1}')"
  size="$(wc -c < "$ATTACH_FILE" | tr -d ' ')"
  code="$(up_http POST "$BASE_URL/api/v1/attachment/confirm" \
    -H "Authorization: Bearer $token" -H 'Content-Type: application/json' \
    -d "{\"object_key\":\"$obj_key\",\"md5\":\"$md5\",\"mime_type\":\"text/plain\",\"size\":$size}")"
  { [ "$code" = "200" ] && [ "$(up_jq '.code')" = "0" ]; } \
    || { STAGE_HINT="attachment/confirm HTTP $code body=$(head -c 200 "$UP_BODY" 2>/dev/null)"; return 1; }
  UP_OBJ_KEY="$obj_key"; UP_OBJ_MD5="$md5"
  ok "附件上传 round-trip 完成（object_key=$obj_key md5=${md5:0:12}…）"
}

gu_group_create() { # <tokenA> <uidB>：成功后 UP_GID
  local code gid
  code="$(up_http POST "$BASE_URL/api/v1/group/add" \
    -H "Authorization: Bearer $1" -H 'Content-Type: application/json' \
    -d "{\"member_uids\":[\"$2\"]}")"
  gid="$(golden_json_id "$UP_BODY" payload group id)" \
    || { STAGE_HINT="group/add 缺少有效群 ID（响应未输出）"; return 1; }
  { [ "$code" = "200" ] && [ "$(up_jq '.code')" = "0" ] && [ -n "$gid" ]; } \
    || { STAGE_HINT="group/add HTTP $code 或业务请求失败（member_uids 须 JSON 字符串数组；响应未输出）"; return 1; }
  UP_GID="$gid"
  ok "群创建成功并拉 B 入群（gid=${gid}）"
}

gu_send_group_msg() { # <token> <uid> <gid> <msg_id> <text>（WS C2G，帧契约见 ws_c2g_send.py）
  local out rc=0
  out="$(WS_URL="$WS_URL" WS_TOKEN="$1" WS_FROM_UID="$2" WS_GID="$3" \
        WS_MSG_ID="$4" WS_TEXT="$5" \
        python3 "$WS_TOOL" 2>&1)" || rc=$?
  { [ "$rc" -eq 0 ] && ! printf '%s' "$out" | grep -q 'C2G_ERROR'; } \
    || { STAGE_HINT="WS C2G 发送失败 rc=$rc: $(printf '%s' "$out" | tail -3 | tr '\n' ' ')（403=非群成员/禁言；429=限流）"; return 1; }
}

gu_friend_make() { # <tokenA> <uidA> <tokenB> <uidB>：A 申请 → B 确认
  golden_friend_make "$@"
}

gu_friend_has() { # <token> <uid>：0=好友列表含 uid
  golden_friend_has "$@"
}

gu_datagen() { # <tokenA> <uidA> <tokenB> <uidB>：编排数据生成（结果全落 state）
  local tokenA="$1" uidA="$2" tokenB="$3" uidB="$4"
  local i mid text gmsgid gtext runts
  runts="$(date +%s)"

  say "[datagen] 好友关系（A 添加 B → B 确认 → 双向列表可见断言）"
  gu_friend_make "$tokenA" "$uidA" "$tokenB" "$uidB" || die "好友关系建立失败（vN 基线）"

  say "[datagen] C2C 消息 ${MSG_COUNT} 条（A→B，imboy_ctl msg send + 归档可见断言）"
  i=1
  while [ "$i" -le "$MSG_COUNT" ]; do
    text="golden-up-c2c-${runts}-${i}"
    mid="$(gu_send_c2c "$uidA" "$uidB" "$text")" \
      || die "C2C 消息 #$i 发送失败（vN 基线）"
    gu_wait_msg "$tokenA" c2c "$uidB" "$mid" 20 \
      || die "C2C 消息 #${i}（msg_id=${mid}）20s 内未在 msg/history 归档可见（vN 数据未落库，断言基线不成立）"
    state_set_json "c2c_msg_$i" "$(jq -nc --arg id "$mid" --arg t "$text" '{id:$id,text:$t}')"
    i=$(( i + 1 ))
    sleep 0.05  # imboy_ctl 的 msg_id=ctl_<ms>，同毫秒会碰撞，错开发送
  done
  ok "C2C ${MSG_COUNT} 条全部发送且 vN 上归档可见（msg id 已记入 state）"
  say "[datagen] 附件 round-trip（A）"
  gu_upload_attachment "$tokenA" || die "附件上传失败（vN 基线）"
  state_set_json attachment "$(jq -nc --arg k "$UP_OBJ_KEY" --arg m "$UP_OBJ_MD5" '{object_key:$k,md5:$m}')"

  say "[datagen] 建群（A 创建 + B 入群）+ 1 条群消息（WS C2G）"
  gu_group_create "$tokenA" "$uidB" || die "建群失败（vN 基线）"
  gmsgid="golden-up-c2g-${runts}"
  gtext="golden-up-group-${runts}"
  gu_send_group_msg "$tokenA" "$uidA" "$UP_GID" "$gmsgid" "$gtext" || die "群消息 WS 发送失败（vN 基线）"
  gu_wait_msg "$tokenA" c2g "$UP_GID" "$gmsgid" 20 \
    || die "群消息（msg_id=${gmsgid}）20s 内未归档可见（vN 数据未落库，断言基线不成立）"
  state_set_json group "$(jq -nc --arg g "$UP_GID" --arg id "$gmsgid" --arg t "$gtext" '{gid:$g,msg_id:$id,text:$t}')"
  ok "群消息归档可见（gid=$UP_GID msg_id=${gmsgid}）"

}

# ── 升级前快照（数据保留断言的对照组）────────────────────────────────────────
gu_migration_versions() { # stdin: release/source filenames' numeric prefixes
  jq -Rces '
    split("\n") | map(select(length > 0)) |
    if length > 0 and all(.[]; test("^[0-9]{8}$")) then
      map(tonumber) | sort |
      if length == (unique | length) then . else error("duplicate migration versions") end
    else error("empty or invalid migration versions") end
  '
}

gu_schema_snapshot() { # stdout: validated head + exact history; no database writes
  local release_versions source_versions ledger
  # Match the cleanup helper's running-release oracle, then bind it to the
  # immutable checkout as well. A filename count alone cannot establish identity.
  release_versions="$(docker exec imboy_backend sh -c '
    for f in /opt/imboy/lib/imboy-*/priv/migrations/*.up.sql; do
      test -f "$f" || exit 1
      f=${f##*/}; printf "%s\n" "${f%%_*}"
    done
  ' | gu_migration_versions)" \
    || { printf '无法读取有效的运行 release 迁移清单\n' >&2; return 1; }
  source_versions="$(
    for file in "$CLONE_DIR"/priv/migrations/*.up.sql; do
      [ -f "$file" ] || exit 1
      file=${file##*/}; printf '%s\n' "${file%%_*}"
    done | gu_migration_versions
  )" || { printf '固定源码迁移清单为空、重号或格式无效\n' >&2; return 1; }
  [ "$release_versions" = "$source_versions" ] \
    || { printf '运行 release 与固定源码的迁移版本集合不一致\n' >&2; return 1; }

  # One statement observes the head and history in the same database snapshot.
  ledger="$(up_pg "SELECT json_build_object(
    'head', (SELECT coalesce(json_agg(json_build_object('version', version, 'dirty', dirty)), '[]'::json)
             FROM public.schema_migrations),
    'history', (SELECT coalesce(json_agg(version ORDER BY version), '[]'::json)
                FROM public.schema_migrations_history))")" \
    || { printf '无法读取迁移版本头或历史表\n' >&2; return 1; }
  # Slurp rejects extra JSON output; empty/error output must never become a pass.
  jq -ces --argjson expected "$release_versions" '
    if length == 1 then .[0] else error("expected one migration snapshot") end |
    if type == "object" and (.head | type) == "array" and (.head | length) == 1
       and .head[0].dirty == false and .head[0].version == ($expected | max)
       and .history == $expected
    then {head: .head[0], versions: .history}
    else error("migration head/dirty/history differs from the release") end
  ' <<< "$ledger"
}

gu_snapshot_before() {
  local schema backend_ref
  schema="$(gu_schema_snapshot)" \
    || { STAGE_HINT="vN 迁移头/历史未与实际 release 及固定源码精确对应；不能建立升级基线"; return 1; }
  SCHEMA_BEFORE_ROWS=1
  SCHEMA_BEFORE_VER="$(jq -r '.head.version' <<< "$schema")"
  HISTORY_BEFORE_VERSIONS="$(jq -c '.versions' <<< "$schema")"
  HISTORY_BEFORE_ROWS="$(jq '.versions | length' <<< "$schema")"

  backend_ref="$(docker inspect -f '{{.Config.Image}}' imboy_backend 2>/dev/null || true)"
  [ -n "$backend_ref" ] || { STAGE_HINT="docker inspect imboy_backend 取 Config.Image 失败"; return 1; }

  # pg/garage 挂载源快照（compose 数据卷= DATA_DIR bind mount；up 重建容器后
  # Source 必须不变——"数据卷跨升级保留"的机制所在）
  docker inspect -f '{{range .Mounts}}{{.Source}} -> {{.Destination}}{{"\n"}}{{end}}' \
      imboy_pg18 imboy_garage 2>/dev/null | sed '/^$/d' | sort > "$MOUNTS_BEFORE" \
    || { STAGE_HINT="docker inspect pg/garage 挂载快照失败"; return 1; }

  state_set_json snapshot_before \
    "$(jq -nc --argjson schema "$schema" --arg img "$backend_ref" \
       '{schema_rows:1,schema_max_version:$schema.head.version,schema_dirty:$schema.head.dirty,
         history_count:($schema.versions | length),history_versions:$schema.versions,backend_image:$img}')" \
    || { STAGE_HINT="无法保存升级前迁移历史快照"; return 1; }
  ok "升级前快照：schema_migrations=1（ver=${SCHEMA_BEFORE_VER}，dirty=false），history=${HISTORY_BEFORE_ROWS}，backend=${backend_ref}"
}

# ── 升级段 ───────────────────────────────────────────────────────────────────
gu_checkout_target() { # git fetch --to-ref + checkout（clone 副本原地切换）
  # 先还原被 wrapper 替换的 scripts/imboy_ctl（tracked modified 会阻碍 checkout）
  if [ -f "$CLONE_DIR/scripts/imboy_ctl.golden-real" ]; then
    mv -f "$CLONE_DIR/scripts/imboy_ctl.golden-real" "$CLONE_DIR/scripts/imboy_ctl"
  fi
  git -C "$CLONE_DIR" fetch -q --depth 1 origin "$TO_REF" \
    || { STAGE_HINT="git fetch ${TO_REF} 失败（git ls-remote --tags $REPO | grep $TO_REF 检查）"; return 1; }
  git -C "$CLONE_DIR" checkout -q --detach FETCH_HEAD \
    || { STAGE_HINT="git checkout FETCH_HEAD 失败（tracked 改动未还原？）"; return 1; }
  { [ -f "$DEPLOY_DIR/install.sh" ] && [ -f "$COMMUNITY_YML" ] && [ -d "$CLONE_DIR/priv/migrations" ]; } \
    || { STAGE_HINT="checkout ${TO_REF} 后缺 install.sh/community.yml/priv/migrations 之一"; return 1; }
}

gu_observe_migration() { # 新 backend 容器日志的 auto_migrate 锚点 → MIG_NOTE
  # 迁移失败语义（代码核实）：migrate 失败 → erlang:error → 节点起不来 → healthz
  # 超时 FAIL。故锚点缺失/WARN 不单独 FAIL，权威断言在 gu_assert_schema。
  local logs
  logs="$(docker logs imboy_backend 2>&1 || true)"
  if printf '%s' "$logs" | grep -qF 'automatic migrations disabled'; then
    MIG_NOTE="WARN：auto_migrate=false（显式迁移模式）——以 schema_migrations 断言为准"
  elif printf '%s' "$logs" | grep -qF '[imboy_migrate] all migrations applied'; then
    MIG_NOTE="PASS：auto_migrate 执行（imboy_migrate running→all applied 锚点齐备）"
  elif printf '%s' "$logs" | grep -qF '[imboy_migrate] running migrations from'; then
    MIG_NOTE="WARN：见 running 锚点但未见 all applied（输出截断？）——以 schema_migrations 断言为准"
  else
    MIG_NOTE="WARN：auto_migrate 日志锚点缺失（输出格式变化或日志级别调整）——以 schema_migrations 断言为准"
  fi
}

gu_assert_schema() { # 单行版本头 + 真实 release/source/history 精确集合 + 基线保留
  local schema versions
  MIGRATION_RESULT=FAIL
  schema="$(gu_schema_snapshot)" \
    || { ret_fail schema_ledger "目标迁移头/dirty/history 与实际 release 或固定源码不一致"; return 1; }
  SCHEMA_AFTER_ROWS=1
  SCHEMA_AFTER_VER="$(jq -r '.head.version' <<< "$schema")"
  versions="$(jq -c '.versions' <<< "$schema")"
  HISTORY_AFTER_ROWS="$(jq '.versions | length' <<< "$schema")"
  MIG_FILES="$HISTORY_AFTER_ROWS"
  if ! jq -en --argjson before "$HISTORY_BEFORE_VERSIONS" --argjson after "$versions" \
      '($before | length) > 0 and ($before - $after | length) == 0' >/dev/null; then
    ret_fail schema_history "升级前迁移历史缺失或有版本从目标历史中丢失"
    return 1
  fi
  MIG_APPLIED_DELTA="$(jq -n --argjson before "$HISTORY_BEFORE_VERSIONS" --argjson after "$versions" \
    '$after - $before | length')"
  MIGRATION_RESULT=PASS
  ok "迁移断言：schema_migrations=1，version ${SCHEMA_BEFORE_VER}→${SCHEMA_AFTER_VER}，dirty=false；history ${HISTORY_BEFORE_ROWS}→${HISTORY_AFTER_ROWS}（新增 ${MIG_APPLIED_DELTA}），与 release/source ${MIG_FILES} 个版本精确一致"
}

gu_assert_backend_replaced() { # backend 容器确实换成了 to-image
  local ref
  ref="$(docker inspect -f '{{.Config.Image}}' imboy_backend 2>/dev/null || true)"
  if [ "$ref" != "$TO_IMAGE" ]; then  # BACKEND_IMAGE 未生效 / 容器未重建
    ret_fail backend_image "backend 运行镜像=${ref:-?} ≠ --to-image=${TO_IMAGE}（BACKEND_IMAGE 未生效/容器未重建）"
    return 1
  fi
  ok "backend 容器已按 candidate digest 重建（Config.Image=${TO_IMAGE}）"
}

gu_assert_data_volumes() { # 数据卷保留：挂载源不变 + 数据目录非空
  local d="$DATA_DIR_RESOLVED" fail=0
  docker inspect -f '{{range .Mounts}}{{.Source}} -> {{.Destination}}{{"\n"}}{{end}}' \
      imboy_pg18 imboy_garage 2>/dev/null | sed '/^$/d' | sort > "$MOUNTS_AFTER" || true
  cmp -s "$MOUNTS_BEFORE" "$MOUNTS_AFTER" \
    || { ret_fail vol_mounts "pg/garage 挂载源跨升级变化（对照 $MOUNTS_BEFORE vs ${MOUNTS_AFTER}）"; fail=1; }
  { [ -d "$d/pg18" ] && [ -n "$(ls -A "$d/pg18" 2>/dev/null)" ]; } \
    || { ret_fail vol_pg "pg 数据目录丢失或为空：$d/pg18"; fail=1; }
  { [ -d "$d/garage" ] && [ -n "$(ls -A "$d/garage" 2>/dev/null)" ]; } \
    || { ret_fail vol_garage "garage 数据目录丢失或为空：$d/garage"; fail=1; }
  [ "$fail" -eq 0 ] && ok "数据卷保留：pg/garage 挂载源不变，数据目录非空（${d}/{pg18,garage}）"
}

# ── 验收段：数据保留断言（读 upgrade_state.json 对照）────────────────────────
gu_verify_login() { # <label>：0=可登录（UP_TOKEN 为新 token）
  local email pwd code token
  email="$(state_str "user_$1.email")"
  pwd="$(state_str "user_$1.pwd")"
  code="$(up_http POST "$BASE_URL/api/v1/passport/login" \
    -H 'Content-Type: application/x-www-form-urlencoded' \
    --data-urlencode 'type=email' --data-urlencode "account=$email" \
    --data-urlencode "pwd=$pwd" --data-urlencode 'rsa_encrypt=0')"
  token="$(up_jq '.payload.token // empty')"
  if { [ "$code" = "200" ] && [ -n "$token" ]; }; then
    UP_TOKEN="$token"; return 0
  fi
  ret_fail "login_$1" "账号 $1（${email}）升级后无法登录：HTTP $code body=$(head -c 120 "$UP_BODY" 2>/dev/null)"
  return 1
}

gu_verify_c2c() { # <tokenA> <uidB>：逐条比对 msg id 与内容
  local i=1 mid text got miss=0
  gu_history_get "$1" c2c "$2" \
    || { ret_fail c2c_history "升级后 msg/history(c2c,$2) 拉取失败"; return 1; }
  gu_history_dump
  while [ "$i" -le "$MSG_COUNT" ]; do
    mid="$(state_str "c2c_msg_$i.id")"
    text="$(state_str "c2c_msg_$i.text")"
    if ! gu_hist_has "$mid"; then
      ret_fail "c2c_msg_$i" "历史消息丢失：msg_id=${mid} 不在升级后的会话里"
      miss=1
    else
      got="$(gu_hist_text "$mid")"
      if [ "$got" != "$text" ]; then
        ret_fail "c2c_msg_$i" "消息内容不一致：msg_id=${mid} 期望[${text}] 实得[${got}]"
        miss=1
      fi
    fi
    i=$(( i + 1 ))
  done
  [ "$miss" -eq 0 ] && ok "C2C 历史消息 ${MSG_COUNT}/${MSG_COUNT} 条 msg id 与内容逐一一致"
}

gu_verify_attachment() { # <token>：view_url → 下载 → md5 比对
  local key md5 code vurl got_md5
  key="$(state_str 'attachment.object_key')"
  md5="$(state_str 'attachment.md5')"
  code="$(up_http GET "$BASE_URL/api/v1/attachment/view_url?object_key=$key" \
    -H "Authorization: Bearer $1")"
  vurl="$(up_jq '.payload.url // .data.url // empty')"
  { [ "$code" = "200" ] && [ -n "$vurl" ]; } \
    || { ret_fail attachment_view "附件 view_url 失败：HTTP ${code}（object_key=${key}，元数据跨升级丢失？）"; return 1; }
  code="$(up_http GET "$vurl")"
  [ "$code" = "200" ] \
    || { ret_fail attachment_download "附件下载失败：HTTP ${code}（garage 对象跨升级丢失）"; return 1; }
  got_md5="$(openssl md5 -r "$UP_BODY" | awk '{print $1}')"
  [ "$got_md5" = "$md5" ] \
    || { ret_fail attachment_md5 "附件内容 md5 不一致：期望 ${md5} 实得 ${got_md5}"; return 1; }
  ok "附件可下载且 md5 一致（${md5:0:12}…）"
}

gu_verify_group() { # <tokenA>：群存在 + 群消息可读且内容一致
  local gid gmsgid gtext code
  gid="$(state_str 'group.gid')"
  gmsgid="$(state_str 'group.msg_id')"
  gtext="$(state_str 'group.text')"
  code="$(up_http GET "$BASE_URL/api/v1/group/detail?gid=$gid" -H "Authorization: Bearer $1")"
  { [ "$code" = "200" ] && [ "$(up_jq '.code')" = "0" ]; } \
    || ret_fail group_detail "群 ${gid} 升级后不可见：HTTP ${code}（群数据丢失？）"
  gu_history_get "$1" c2g "$gid" \
    || { ret_fail group_history "升级后 msg/history(c2g,$gid) 拉取失败"; return 1; }
  gu_history_dump
  if ! gu_hist_has "$gmsgid"; then
    ret_fail group_msg "群消息丢失：msg_id=${gmsgid}（gid=${gid}）"
  elif [ "$(gu_hist_text "$gmsgid")" != "$gtext" ]; then
    ret_fail group_msg "群消息内容不一致：msg_id=${gmsgid} 期望[${gtext}] 实得[$(gu_hist_text "$gmsgid")]"
  else
    ok "群存在且群消息可读（gid=${gid} msg_id=${gmsgid} 内容一致）"
  fi
}

gu_verify_friend() { # <tokenA> <uidB>
  if gu_friend_has "$1" "$2"; then
    ok "好友关系保留（A 的列表含 B）"
  else
    ret_fail friend "好友关系丢失：A 的 friend/list 不再含 B（uid=$2）"
  fi
}

# ── 报告 / 清理 ──────────────────────────────────────────────────────────────
report() { # <verdict> <total> <data_retention> <smoke8>
  local verdict="$1" total="$2" retention="$3" smoke8="$4"
  printf '\n'
  printf '\033[1;36m════════════ Golden Upgrade 分环节计时（§5，无预算判定）════════════\033[0m\n'
  printf '%-34s %8s  %s\n' '环节' '秒' '备注'
  printf '%-34s %8s  %s\n' 'clone（基线 vN）'        "$SEC_CLONE"     'git clone/fetch（--from-ref）'
  printf '%-34s %8s  %s\n' 'init（env 配置）'         "$SEC_ENVINIT"   '.env+域名+BACKEND_IMAGE+证书预置+wrapper'
  printf '%-34s %8s  %s\n' '拉镜像（pull 基线）'       "$SEC_PULL"      'docker compose pull（vN 全套）'
  printf '%-34s %8s  %s\n' '安装 vN（install.sh）'     "$SEC_INSTALLVN" '两段式第二段：起栈+TLS+等健康+超管'
  printf '%-34s %8s  %s\n' '自检（sanity 等价）'       "$SEC_SANITY"    'community 口径 7 容器+healthz+迁移行'
  printf '%-34s %8s  %s\n' '数据生成（升级前）'        "$SEC_DATAGEN"   "账号/消息×${MSG_COUNT}/附件/群/好友→state"
  printf '%-34s %8s  %s\n' '升级（checkout+up）'       "$SEC_UPGRADE"   "fetch ${TO_REF}+BACKEND_IMAGE=to-image+up+健康"
  printf '%-34s %8s  %s\n' '自检（8 步链）'            "$SEC_SMOKE8"    '新账号注册→…→附件下载（升级后可用性）'
  printf '%-34s %8s  %s\n' '数据保留断言'              "$SEC_VERIFY"    '登录/消息/附件/群/好友/卷 对照 state'
  printf '\n'
  printf '  总耗时 TOTAL=%ss（无预算判定：§4.2 的 900s 仅钉死 Golden Install；本值进质量总账）\n' "$total"
  printf '\n  ── 升级身份与判定 ──\n'
  printf '  FROM: ref=%s image=%s / TO: ref=%s image=%s（digest=%s）\n' \
    "$FROM_REF" "$FROM_IMAGE" "$TO_REF" "$TO_IMAGE" "$EXPECTED_TO_DIGEST"
  printf '  vN 三元组: VERSION=%s SHA=%s DIGEST=%s\n' \
    "${INST_VERSION:-unknown}" "${INST_GIT_SHA:-unknown}" "${INST_DIGEST:-unknown}"
  printf '  auto_migrate: %s\n  schema_migrations: %s→%s 行，version %s→%s\n  schema_migrations_history: %s→%s 个版本（新增 %s），release/source 迁移版本 %s 个，账本断言 %s\n' \
    "$MIG_NOTE" "$SCHEMA_BEFORE_ROWS" "$SCHEMA_AFTER_ROWS" "$SCHEMA_BEFORE_VER" "$SCHEMA_AFTER_VER" \
    "$HISTORY_BEFORE_ROWS" "$HISTORY_AFTER_ROWS" "$MIG_APPLIED_DELTA" "$MIG_FILES" "$MIGRATION_RESULT"
  printf '  8 步冒烟链（升级后）: %s\n  数据保留断言: %s%s\n' \
    "$smoke8" "$retention" "${RET_FAILURES:+（失败项: ${RET_FAILURES}）}"
  if [ "$PROFILE" = "ci" ]; then
    printf '\n  ── ci 模式差异（客观约束，透明披露；同 G1 口径）──\n'
    printf '  · 五域 CI 自签证书：%s/%s/%s/%s/%s；冒烟直连 %s，未验证公网 TLS\n' \
      "$API_DOMAIN" "$ADMIN_DOMAIN" "$CS_WIDGET_DOMAIN" "$RTC_DOMAIN" "$TURN_DOMAIN" "$BASE_URL"
    printf '  · backend IMBOYENV=dev（override，仅冒烟阶段）：万能码/captcha 自动化前提\n'
    printf '  · vN 基线对数据生成端点的兼容性由数据生成段就地验证（不支持即 FAIL，不臆造）\n'
  fi
  printf '\n'
  printf 'GOLDEN_UPGRADE=%s TOTAL=%ss FROM=%s TO=%s PROFILE=%s MIGRATION=%s DATA_RETENTION=%s SMOKE8=%s\n' \
    "$verdict" "$total" "$FROM_REF" "$TO_REF" "$PROFILE" \
    "$MIGRATION_RESULT" "$retention" "$smoke8"
}

cleanup() {
  hosts_del_garage 2>/dev/null || true
  if [ "$KEEP" = 0 ]; then
    say "清理现场（--keep 可保留）"
    $COMPOSE_O down --remove-orphans -v >/dev/null 2>&1 || true
    docker network rm imboy-network >/dev/null 2>&1 || true
    rm -rf "$CLONE_DIR"
    ok "已清理：compose 栈 / imboy-network / clone 目录 / hosts 行（日志与 state 保留 ${RUN_DIR}）"
  else
    say "现场保留（--keep）"
    printf '  clone=%s  日志/state=%s  栈状态: %s ps\n' "$CLONE_DIR" "$RUN_DIR" "$COMPOSE_O"
    warn "hosts 的 golden-upgrade garage 行请用完手工删除: grep -v '# golden-upgrade garage' $HOSTS_FILE"
  fi
}

# ═════════════════════════════════════════════════════════════════════════════
# 主流程
# ═════════════════════════════════════════════════════════════════════════════
say "Golden Upgrade（profile=${PROFILE}，无预算判定）"
printf '  from: %s（image=%s）\n  from-ref: %s\n  to-image: %s\n  to-ref: %s\n  repo: %s\n  workdir: %s\n' \
  "${FROM:-（显式组）}" "$FROM_IMAGE" "$FROM_REF" "$TO_IMAGE" "$TO_REF" "$REPO" "$WORKDIR"
[ -n "$FROM" ] || printf '  基线模式：--from-image + --from-ref（首个正式版发布前无上一稳定 tag）\n'
# 1) cleanroom 断言（同 G1：升级前状态由脚本自己安装 vN 建立，故同样要求干净环境）
CURRENT_STAGE="cleanroom-assert"
STAGE_HINT="按上方第一条 ❌ 提示修复环境后重跑。"
say "cleanroom 断言"
assert_cleanroom
# 2) T0 → clone 基线 vN
CURRENT_STAGE="clone"
STAGE_HINT="网络/权限: git ls-remote ${REPO}；ref 是否存在: git ls-remote --tags $REPO | grep -E '$FROM_REF|$TO_REF'"
T0=$(now)
say "[clone] git clone（基线 vN，--branch ${FROM_REF}）"
t=$(now)
if printf '%s' "$FROM_REF" | grep -Eq '^[0-9a-f]{40}$'; then
  git init -q "$CLONE_DIR"
  git -C "$CLONE_DIR" remote add origin "$REPO"
  git -C "$CLONE_DIR" fetch -q --depth 1 origin "$FROM_REF"
  git -C "$CLONE_DIR" checkout -q FETCH_HEAD
else
  git clone -q --depth 1 --branch "$FROM_REF" "$REPO" "$CLONE_DIR"
fi
[ -f "$DEPLOY_DIR/install.sh" ] || die "clone 完成但缺少 deploy/install.sh（仓库不完整？）"
SEC_CLONE=$(( $(now) - t ))
# 3) init（env 配置，同 G1 两段式第一段 + 钉基线镜像）
CURRENT_STAGE="env-init"
STAGE_HINT="第一段 install.sh 失败看上方输出；常见: openssl 缺失 / .env.example 缺失"
say "[init] 生成 .env 并配置（BACKEND_IMAGE 钉基线 vN 镜像）"
t=$(now)
prepare_golden_install "$FROM_IMAGE"
DATA_DIR_RESOLVED="$(resolve_data_dir)"
SEC_ENVINIT=$(( $(now) - t ))
# 4) 拉基线镜像
CURRENT_STAGE="pull"
STAGE_HINT="拉取失败: registry 凭据 / 基线镜像 tag 是否存在（docker pull $FROM_IMAGE 手工复现）"
say "[pull] docker compose pull（基线 vN）"
t=$(now)
( cd "$DEPLOY_DIR" && $COMPOSE pull ) || die "docker compose pull（基线）失败"
SEC_PULL=$(( $(now) - t ))
# 5) 安装 vN（install.sh 两段式第二段，同 G1）
CURRENT_STAGE="install-vn"
STAGE_HINT="install.sh 失败。完整日志: $INSTALL_LOG"
say "[install-vn] bash install.sh --edition community --yes（安装基线 vN）"
t=$(now)
run_install_phase2
INSTALL_END=$(now)
SEC_INSTALLVN=$(( INSTALL_END - t ))
# 6) vN 后置检查：三元组 + 基线 digest 一致性（--from-image 为 digest 时）
CURRENT_STAGE="post-install-check"
INST_VERSION="$(grep -o 'IMBOY_VERSION=[^ ]*' "$INSTALL_LOG" | head -1 | cut -d= -f2 || true)"
INST_GIT_SHA="$(grep -o 'IMBOY_GIT_SHA=[0-9a-f]*' "$INSTALL_LOG" | head -1 | cut -d= -f2 || true)"
INST_DIGEST="$(grep -o 'IMBOY_IMAGE_DIGEST=sha256:[0-9a-f]*' "$INSTALL_LOG" | head -1 | cut -d= -f2 || true)"
if [ -n "$EXPECTED_FROM_DIGEST" ]; then
  [ "$INST_DIGEST" = "$EXPECTED_FROM_DIGEST" ] \
    || die "vN 基线 digest 一致性断言失败: 安装运行=${INST_DIGEST:-未捕获} vs --from-image=${EXPECTED_FROM_DIGEST}"
fi
ok "vN 三元组: VERSION=${INST_VERSION:-?} SHA=${INST_GIT_SHA:-?} DIGEST=${INST_DIGEST:-?}${EXPECTED_FROM_DIGEST:+（digest 校验 PASS）}"
parse_install_stages "$INSTALL_END"
# 7) 冒烟准备（override IMBOYENV=dev 重建 + hosts）+ sanity 等价自检
CURRENT_STAGE="sanity"
STAGE_HINT="override up / healthz 见输出；$COMPOSE_O logs --tail=100 imboy_backend"
say "[sanity] 冒烟准备（backend 重建为 IMBOYENV=dev + hosts 注入 garage）+ 等价自检"
t=$(now)
( cd "$DEPLOY_DIR" && $COMPOSE_O up -d ) || die "override up 失败（backend IMBOYENV=dev 重建）"
wait_healthz 180 "（vN 冒烟配置就绪）" || die "vN backend 冒烟配置重建后未就绪"
hosts_set_garage || die "hosts 注入 garage 解析失败"
sanity_equivalent || die "vN sanity 等价自检未通过"
SEC_SANITY=$(( $(now) - t ))
# 8) 数据生成段（§5：升级前产生真实数据，全部落 upgrade_state.json）
CURRENT_STAGE="datagen"
STAGE_HINT="见上方首个 ❌（每个子步骤已带端点级排查提示）；vN 兼容性问题在此暴露"
say "[datagen] 产生真实数据（vN 上执行并就地验证，关键值落 $(basename "$STATE_FILE")）"
[ -f "$WS_TOOL" ] || die "缺少 WS C2G 发送器: $WS_TOOL"
jq -nc --argjson ts "$(date +%s)" --arg f "$FROM_REF" --arg t "$TO_REF" \
  '{created_at:$ts, from_ref:$f, to_ref:$t}' > "$STATE_FILE"
t=$(now)
gu_register_login A || die "数据生成失败：账号 A"
UID_A="$UP_UID"; TOKEN_A="$UP_TOKEN"
gu_register_login B || die "数据生成失败：账号 B"
UID_B="$UP_UID"; TOKEN_B="$UP_TOKEN"
gu_datagen "$TOKEN_A" "$UID_A" "$TOKEN_B" "$UID_B" || die "数据生成段失败（上方已列出具体端点与提示）"
gu_snapshot_before || die "升级前快照失败"
SEC_DATAGEN=$(( $(now) - t ))
# 9) 升级段（§5：checkout vN+1 + BACKEND_IMAGE=candidate digest + compose up）
CURRENT_STAGE="upgrade"
STAGE_HINT="见上方首个 ❌；升级后日志: $COMPOSE_O logs --tail=200 imboy_backend"
say "[upgrade] checkout ${TO_REF} → BACKEND_IMAGE=${TO_IMAGE} → compose up -d"
t=$(now)
gu_checkout_target || die "切换到 ${TO_REF} 失败"
prepare_golden_control # 目标 VM 配置 + 同一 run 的节点/cookie；不沿用旧 VM flags。
install_ctl_wrapper   # vN+1 代码内重装容器内转发 wrapper（checkout 还原了原文件）
env_set_var BACKEND_IMAGE "$TO_IMAGE"
# 旧基线可能没有 Widget 服务；在目标 checkout 后再次应用并显式拉取。
# Reapply after checkout so a baseline without Widget still tests the target digest.
upgrade_pull_services=(imboy_backend)
if [ -n "$WIDGET_IMAGE_REF" ]; then
  env_set_var IMBOY_WIDGET_IMAGE "$WIDGET_IMAGE_REF"
  upgrade_pull_services+=(imboy_widget)
fi
( cd "$DEPLOY_DIR" && $COMPOSE_O pull "${upgrade_pull_services[@]}" ) || die "拉取 candidate 镜像失败（digest 是否已推送到 registry）"
( cd "$DEPLOY_DIR" && $COMPOSE_O up -d ) || die "docker compose up -d（升级）失败——数据未被动过，可安全重试"
wait_healthz 300 "（vN+1 启动，含 auto_migrate）" || die "升级后 backend 300s 未就绪（迁移失败会 erlang:error 起不来：查 $COMPOSE_O logs imboy_backend）"
# Recheck the target release's real cleanup identity and migration ledger.
# Verification only: upgrading must not recreate or reset an existing LOGIN.
verify_golden_cleanup \
  || die "升级后清理账号验证失败；保留原账号/凭据与数据，不能判定升级成功"
gu_observe_migration
gu_assert_backend_replaced || true
gu_assert_schema || true          # 失败已登记 RET_FAIL_COUNT，跑完全部断言统一判定
gu_assert_data_volumes || true    # （尾部 [ cond ] && ok 在有失败时返回非零，不可裸调）
SEC_UPGRADE=$(( $(now) - t ))
# 10) 验收段：8 步链（升级后新账号证明可用性）+ 数据保留断言
CURRENT_STAGE="smoke8"
STAGE_HINT="见 smoke_8step 输出；C2C 双方用数据生成段的 A→B（真实存在账号）"
say "[smoke8] 8 步链（升级后：Health→…→Download，C2C=A→B）"
t=$(now)
SMOKE_C2C_FROM="$UID_A" SMOKE_C2C_TO="$UID_B" run_smoke8 || die "升级后 8 步冒烟链未全绿"
SMOKE8_RESULT="PASS"
SEC_SMOKE8=$(( $(now) - t ))
CURRENT_STAGE="verify"
# shellcheck disable=SC2034  # 跨文件接口变量（golden_common.sh die() 消费）
STAGE_HINT="见数据保留断言失败项（断言名→说明）；状态对照: $STATE_FILE"
say "[verify] 数据保留断言（对照 $(basename "$STATE_FILE")）"
t=$(now)
hosts_set_garage || die "升级后 hosts 更新 garage 解析失败（容器 IP 变化）"
gu_verify_login A && TOKEN_A="$UP_TOKEN" || true
gu_verify_login B && TOKEN_B="$UP_TOKEN" || true
if [ -n "${TOKEN_A:-}" ]; then
  gu_verify_c2c "$TOKEN_A" "$UID_B" || true
  gu_verify_attachment "$TOKEN_A" || true
  gu_verify_group "$TOKEN_A" || true
  gu_verify_friend "$TOKEN_A" "$UID_B" || true
fi
SEC_VERIFY=$(( $(now) - t ))
# 11) 判定 + 报告 + 清理
# shellcheck disable=SC2034  # 跨文件接口变量（golden_common.sh die() 消费）
CURRENT_STAGE="verdict"
TOTAL=$(( $(now) - T0 ))
RETENTION="PASS"; VERDICT="PASS"
[ "$RET_FAIL_COUNT" -eq 0 ] || { RETENTION="FAIL"; VERDICT="FAIL"; }
report "$VERDICT" "$TOTAL" "$RETENTION" "${SMOKE8_RESULT:-FAIL}"
cleanup
[ "$VERDICT" = "PASS" ] || exit 1
exit 0
