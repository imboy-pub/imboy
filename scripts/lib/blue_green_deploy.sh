#!/usr/bin/env bash
set -Eeuo pipefail

if [ "${IMBOY_DEPLOY_INTERNAL:-}" != "1" ]; then
  echo "该脚本是内部实现；请使用: bash ./scripts/imboy-deploy.sh api" >&2
  exit 2
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# =============================================================================
# 脚本做的事 / What this script does
#
# 蓝绿部署 Imboy 后端；首次边界迁移会先停旧节点并进入维护窗口。
# Blue-green deployment; the first boundary migration stops the old node for maintenance.
#
# 执行流程 / Steps:
#   1. 检测当前运行色（蓝/绿）  Detect active color (blue/green)
#   2. 选择对立色为部署目标     Pick opposite color as deploy target
#   3. 服务器拉代码 + 编译 release（内嵌 ERTS）
#      Pull code + build self-contained release (ERTS embedded, no symlinks)
#   4. 解包到独立目录，生成 vm.args
#      Extract to isolated dir and write vm.args
#   5. 执行已批准的 expand 迁移 Apply approved expand migrations
#   6. 禁用启动迁移并启动新节点 Start node with startup migrations disabled
#   7. 切流并停止旧节点长连接   Switch traffic and drain old node
#   8. 显式执行完整数据库迁移   Explicitly run remaining DB migrations
#   9. 输出部署结果              Print deployment result
#
# 内部实现，仅由 scripts/imboy-deploy.sh 调用。
# Operator entry point: bash ./scripts/imboy-deploy.sh <api|all|migrate|rollback>
#
# 内部环境变量 / Internal environment variables:
#   IMBOY_DEPLOY_USER        SSH 用户       SSH user            (default: root)
#   IMBOY_DEPLOY_PORT        SSH 端口       SSH port            (default: 32)
#   IMBOY_DEPLOY_PROJECT_DIR 远端项目目录   Remote project dir  (default: /www/wwwroot/imboy-api)
#   IMBOY_DEPLOY_NGINX_CONF  Nginx 配置路径 Nginx conf path
#   IMBOY_DEPLOY_PRODADM_CONF Admin vhost path (optional when not installed)
#   IMBOY_DEPLOY_CS_NGINX_CONF CS vhost path (optional when not installed)
#   IMBOY_DEPLOY_BLUE_PORT   蓝端口         Blue port           (default: 9800)
#   IMBOY_DEPLOY_GREEN_PORT  绿端口         Green port          (default: 9801)
#   IMBOY_DEPLOY_NODE_HOST   节点 host      Node host           (default: 127.0.0.1)
#   IMBOY_DEPLOY_COOKIE      节点 cookie    Node cookie         (default: 见 config/vm.args)
#   IMBOY_DEPLOY_BRANCH      部署分支       Deploy branch       (default: main)
#   IMBOY_DEPLOY_SOURCE_HEAD 本地已确认的 40 位 Git HEAD
#   IMBOY_DEPLOY_STOP_OLD    完整迁移必须为 true；--no-migrate 时强制为 false
#   IMBOY_DEPLOY_DB_CONTAINER PostgreSQL 容器名  PostgreSQL container
#   IMBOY_DEPLOY_DB_NAME      PostgreSQL 数据库名 Database name
#   IMBOY_DEPLOY_DB_USER      PostgreSQL 用户名  Database user
#   IMBOY_DEPLOY_EXPAND_MIGRATIONS 切流前执行的可加性迁移文件（空格分隔）
#   IMBOY_DEPLOY_SALES_RELEASE 销售版门禁（default: true）
#   IMBOY_DEPLOY_E2EE_MODE     节点 E2EE 模式（销售版 default: required；其他: disabled）
#   IMBOY_DEPLOY_PLUGIN_TRUSTED_PUBLIC_KEY_FILE 本地 32 字节 Ed25519 公钥
# =============================================================================

# ---------- 静默控制 / Verbosity control ----------
# 默认静默；-v/--verbose 透传远端命令输出
# Quiet by default; -v/--verbose passes SSH output through
SILENT=1
LOCAL_MODE=0
SKIP_MIGRATE=0
ROLLBACK=0
while [[ $# -gt 0 ]]; do
  case "$1" in
    -v|--verbose)    SILENT=0; shift ;;
    -s|--silent)     SILENT=1; shift ;;
    -l|--local)      LOCAL_MODE=1; shift ;;
    -M|--no-migrate) SKIP_MIGRATE=1; shift ;;
    --rollback)      ROLLBACK=1; shift ;;
    --) shift; break ;;
    -*) echo "未知参数 / Unknown flag: $1" >&2; exit 1 ;;
    *)  break ;;
  esac
done

if [ $# -ne 3 ]; then
  grep '^# 用法' "$0" -A 3 | sed 's/^# //'
  exit 1
fi

# ---------- 参数 / Parameters ----------
SERVER_HOST="$1"
VSN="$2"
NODE_NAME="$3"

# 版本一致性门禁：VERSION 文件（app 级 vsn，PROJECT_VERSION=$(cat VERSION)）
# 必须与目标版本一致，否则远端构建重建 ebin/imboy.app 时会写回旧版，
# /healthz 自报版本失配导致 wait_for_health 永远失败。
if [ -f "$SCRIPT_DIR/../VERSION" ]; then
  FILE_VSN="$(head -n1 "$SCRIPT_DIR/../VERSION" | tr -d '[:space:]')"
  if [ -n "$FILE_VSN" ] && [ "$FILE_VSN" != "$VSN" ]; then
    echo "✗ VERSION 文件 ($FILE_VSN) 与目标版本 ($VSN) 不一致 / VERSION file and target VSN mismatch" >&2
    echo "  将 .env.deploy 的 DEPLOY_VSN 改为 $FILE_VSN，或提交 VERSION 变更（relx 版本行已由脚本自动对齐）" >&2
    exit 1
  fi
fi

SERVER_USER="${IMBOY_DEPLOY_USER:-root}"
SERVER_PORT="${IMBOY_DEPLOY_PORT:-32}"
PROJECT_DIR="${IMBOY_DEPLOY_PROJECT_DIR:-/www/wwwroot/imboy-api}"
NGINX_CONF="${IMBOY_DEPLOY_NGINX_CONF:-/www/server/panel/vhost/nginx/pro.imboy.pub.conf}"
# 管理后台 vhost 直接 proxy_pass 到应用端口（不走 upstream），切流时必须跟切，
# 否则 admin API 打到已停止的旧槽位（alpha.72/alpha.73 两次实战踩坑）。
PRODADM_CONF="${IMBOY_DEPLOY_PRODADM_CONF:-/www/server/panel/vhost/nginx/prodadm.imboy.pub.conf}"
CS_NGINX_CONF="${IMBOY_DEPLOY_CS_NGINX_CONF:-}"
BLUE_PORT="${IMBOY_DEPLOY_BLUE_PORT:-9800}"
GREEN_PORT="${IMBOY_DEPLOY_GREEN_PORT:-9801}"
LEGACY_PORT="${IMBOY_DEPLOY_LEGACY_PORT:-}"
LEGACY_RELEASE_DIR="${IMBOY_DEPLOY_LEGACY_RELEASE_DIR:-}"
NODE_HOST="${IMBOY_DEPLOY_NODE_HOST:-127.0.0.1}"
COOKIE="${IMBOY_DEPLOY_COOKIE:-imboy_local_dev_only}"
BRANCH="${IMBOY_DEPLOY_BRANCH:-main}"
SOURCE_HEAD="${IMBOY_DEPLOY_SOURCE_HEAD:-}"
# --local 模式下远端迁移 staging 目录（空 = 非 -l 模式，迁移读 release 内 priv/migrations）
MIGRATE_STAGING=""
STOP_OLD="${IMBOY_DEPLOY_STOP_OLD:-true}"
DB_CONTAINER="${IMBOY_DEPLOY_DB_CONTAINER:-}"
DB_NAME="${IMBOY_DEPLOY_DB_NAME:-}"
DB_USER="${IMBOY_DEPLOY_DB_USER:-}"
EXPAND_MIGRATIONS="${IMBOY_DEPLOY_EXPAND_MIGRATIONS:-}"
PLUGIN_TRUSTED_PUBLIC_KEY_FILE="${IMBOY_DEPLOY_PLUGIN_TRUSTED_PUBLIC_KEY_FILE:-}"
TSID_STATE_DIR="${IMBOY_DEPLOY_TSID_STATE_DIR:-}"
TSID_BLUE_NODE_ID="${IMBOY_DEPLOY_TSID_BLUE_NODE_ID:-}"
TSID_GREEN_NODE_ID="${IMBOY_DEPLOY_TSID_GREEN_NODE_ID:-}"
TSID_BOOTSTRAP_MODE="${IMBOY_DEPLOY_TSID_BOOTSTRAP_MODE:-}"
TSID_BOOTSTRAP_FLOOR_UNIX_MS="${IMBOY_DEPLOY_TSID_BOOTSTRAP_FLOOR_UNIX_MS:-}"
TSID_BOOTSTRAP_LEGACY_ACK="${IMBOY_DEPLOY_TSID_BOOTSTRAP_LEGACY_ACK:-}"
TSID_BOOTSTRAP_REBIND_ACK="${IMBOY_DEPLOY_TSID_BOOTSTRAP_REBIND_ACK:-}"
BOUNDARY_CUTOVER_MARKER="$PROJECT_DIR/.deploy-c2g-boundary-v109-ready"
E2EE_ATTESTATION_SCHEMA_PREDICATE="to_regclass('public.e2ee_group_session_attestation') IS NOT NULL AND to_regclass('public.e2ee_group_session_member') IS NOT NULL AND (SELECT count(*) FROM information_schema.columns WHERE table_schema='public' AND table_name='e2ee_group_session_attestation' AND is_nullable='NO' AND (ordinal_position || ':' || column_name || ':' || udt_name) IN ('1:group_id:int8','2:session_id:varchar','3:sender_uid:int8','4:sender_did:varchar','5:room_key_msg_id:varchar','6:recipient_uids:_int8','7:start_seq:int8','8:end_seq:int8','9:created_at:timestamptz','10:updated_at:timestamptz')) = 10 AND (SELECT count(*) FROM information_schema.columns WHERE table_schema='public' AND table_name='e2ee_group_session_attestation' AND (column_name, character_maximum_length) IN (('session_id',256),('sender_did',128),('room_key_msg_id',40))) = 3 AND (SELECT count(*) FROM information_schema.columns WHERE table_schema='public' AND table_name='e2ee_group_session_member' AND is_nullable='NO' AND (ordinal_position || ':' || column_name || ':' || udt_name) IN ('1:group_id:int8','2:session_id:varchar','3:user_id:int8','4:generation_no:int4','5:generation_start_seq:int8')) = 5 AND EXISTS (SELECT 1 FROM information_schema.columns WHERE table_schema='public' AND table_name='e2ee_group_session_member' AND column_name='session_id' AND character_maximum_length=256) AND (SELECT count(*) FROM pg_constraint WHERE conrelid=to_regclass('public.e2ee_group_session_attestation') AND convalidated AND ((conname='e2ee_group_session_attestation_pkey' AND contype='p' AND conkey=ARRAY[1,2]::smallint[]) OR (conname='e2ee_group_session_attestation_session_id_key' AND contype='u' AND conkey=ARRAY[2]::smallint[]) OR (conname='e2ee_group_session_attestation_room_key_msg_id_key' AND contype='u' AND conkey=ARRAY[5]::smallint[]) OR (conname='chk_e2ee_group_session_ids' AND contype='c' AND conkey=ARRAY[1,3,2,4,5]::smallint[] AND pg_get_constraintdef(oid,true)='CHECK (group_id > 0 AND sender_uid > 0 AND octet_length(session_id::text) >= 1 AND octet_length(session_id::text) <= 256 AND octet_length(sender_did::text) >= 1 AND octet_length(sender_did::text) <= 128 AND octet_length(room_key_msg_id::text) >= 1 AND octet_length(room_key_msg_id::text) <= 40)') OR (conname='chk_e2ee_group_session_range' AND contype='c' AND conkey=ARRAY[7,8]::smallint[] AND pg_get_constraintdef(oid,true)='CHECK (start_seq >= 1 AND end_seq >= start_seq)') OR (conname='chk_e2ee_group_session_recipients' AND contype='c' AND conkey=ARRAY[6]::smallint[] AND pg_get_constraintdef(oid,true)='CHECK (array_ndims(recipient_uids) = 1 AND cardinality(recipient_uids) >= 1 AND cardinality(recipient_uids) <= 5000 AND array_position(recipient_uids, NULL::bigint) IS NULL AND (0 < ALL (recipient_uids)))'))) = 6 AND (SELECT count(*) FROM pg_constraint WHERE conrelid=to_regclass('public.e2ee_group_session_member') AND convalidated AND ((conname='e2ee_group_session_member_pkey' AND contype='p' AND conkey=ARRAY[1,2,3]::smallint[]) OR (conname='fk_e2ee_group_session_member_session' AND contype='f' AND conkey=ARRAY[1,2]::smallint[] AND confrelid=to_regclass('public.e2ee_group_session_attestation') AND confkey=ARRAY[1,2]::smallint[] AND confupdtype='a' AND confdeltype='a' AND confmatchtype='s') OR (conname='chk_e2ee_group_session_member_values' AND contype='c' AND conkey=ARRAY[1,3,4,5]::smallint[] AND pg_get_constraintdef(oid,true)='CHECK (group_id > 0 AND user_id > 0 AND generation_no > 0 AND generation_start_seq >= 1)'))) = 3 AND EXISTS (SELECT 1 FROM pg_indexes WHERE schemaname='public' AND tablename='e2ee_group_session_member' AND indexname='idx_e2ee_group_session_member_grant' AND indexdef LIKE '%(group_id, user_id, generation_no, session_id)%')"
SALES_RELEASE="${IMBOY_DEPLOY_SALES_RELEASE:-true}"
if [ "$SALES_RELEASE" = "true" ]; then
  E2EE_MODE="${IMBOY_DEPLOY_E2EE_MODE:-required}"
else
  E2EE_MODE="${IMBOY_DEPLOY_E2EE_MODE:-disabled}"
fi
# --local 模式：从本地 rsync 源码到远端，跳过 git pull
# --local mode: rsync local source to remote, skip git pull
LOCAL_SRC_DIR="${IMBOY_LOCAL_SRC_DIR:-$(cd "$SCRIPT_DIR/.." && pwd)}"
OLD_NODE_STOPPED=0
OLD_DIR=""
TRAFFIC_SWITCHED=0
FAIL_RECOVERY_ATTEMPTED=0
CANDIDATE_STARTED=0
BOUNDARY_BOOTSTRAP=0
BOUNDARY_CUTOVER_PENDING=0
BOUNDARY_SCHEMA_REQUIRED=0

RELEASE_DIR="/usr/local/imboy-${VSN}-${NODE_NAME}"
RELEASE_TARBALL="${PROJECT_DIR}/_rel/imboy/imboy-${VSN}.tar.gz"
PLUGIN_TRUSTED_PUBLIC_KEY_REMOTE="$RELEASE_DIR/etc/plugin_trusted_ed25519.pub"

# 校验参数格式，防止注入 vm.args 或 rm -rf 路径
# Validate inputs to prevent vm.args injection and path anomalies
[[ "$SERVER_HOST" =~ ^[a-zA-Z0-9._-]+$  ]] || { echo "无效 SERVER_HOST / invalid SERVER_HOST: $SERVER_HOST" >&2; exit 1; }
[[ "$SERVER_USER" =~ ^[a-zA-Z_][a-zA-Z0-9_-]*$ ]] || { echo "无效 SERVER_USER / invalid SERVER_USER" >&2; exit 1; }
[[ "$SERVER_PORT" =~ ^[0-9]+$ ]] && [ "$SERVER_PORT" -ge 1 ] && [ "$SERVER_PORT" -le 65535 ] \
  || { echo "无效 SERVER_PORT / invalid SERVER_PORT" >&2; exit 1; }
[[ "$BRANCH"      =~ ^[a-zA-Z0-9._/-]+$ ]] || { echo "无效 BRANCH / invalid BRANCH: $BRANCH" >&2; exit 1; }
[[ "$BRANCH" != -* && "$BRANCH" != *..* ]] || { echo "危险 BRANCH / unsafe BRANCH: $BRANCH" >&2; exit 1; }
[[ "$VSN"       =~ ^[a-zA-Z0-9._-]+$ ]] || { echo "无效 VSN / invalid VSN: $VSN" >&2; exit 1; }
[[ "$NODE_NAME" =~ ^[a-zA-Z0-9_-]+$  ]] || { echo "NODE_NAME 含非法字符 / invalid NODE_NAME: $NODE_NAME" >&2; exit 1; }
[[ "$NODE_HOST" =~ ^[a-zA-Z0-9._-]+$ ]] || { echo "NODE_HOST 含非法字符 / invalid NODE_HOST" >&2; exit 1; }
[[ "$COOKIE"    =~ ^[a-zA-Z0-9_-]+$  ]] || { echo "COOKIE 含非法字符 / invalid COOKIE: $COOKIE" >&2; exit 1; }
[[ "$BLUE_PORT" =~ ^[0-9]+$ ]] && [ "$BLUE_PORT" -ge 1024 ] && [ "$BLUE_PORT" -le 65535 ] \
  || { echo "无效 BLUE_PORT / invalid BLUE_PORT" >&2; exit 1; }
[[ "$GREEN_PORT" =~ ^[0-9]+$ ]] && [ "$GREEN_PORT" -ge 1024 ] && [ "$GREEN_PORT" -le 65535 ] \
  || { echo "无效 GREEN_PORT / invalid GREEN_PORT" >&2; exit 1; }
[[ "$BLUE_PORT" != "$GREEN_PORT" ]] || { echo "蓝绿端口不得相同 / blue and green ports must differ" >&2; exit 1; }
if [ -n "$LEGACY_PORT" ]; then
  [[ "$LEGACY_PORT" =~ ^[0-9]+$ ]] && [ "$LEGACY_PORT" -ge 1024 ] && [ "$LEGACY_PORT" -le 65535 ] \
    || { echo "无效 LEGACY_PORT / invalid legacy port" >&2; exit 1; }
  [[ "$LEGACY_PORT" != "$BLUE_PORT" && "$LEGACY_PORT" != "$GREEN_PORT" ]] \
    || { echo "LEGACY_PORT 不得与蓝绿端口相同" >&2; exit 1; }
fi
if [ -n "$LEGACY_RELEASE_DIR" ]; then
  [[ "$LEGACY_RELEASE_DIR" =~ ^/usr/local/imboy-[a-zA-Z0-9._-]+-[a-zA-Z0-9_-]+$ ]] \
    || { echo "LEGACY_RELEASE_DIR 不符合安全 release 路径模板" >&2; exit 1; }
  [ -n "$LEGACY_PORT" ] \
    || { echo "LEGACY_RELEASE_DIR 要求同时配置 LEGACY_PORT" >&2; exit 1; }
fi
if [ -n "$TSID_STATE_DIR" ]; then
  [[ "$TSID_STATE_DIR" =~ ^/[a-zA-Z0-9._/-]+$ && "$TSID_STATE_DIR" != "/" \
     && "$TSID_STATE_DIR" != *..* ]] \
    || { echo "无效 TSID_STATE_DIR / invalid TSID state dir" >&2; exit 1; }
  [[ "$TSID_BLUE_NODE_ID" =~ ^[0-9]+$ && "$TSID_GREEN_NODE_ID" =~ ^[0-9]+$ ]] \
    && [ "$TSID_BLUE_NODE_ID" -le 127 ] && [ "$TSID_GREEN_NODE_ID" -le 127 ] \
    && [ "$TSID_BLUE_NODE_ID" != "$TSID_GREEN_NODE_ID" ] \
    || { echo "蓝绿 TSID node_id 必须不同且位于 0..127" >&2; exit 1; }
fi
case "$TSID_BOOTSTRAP_MODE" in ""|auto_scan|manual_floor) ;; *) echo "无效 TSID bootstrap mode" >&2; exit 1 ;; esac
if [ "$TSID_BOOTSTRAP_MODE" = manual_floor ]; then
  [[ "$TSID_BOOTSTRAP_FLOOR_UNIX_MS" = now || "$TSID_BOOTSTRAP_FLOOR_UNIX_MS" =~ ^[0-9]{13}$ ]] \
    || { echo "manual_floor 需要 now 或 13 位 unix 毫秒" >&2; exit 1; }
fi
if [ -n "$TSID_BOOTSTRAP_LEGACY_ACK" ] \
   && [ "$TSID_BOOTSTRAP_LEGACY_ACK" != "I-CONFIRM-OLD-WRITER-STOPPED" ]; then
  echo "无效 TSID legacy ACK" >&2
  exit 1
fi
if [ -n "$TSID_BOOTSTRAP_REBIND_ACK" ] \
   && [[ ! "$TSID_BOOTSTRAP_REBIND_ACK" =~ ^I-CONFIRM-OLD-WRITER-STOPPED-AND-REBIND:[0-9A-Fa-f]{64}:[0-9A-Fa-f]{64}$ ]]; then
  echo "无效 TSID rebind ACK" >&2
  exit 1
fi
[[ "$PROJECT_DIR" =~ ^/[a-zA-Z0-9._/-]+$ && "$PROJECT_DIR" != *..* ]] \
  || { echo "PROJECT_DIR 必须是无 .. 的安全绝对路径 / unsafe PROJECT_DIR" >&2; exit 1; }
[[ "$NGINX_CONF" =~ ^/[a-zA-Z0-9._/-]+$ && "$NGINX_CONF" != *..* ]] \
  || { echo "NGINX_CONF 必须是无 .. 的安全绝对路径 / unsafe NGINX_CONF" >&2; exit 1; }
[[ "$PRODADM_CONF" =~ ^/[a-zA-Z0-9._/-]+$ && "$PRODADM_CONF" != "/" && "$PRODADM_CONF" != *..* ]] \
  || { echo "PRODADM_CONF 必须是无 .. 的安全绝对路径 / unsafe PRODADM_CONF" >&2; exit 1; }
if [ -n "$CS_NGINX_CONF" ]; then
  [[ "$CS_NGINX_CONF" =~ ^/[a-zA-Z0-9._/-]+$ && "$CS_NGINX_CONF" != "/" && "$CS_NGINX_CONF" != *..* ]] \
    || { echo "CS_NGINX_CONF 必须是无 .. 的安全绝对路径 / unsafe CS_NGINX_CONF" >&2; exit 1; }
fi
[[ "$LOCAL_SRC_DIR" == /* && -d "$LOCAL_SRC_DIR" ]] \
  || { echo "LOCAL_SRC_DIR 必须是存在的绝对目录 / invalid LOCAL_SRC_DIR" >&2; exit 1; }
case "$SALES_RELEASE" in true|false) ;; *) echo "IMBOY_DEPLOY_SALES_RELEASE 只能为 true/false" >&2; exit 1 ;; esac
case "$E2EE_MODE" in disabled|optional|required|compliance) ;; *) echo "IMBOY_DEPLOY_E2EE_MODE 非法" >&2; exit 1 ;; esac
if [ "$ROLLBACK" -eq 0 ]; then
  [[ "$SOURCE_HEAD" =~ ^[0-9a-f]{40}$ ]] \
    || { echo "IMBOY_DEPLOY_SOURCE_HEAD 必须是 40 位 Git SHA" >&2; exit 1; }
fi
if [ "$SALES_RELEASE" = "true" ] && [ "$E2EE_MODE" != "required" ] && [ "$E2EE_MODE" != "compliance" ]; then
  echo "销售版 IMBOY_DEPLOY_E2EE_MODE 必须为 required/compliance" >&2
  exit 1
fi
# product_profile=enterprise 时插件签名在 boot 强制（imboy_plugin_signature:signature_required/0），
# 缺可信公钥 = 启动即崩（invalid_plugin_signature_config）+ heart 复活循环。
# 公钥非机密，只要配置了本地公钥就随 release 安装，与销售版开关解耦。
if [ "$ROLLBACK" -eq 0 ] && [ -n "$PLUGIN_TRUSTED_PUBLIC_KEY_FILE" ]; then
  [[ "$PLUGIN_TRUSTED_PUBLIC_KEY_FILE" == /* && -f "$PLUGIN_TRUSTED_PUBLIC_KEY_FILE" \
     && -r "$PLUGIN_TRUSTED_PUBLIC_KEY_FILE" ]] \
    || { echo "缺少可读的本地 Ed25519 插件签名公钥（需绝对路径）: $PLUGIN_TRUSTED_PUBLIC_KEY_FILE" >&2; exit 1; }
  [[ "$(wc -c <"$PLUGIN_TRUSTED_PUBLIC_KEY_FILE" | tr -d '[:space:]')" == 32 ]] \
    || { echo "插件签名可信公钥必须是 32 字节 raw public key" >&2; exit 1; }
fi
[[ "$RELEASE_DIR" == /usr/local/imboy-?* ]] || { echo "RELEASE_DIR 路径异常 / anomalous RELEASE_DIR: $RELEASE_DIR" >&2; exit 1; }
if [[ -n "$DB_CONTAINER" && ! "$DB_CONTAINER" =~ ^[a-zA-Z0-9_.-]+$ ]]; then
  echo "IMBOY_DEPLOY_DB_CONTAINER 含非法字符 / invalid DB container" >&2
  exit 1
fi
if [[ -n "$DB_NAME" && ! "$DB_NAME" =~ ^[a-zA-Z0-9_.-]+$ ]]; then
  echo "IMBOY_DEPLOY_DB_NAME 含非法字符 / invalid DB name" >&2
  exit 1
fi
if [[ -n "$DB_USER" && ! "$DB_USER" =~ ^[a-zA-Z0-9_.-]+$ ]]; then
  echo "IMBOY_DEPLOY_DB_USER 含非法字符 / invalid DB user" >&2
  exit 1
fi

# 规范化 STOP_OLD：接受 true/1/yes（大小写不敏感）
# Normalize STOP_OLD: accept true/1/yes case-insensitively
case "$(echo "$STOP_OLD" | tr '[:upper:]' '[:lower:]')" in true|1|yes) STOP_OLD=true ;; *) STOP_OLD=false ;; esac

# ---------- 日志函数 / Log helpers ----------
log()  { echo -e "\033[36m[$(date '+%H:%M:%S')] $*\033[0m"; }
ok()   { echo -e "\033[32m✓ $*\033[0m"; }
fail() {
  local message="$*"
  trap - ERR
  if declare -F cleanup_new_node_before_cutover >/dev/null 2>&1; then
    cleanup_new_node_before_cutover || true
  fi
  if declare -F recover_old_node_before_cutover >/dev/null 2>&1; then
    recover_old_node_before_cutover || true
  fi
  echo -e "\033[31m✗ $message\033[0m" >&2
  exit 1
}

if [ "$SKIP_MIGRATE" -eq 1 ] && [ "$STOP_OLD" = "true" ]; then
  STOP_OLD=false
  log "--no-migrate 强制保留旧节点；本次仅允许发布 schema 兼容代码 / keeping old node"
elif [ "$SKIP_MIGRATE" -eq 0 ] && [ "$STOP_OLD" != "true" ]; then
  fail "完整迁移前必须停止旧节点及其长连接；如需保留旧节点，请使用 --no-migrate"
fi

trap 'fail "脚本意外终止，使用 -v 查看详情 / Aborted — rerun with -v for details"' ERR

# =============================================================================
# SSH ControlMaster：建立一条持久主连接，后续命令复用，避免重复握手
# SSH ControlMaster: one persistent TCP connection reused by all ssh_exec calls
# =============================================================================
SSH_CTRL="/tmp/imboy-deploy-$$"
SSH_OPTS=(-p "$SERVER_PORT" -o ControlPath="$SSH_CTRL" -o StrictHostKeyChecking=accept-new)

_cleanup() {
  ssh "${SSH_OPTS[@]}" -O exit "$SERVER_USER@$SERVER_HOST" 2>/dev/null || true
  rm -f "$SSH_CTRL"
}
trap '_cleanup' EXIT

log "连接服务器 $SERVER_USER@$SERVER_HOST:$SERVER_PORT / Connecting to server..."
ssh -fNM -o ControlMaster=yes "${SSH_OPTS[@]}" "$SERVER_USER@$SERVER_HOST"
ok "SSH 连接就绪 / SSH connection ready"

# 执行远端命令；静默模式丢弃输出，详细模式透传
# Run remote command; discard output in silent, pass through in verbose
ssh_exec() {
  if [ "$SILENT" -eq 1 ]; then
    ssh -n "${SSH_OPTS[@]}" "$SERVER_USER@$SERVER_HOST" "$1" >/dev/null 2>&1
  else
    ssh -n "${SSH_OPTS[@]}" "$SERVER_USER@$SERVER_HOST" "$1"
  fi
}

# 捕获远端 stdout（不走 ssh_exec，避免静默模式将输出丢入 /dev/null）
# Capture remote stdout — bypass ssh_exec to avoid silent-mode discard
ssh_capture() {
  ssh -n "${SSH_OPTS[@]}" "$SERVER_USER@$SERVER_HOST" "$1" | tr -d '\r'
}

ssh_upload() {
  local source_file=$1 target_file=$2
  ssh "${SSH_OPTS[@]}" "$SERVER_USER@$SERVER_HOST" \
    "umask 022; cat > '$target_file'" <"$source_file"
}

safe_release_dir() {
  local dir=$1
  [[ "$dir" =~ ^/usr/local/imboy-[a-zA-Z0-9._-]+-[a-zA-Z0-9_-]+$ \
     || "$dir" = "$PROJECT_DIR/_rel/imboy" ]]
}

find_running_release_for_port() {
  local port=$1
  ssh_capture "
    command -v lsof >/dev/null 2>&1 || exit 2
    PID=\$(lsof -ti:$port -sTCP:LISTEN | head -1) || exit 3
    [ -n \"\$PID\" ] || exit 4
    tr '\\0' '\\n' < \"/proc/\$PID/cmdline\" 2>/dev/null \
      | awk 'p { print; exit } /^-root\$/ { p=1 }'
  "
}

stop_release_processes() {
  local dir=$1
  safe_release_dir "$dir" || return 2
  ssh_exec "
    command -v pgrep >/dev/null 2>&1 || exit 2
    for round in TERM KILL; do
      for H in \$(pgrep -x heart || true); do
        if tr '\\0' '\\n' < \"/proc/\$H/environ\" 2>/dev/null \
             | grep -Fq \"HEART_COMMAND=\\\"$dir\"; then
          kill -\$round \$H 2>/dev/null || true
        fi
      done
      for P in \$(pgrep -x beam.smp || true); do
        ROOT=\$(tr '\\0' '\\n' < \"/proc/\$P/cmdline\" 2>/dev/null \
          | awk 'p { print; exit } /^-root\$/ { p=1 }')
        [ \"\$ROOT\" = '$dir' ] && kill -\$round \$P 2>/dev/null || true
      done
      [ \"\$round\" = TERM ] && sleep 2
    done
    sleep 1
    for H in \$(pgrep -x heart || true); do
      tr '\\0' '\\n' < \"/proc/\$H/environ\" 2>/dev/null \
        | grep -Fq \"HEART_COMMAND=\\\"$dir\" && exit 1
    done
    for P in \$(pgrep -x beam.smp || true); do
      ROOT=\$(tr '\\0' '\\n' < \"/proc/\$P/cmdline\" 2>/dev/null \
        | awk 'p { print; exit } /^-root\$/ { p=1 }')
      [ \"\$ROOT\" = '$dir' ] && exit 1
    done
    exit 0
  "
}

cleanup_new_node_before_cutover() {
  [ "$CANDIDATE_STARTED" -eq 1 ] || return 0
  [ "$TRAFFIC_SWITCHED" -eq 0 ] || return 0
  CANDIDATE_STARTED=0
  log "回收切流前失败的新节点: $RELEASE_DIR"
  stop_release_processes "$RELEASE_DIR"
}

cleanup_inactive_target() {
  local dir dirs
  # 显式捕获失败；进程替换中的 ERR trap 会在子 shell 误启动恢复，主流程却继续。
  dirs="$(ssh_capture "
    for DIR in /usr/local/imboy-*; do
      [ -d \"\$DIR\" ] || continue
      if grep -qsE '\\{http_port,[[:space:]]*$APP_PORT\\}' \"\$DIR\"/releases/*/sys.config; then
        printf '%s\\n' \"\$DIR\"
      fi
    done
    exit 0
  ")" || fail "无法枚举目标槽 release，拒绝继续发布"
  while IFS= read -r dir; do
    [ -n "$dir" ] || continue
    [ "$dir" != "${ACTIVE_DIR:-}" ] || continue
    safe_release_dir "$dir" || fail "目标端口残留 release 目录不安全: $dir"
    log "清理目标槽残留进程: $dir (port=$APP_PORT)"
    stop_release_processes "$dir" \
      || fail "目标槽残留进程无法安全停止: $dir"
  done <<<"$dirs"
  ssh_exec "! ss -tlnH \"sport = :$APP_PORT\" 2>/dev/null | grep -q ." \
    || fail "目标端口 $APP_PORT 仍被未知进程占用，拒绝误杀"
}

# 轮询端口，最多等 40s（每 2s 一次，共 20 次）
# Poll until port is bound, timeout 40 s (2 s × 20 attempts)
# 单次 SSH 调用在远端执行整个等待循环，避免 20 次往返
# Single SSH call runs the entire wait loop remotely, avoiding 20 round trips
# C-51：只探端口是**不够**的。
#   目标色端口上残留着上一次部署的进程时，`ss` 立刻就能看到端口被监听，
#   于是部署判定"就绪"→ 切流 → 流量被打到**旧二进制**上，而部署日志全绿。
#   本次改为探 /healthz 并核对它自报的版本号：
#     - HTTP 200  ⇒ 进程真的能服务（且 PG 可达，见 C-49 的 503 语义）
#     - version 匹配 ⇒ 端口后面站着的确实是这次要发的那个版本
# 缺任何一条都判失败，宁可部署中止也不要把流量切到错误的进程上。
wait_for_health() {
  local port=$1 expect_vsn=$2
  ssh_exec "
    for i in \$(seq 1 20); do
      BODY=\$(curl -fsS --max-time 3 \"http://127.0.0.1:$port/healthz\" 2>/dev/null || true)
      case \"\$BODY\" in
        *'\"status\":\"ok\"'*)
          case \"\$BODY\" in
            *'\"version\":\"$expect_vsn\"'*) exit 0 ;;
            *) echo \"就绪但版本不符 / ready but version mismatch: \$BODY\" >&2 ;;
          esac
          ;;
      esac
      sleep 2
    done
    exit 1
  "
}

wait_for_health_status() {
  local port=$1
  ssh_exec "
    RECOVERY_HEALTH=1
    for i in \$(seq 1 20); do
      BODY=\$(curl -fsS --max-time 3 \"http://127.0.0.1:$port/healthz\" 2>/dev/null || true)
      case \"\$BODY\" in
        *'\"status\":\"ok\"'*) exit 0 ;;
      esac
      sleep 2
    done
    exit 1
  "
}

probe_nginx_port() {
  ssh_capture "
    [ -r '$NGINX_CONF' ] || exit 2
    PORTS=\$(sed -nE 's/^[[:space:]]*server[[:space:]]+127\\.0\\.0\\.1:([0-9]+);[[:space:]]*$/\\1/p' '$NGINX_CONF' | sort -u)
    set -- \$PORTS
    [ \$# -eq 1 ] || { [ \$# -eq 0 ] && echo none || echo conflict; exit 0; }
    echo \"\$1\"
  "
}

probe_nginx_color() {
  local port
  port="$(probe_nginx_port)" || return
  case "$port" in
    "$BLUE_PORT") echo blue ;;
    "$GREEN_PORT") echo green ;;
    none|conflict) echo "$port" ;;
    *) echo legacy ;;
  esac
}

find_release_for_port() {
  local port=$1
  ssh_capture "
    for DIR in \$(ls -dt /usr/local/imboy-* 2>/dev/null); do
      [ -x \"\$DIR/bin/imboy\" ] || continue
      if grep -qsE '\\{http_port,[[:space:]]*$port\\}' \"\$DIR\"/releases/*/sys.config; then
        printf '%s\\n' \"\$DIR\"
        exit 0
      fi
    done
    exit 1
  " || return 1
}

find_stopped_release_for_port() {
  local port=$1 dir
  if dir="$(find_release_for_port "$port")" && [ -n "$dir" ]; then
    printf '%s\n' "$dir"
    return 0
  fi
  if [ "$port" = "$LEGACY_PORT" ] && [ -n "$LEGACY_RELEASE_DIR" ] \
     && ssh_exec "[ -x '$LEGACY_RELEASE_DIR/bin/imboy' ]"; then
    printf '%s\n' "$LEGACY_RELEASE_DIR"
    return 0
  fi
  return 1
}

# 保留端口探测供"旧节点是否还活着"这类不关心版本的判断使用。
# ⚠️ 不要再拿它当**部署就绪**判据 —— 那正是 C-51 修掉的坑。
wait_for_port() {
  local port=$1
  ssh_exec "
    for i in \$(seq 1 20); do
      ss -tlnH \"sport = :$port\" 2>/dev/null | grep -q . && exit 0
      sleep 2
    done
    exit 1
  "
}

wait_for_port_closed() {
  local port=$1
  ssh_exec "
    for i in \$(seq 1 20); do
      LISTENERS=\$(ss -tlnH \"sport = :$port\") || exit 2
      [ -z \"\$LISTENERS\" ] && exit 0
      sleep 1
    done
    exit 1
  "
}

stop_old_node() {
  [ "$OLD_NODE_STOPPED" -eq 0 ] || return 0
  [ -n "$OLD_PORT" ] || return 0
  log "停止旧节点并关闭既有长连接 (port=$OLD_PORT)... / Draining old node..."
  OLD_DIR="$(find_running_release_for_port "$OLD_PORT")" || OLD_DIR=""
  [ -n "$OLD_DIR" ] || fail "无法定位旧节点 release 目录，拒绝在旧连接仍存活时迁移"
  safe_release_dir "$OLD_DIR" \
    || fail "旧节点 release 目录不符合安全模板，拒绝拼入远端命令: $OLD_DIR"
  # 从这里开始的任意失败都必须尝试恢复原节点，包括 stop 部分成功的情况。
  OLD_NODE_STOPPED=1
  if ! ssh_exec "command -v timeout >/dev/null 2>&1 && timeout 20s '$OLD_DIR/bin/imboy' stop"; then
    log "旧节点 20s 内未优雅停止，继续按 release 根目录精确回收 heart/beam"
  fi
  stop_release_processes "$OLD_DIR" \
    || fail "旧节点 stop 返回后仍有归属进程，拒绝执行完整迁移"
  wait_for_port_closed "$OLD_PORT" \
    || fail "旧节点端口在 20s 后仍开放，拒绝执行完整迁移"
  ok "旧节点已停止，既有 WebSocket 已断开并将重连到新节点"
}

recover_old_node_before_cutover() {
  [ "$OLD_NODE_STOPPED" -eq 1 ] || return 0
  [ "$TRAFFIC_SWITCHED" -eq 0 ] || return 0
  [ "$FAIL_RECOVERY_ATTEMPTED" -eq 0 ] || return 0
  [ -n "$OLD_PORT" ] && [ -n "$OLD_DIR" ] || return 0
  FAIL_RECOVERY_ATTEMPTED=1

  log "自动恢复 Nginx 当前指向的原节点 (port=$OLD_PORT)..."
  ssh_exec "
    if [ -x '$RELEASE_DIR/bin/imboy' ]; then
      command -v timeout >/dev/null 2>&1 && timeout 10s '$RELEASE_DIR/bin/imboy' stop >/dev/null 2>&1 || true
    fi
    cd '$OLD_DIR'
    if [ -f '$OLD_DIR/etc/plugin_trusted_ed25519.pub' ] \
       && find '$OLD_DIR/etc/plugin_trusted_ed25519.pub' -prune -size 32c | grep -q .; then
      export IMBOY_PLUGIN_TRUSTED_PUBLIC_KEY_FILES='$OLD_DIR/etc/plugin_trusted_ed25519.pub'
    fi
    IMBOYENV=pro IMBOY_AUTO_MIGRATE=false HTTP_PORT='$OLD_PORT' IMBOY_HTTP_PORT='$OLD_PORT' ./bin/imboy daemon || true
  " || true
  if wait_for_health_status "$OLD_PORT"; then
    OLD_NODE_STOPPED=0
    ok "原节点已恢复且 /healthz 正常，Nginx 未切流"
    return 0
  fi

  echo "✗ 自动恢复原节点失败，服务仍可能不可用: $OLD_DIR (port=$OLD_PORT)" >&2
  return 1
}

# =============================================================================
# Expand 迁移：切流前先补齐新代码必需的可空列
#
# 本轮 00000064 为纯 expand：只给 msg_store 增加可空 sender_did 列，旧节点
# 不会受影响，新节点却会在归档读写 SQL 中直接引用它。不能把它留到切流之后
# 的全量迁移阶段，否则新节点会先接流量再因 schema 不完整报错。
#
# 这里只执行显式列出的、经过发布评审确认的 expand SQL；完整迁移仍在切流后
# 由 db migrate 执行并登记版本。这样不会把未知的 contract 迁移整体提前。
# =============================================================================
probe_boundary_schema() {
  local structure_ready

  structure_ready="$(ssh_capture "docker exec '$DB_CONTAINER' psql -Atq -U '$DB_USER' -d '$DB_NAME' -c \"SELECT CASE WHEN to_regclass('public.schema_migrations') IS NOT NULL AND EXISTS (SELECT 1 FROM public.schema_migrations WHERE version >= 112 AND dirty = false) AND to_regclass('public.msg_c2g_recipient_snapshot') IS NOT NULL AND to_regclass('public.msg_c2g_request_ledger') IS NOT NULL AND $E2EE_ATTESTATION_SCHEMA_PREDICATE AND EXISTS (SELECT 1 FROM information_schema.columns WHERE table_schema='public' AND table_name='msg_store_staging' AND column_name='type') AND EXISTS (SELECT 1 FROM information_schema.columns WHERE table_schema='public' AND table_name='msg_store_staging' AND column_name='to_id') AND EXISTS (SELECT 1 FROM information_schema.columns WHERE table_schema='public' AND table_name='msg_store_staging' AND column_name='conv_seq') AND EXISTS (SELECT 1 FROM information_schema.columns WHERE table_schema='public' AND table_name='msg_store_staging' AND column_name='payload') AND EXISTS (SELECT 1 FROM information_schema.columns WHERE table_schema='public' AND table_name='msg_c2g_timeline' AND column_name='conv_seq') AND EXISTS (SELECT 1 FROM information_schema.columns WHERE table_schema='public' AND table_name='msg_c2g' AND column_name='sender_did') AND EXISTS (SELECT 1 FROM information_schema.columns WHERE table_schema='public' AND table_name='msg_c2g_request_ledger' AND column_name='request_hash' AND is_nullable='NO') AND (SELECT count(*) FROM pg_constraint WHERE conrelid=to_regclass('public.msg_c2g_recipient_snapshot') AND conname IN ('chk_msg_c2g_recipient_snapshot_msg_id','chk_msg_c2g_recipient_snapshot_size','chk_msg_c2g_recipient_snapshot_shape','chk_msg_c2g_recipient_snapshot_positive')) = 4 AND (SELECT count(*) FROM pg_constraint WHERE conrelid=to_regclass('public.msg_c2g_request_ledger') AND conname IN ('chk_msg_c2g_request_ledger_msg_id','chk_msg_c2g_request_ledger_hash')) = 2 THEN 1 ELSE 0 END\"")" || return

  case "$structure_ready" in
    0) printf '%s\n' 0; return 0 ;;
    1) ;;
    *) printf '%s\n' "$structure_ready"; return 0 ;;
  esac

  ssh_capture "docker exec '$DB_CONTAINER' psql -Atq -U '$DB_USER' -d '$DB_NAME' -c \"SELECT CASE WHEN NOT EXISTS (SELECT 1 FROM public.msg_store_staging s WHERE s.type='c2g' AND (s.to_id IS NULL OR s.conv_seq IS NULL OR s.conv_seq < 1 OR jsonb_typeof(s.payload) IS DISTINCT FROM 'object' OR pg_input_is_valid(s.payload ->> 'to', 'bigint') IS NOT TRUE OR (s.payload ->> 'to')::bigint IS DISTINCT FROM s.to_id)) THEN 1 ELSE 0 END\""
}

probe_boundary_dirty() {
  ssh_capture "docker exec '$DB_CONTAINER' psql -Atq -U '$DB_USER' -d '$DB_NAME' -c \"SELECT CASE WHEN to_regclass('public.schema_migrations') IS NOT NULL AND EXISTS (SELECT 1 FROM public.schema_migrations WHERE version IN (108,109,110,111,112) AND dirty = true) THEN 1 ELSE 0 END\""
}

run_expand_migrations() {
  local migration
  local required
  local remote_file
  local boundary_ready
  local boundary_dirty
  local required_status
  local applied
  local -a migrations=()
  local -a filtered_migrations=()

  if [ -z "$EXPAND_MIGRATIONS" ]; then
    # 自动模式：不做 psql 直跑。schema 升级统一由切流后的
    # `make ctl ARGS='db migrate'`（erlang_migrate）按台账自动判断并登记。
    # 此处仅保留 boundary 护航门：release 携带 boundary 文件而台账未登记时，
    # 禁止静默绕过受控 expand 时序，要求显式配置清单。
    [ -n "$DB_CONTAINER" ] || fail "expand 自动模式需要 IMBOY_DEPLOY_DB_CONTAINER"
    [ -n "$DB_NAME" ] || fail "expand 自动模式需要 IMBOY_DEPLOY_DB_NAME"
    [ -n "$DB_USER" ] || fail "expand 自动模式需要 IMBOY_DEPLOY_DB_USER"
    applied="$(ssh_capture "docker exec '$DB_CONTAINER' psql -Atq -U '$DB_USER' -d '$DB_NAME' -c 'SELECT max(version) FROM schema_migrations WHERE dirty = false'")" \
      || fail "无法读取 schema_migrations 台账（表不存在或库不可达）；请显式配置 DEPLOY_EXPAND_MIGRATIONS"
    # erlang_migrate 是单行台账（DELETE 全部 + INSERT 当前版本）：version=N
    # 表示 1..N 全部已应用。判断 boundary 迁移是否已应用须比较版本大小，
    # 不能按多行台账逐行 grep——否则收敛后的台账（如单行 134）永远匹配
    # 不上历史迁移号（如 64），守门必误报。
    for required in \
      00000064_msg_store_sender_did.up.sql \
      00000108_group_attachment_anchor.up.sql \
      00000109_c2g_timeline_generation_boundary.up.sql \
      00000111_c2g_request_recipient_boundary.up.sql \
      00000112_e2ee_group_session_attestation.up.sql; do
      if ssh_exec "test -f '$PROJECT_DIR/priv/migrations/$required'" \
         && [ "${applied:-0}" -lt "$((10#${required%%_*}))" ]; then
        fail "release 包含 boundary 迁移 $required 且台账版本（${applied:-无}）未达：请显式配置 DEPLOY_EXPAND_MIGRATIONS 走受控 expand 护航"
      fi
    done
    log "expand 自动模式：schema 升级由切流后 db migrate 统一执行（erlang_migrate 台账自动判断）"
    return 0
  fi

  if [ -n "$EXPAND_MIGRATIONS" ]; then
    read -r -a migrations <<< "$EXPAND_MIGRATIONS"
  fi
  for required in \
    00000064_msg_store_sender_did.up.sql \
    00000108_group_attachment_anchor.up.sql \
    00000109_c2g_timeline_generation_boundary.up.sql \
    00000111_c2g_request_recipient_boundary.up.sql \
    00000112_e2ee_group_session_attestation.up.sql; do
    if ssh_exec "test -f '$PROJECT_DIR/priv/migrations/$required'"; then
      required_status=0
    else
      required_status=$?
    fi
    case "$required_status" in
      0)
        if [ "$required" = 00000112_e2ee_group_session_attestation.up.sql ]; then
          BOUNDARY_SCHEMA_REQUIRED=1
        fi
        if [ "${#migrations[@]}" -eq 0 ] \
           || ! printf '%s\n' "${migrations[@]}" | grep -qx "$required"; then
          # 清单未含时，台账已登记(dirty=false)视为历史已应用，放行
          if ! printf '%s\n' "${applied:-}" | grep -qx "$((10#${required%%_*}))"; then
            fail "release 包含必需的 expand 迁移但清单未配置且台账未登记: $required"
          fi
        fi
        ;;
      1) ;;
      *) fail "无法探测必需的 expand 迁移文件: $required (status=$required_status)" ;;
    esac
  done
  [ "${#migrations[@]}" -gt 0 ] || fail "IMBOY_DEPLOY_EXPAND_MIGRATIONS 为空"
  [ -n "$DB_CONTAINER" ] || fail "执行 expand 迁移需要 IMBOY_DEPLOY_DB_CONTAINER"
  [ -n "$DB_NAME" ] || fail "执行 expand 迁移需要 IMBOY_DEPLOY_DB_NAME"
  [ -n "$DB_USER" ] || fail "执行 expand 迁移需要 IMBOY_DEPLOY_DB_USER"

  for migration in "${migrations[@]}"; do
    [[ "$migration" =~ ^[a-zA-Z0-9._-]+\.up\.sql$ ]] \
      || fail "expand 迁移文件名非法: $migration"
    remote_file="$PROJECT_DIR/priv/migrations/$migration"
    ssh_exec "test -s '$remote_file'" \
      || fail "远端缺少 expand 迁移文件: $remote_file"
  done

  # 109 首次启用时，新代码与旧 C2G staging 形状不兼容。只有迁移已正式登记、
  # schema/backlog 完整且成功切流 marker 存在，才允许继续滚动发布。
  if printf '%s\n' "${migrations[@]}" \
      | grep -qx '00000109_c2g_timeline_generation_boundary.up.sql'; then
    boundary_dirty="$(probe_boundary_dirty)" \
      || fail "无法探测 C2G boundary migration dirty 状态，拒绝继续"
    case "$boundary_dirty" in 0|1) ;; *) fail "C2G boundary migration dirty 探测返回异常，拒绝继续" ;; esac
    [ "$boundary_dirty" = 0 ] \
      || fail "C2G boundary schema_migrations version 108-112 存在 dirty=true；旧节点保持运行。请人工核查失败 SQL 与事务状态，完成受控恢复后重试；禁止直接 force/清 dirty"
    boundary_ready="$(probe_boundary_schema)" \
      || fail "无法探测 C2G boundary schema，拒绝继续"
    case "$boundary_ready" in 0|1) ;; *) fail "C2G boundary schema 探测返回异常，拒绝继续" ;; esac
    if [ "$boundary_ready" = 1 ] && ssh_exec "test -f '$BOUNDARY_CUTOVER_MARKER'"; then
      for migration in "${migrations[@]}"; do
        case "$migration" in
          00000108_group_attachment_anchor.up.sql|00000109_c2g_timeline_generation_boundary.up.sql|00000111_c2g_request_recipient_boundary.up.sql|00000112_e2ee_group_session_attestation.up.sql) ;;
          *) filtered_migrations+=("$migration") ;;
        esac
      done
      migrations=("${filtered_migrations[@]}")
    else
      BOUNDARY_CUTOVER_PENDING=1
      case "$SKIP_MIGRATE" in
        0) ;;
        *) fail "首次启用或恢复 C2G boundary 不允许 --no-migrate" ;;
      esac
      log "C2G boundary 尚未完成切流：先停止旧节点，消除 legacy staging 混写窗口"
      stop_old_node
      if [ "$boundary_ready" = 1 ]; then
        log "重新校验并修复 cutover 前产生的 legacy C2G backlog"
        ssh_exec "docker exec -i '$DB_CONTAINER' psql -1 -v ON_ERROR_STOP=1 -U '$DB_USER' -d '$DB_NAME' -f - < '$PROJECT_DIR/priv/migrations/00000111_c2g_request_recipient_boundary.up.sql'" \
          || fail "C2G boundary 恢复校验失败"
      else
        BOUNDARY_BOOTSTRAP=1
        START_AUTO_MIGRATE=true
        log "由新节点 boot migration 在接流量前原子应用并登记 108/109/110/111/112"
      fi
      return 0
    fi
  fi

  for migration in "${migrations[@]}"; do
    remote_file="$PROJECT_DIR/priv/migrations/$migration"
    log "执行切流前 expand 迁移: $migration"
    ssh_exec "docker exec -i '$DB_CONTAINER' psql -1 -v ON_ERROR_STOP=1 -U '$DB_USER' -d '$DB_NAME' -f - < '$remote_file'" \
      || fail "expand 迁移失败: $migration"
  done

  # 对本轮关键列做机器验证；不把 psql 输出带回日志，避免泄漏环境细节。
  if printf '%s\n' "${migrations[@]}" | grep -qx '00000064_msg_store_sender_did.up.sql'; then
    ssh_exec "docker exec '$DB_CONTAINER' psql -Atq -U '$DB_USER' -d '$DB_NAME' -c \"SELECT 1 FROM information_schema.columns WHERE table_schema='public' AND table_name='msg_store' AND column_name='sender_did'\" | grep -qx 1" \
      || fail "schema 验证失败：public.msg_store.sender_did 不存在"
    ok "schema 已确认：public.msg_store.sender_did"
  fi
  if printf '%s\n' "${migrations[@]}" | grep -qx '00000108_group_attachment_anchor.up.sql'; then
    ssh_exec "docker exec '$DB_CONTAINER' psql -Atq -U '$DB_USER' -d '$DB_NAME' -c \"SELECT count(*) FROM information_schema.columns WHERE table_schema='public' AND table_name='attachment' AND column_name IN ('anchor_msg_id','anchor_conv_seq','group_file_id')\" | grep -qx 3" \
      || fail "schema 验证失败：attachment group anchor 列不完整"
    ok "schema 已确认：attachment group anchor"
  fi
  if printf '%s\n' "${migrations[@]}" | grep -qx '00000109_c2g_timeline_generation_boundary.up.sql'; then
    ssh_exec "docker exec '$DB_CONTAINER' psql -Atq -U '$DB_USER' -d '$DB_NAME' -c \"SELECT CASE WHEN to_regclass('public.msg_c2g_recipient_snapshot') IS NOT NULL AND EXISTS (SELECT 1 FROM information_schema.columns WHERE table_schema='public' AND table_name='msg_c2g_timeline' AND column_name='conv_seq') AND EXISTS (SELECT 1 FROM information_schema.columns WHERE table_schema='public' AND table_name='msg_c2g' AND column_name='sender_did') THEN 1 ELSE 0 END\" | grep -qx 1" \
      || fail "schema 验证失败：C2G boundary schema 不完整"
    ok "schema 已确认：C2G request ledger + recipient snapshot + timeline conv_seq + sender_did"
  fi
  if printf '%s\n' "${migrations[@]}" | grep -qx '00000112_e2ee_group_session_attestation.up.sql'; then
    ssh_exec "docker exec '$DB_CONTAINER' psql -Atq -U '$DB_USER' -d '$DB_NAME' -c \"SELECT CASE WHEN $E2EE_ATTESTATION_SCHEMA_PREDICATE THEN 1 ELSE 0 END\" | grep -qx 1" \
      || fail "schema 验证失败：E2EE group session attestation schema 不完整"
    ok "schema 已确认：E2EE group session attestation + member generation"
  fi
}

# =============================================================================
# 0️⃣ 回滚模式 / Rollback mode（C-52）
#
# 蓝绿部署的价值一半在"能快速切回去"，而此前脚本**没有任何回滚入口** ——
# 出事时只能手工 sed nginx 配置，正是最不该手工操作的时刻。
#
# 回滚只做一件事：把 nginx 切回另一色，并在切之前**确认那一色真的健康**。
# ⚠️ 它**不回滚数据库迁移**。迁移已应用的 schema 变更无法靠切流撤销，
#   这也是 expand/contract 纪律不可省的原因（见 7️⃣ 的说明）。
#   只有 --no-migrate 的外层编排会保留旧节点；正常完整迁移为避免既有 WebSocket
#   继续运行旧代码，会在迁移前停止旧节点，此时回滚需先人工确认 schema 兼容性。
# =============================================================================
if [ "$ROLLBACK" -eq 1 ]; then
  log "回滚模式 / Rollback mode"
  if ! CUR="$(probe_nginx_color)"; then
    fail "Nginx 当前 upstream 不是唯一蓝/绿色，拒绝猜测回滚方向"
  fi
  case "$CUR" in
    blue)  RB_PORT="$GREEN_PORT"; RB_COLOR=green; CUR_PORT="$BLUE_PORT" ;;
    green) RB_PORT="$BLUE_PORT";  RB_COLOR=blue;  CUR_PORT="$GREEN_PORT" ;;
    *)     fail "Nginx upstream 状态未知，无法回滚 / Unknown upstream" ;;
  esac

  # 切之前必须确认目标色真的能服务 —— 切到一个死节点上比不回滚更糟。
  # 这里不校验版本：回滚的目标本来就是**旧**版本。
  ssh_exec "curl -fsS --max-time 3 \"http://127.0.0.1:$RB_PORT/healthz\" | grep -q '\"status\":\"ok\"'" \
    || fail "目标色 $RB_COLOR (port=$RB_PORT) 不健康或未运行，拒绝回滚 / rollback target not healthy。
  若当初部署时设了 IMBOY_DEPLOY_STOP_OLD=true，旧节点已被停止，需先手工启动它。"

  ssh_exec "
    : imboy-nginx-rollback
    set -eu
    HAD_ADMIN=0
    HAD_CS=0
    restore_configs() {
      cp '$NGINX_CONF'.bak '$NGINX_CONF'
      [ "\$HAD_ADMIN" -eq 0 ] || cp '$PRODADM_CONF'.bak '$PRODADM_CONF'
      [ "\$HAD_CS" -eq 0 ] || cp '$CS_NGINX_CONF'.bak '$CS_NGINX_CONF'
    }
    cp '$NGINX_CONF' '$NGINX_CONF'.bak
    if [ -f '$PRODADM_CONF' ]; then cp '$PRODADM_CONF' '$PRODADM_CONF'.bak; HAD_ADMIN=1; fi
    if [ -n '$CS_NGINX_CONF' ] && [ -f '$CS_NGINX_CONF' ]; then cp '$CS_NGINX_CONF' '$CS_NGINX_CONF'.bak; HAD_CS=1; fi
    sed -i 's|server 127.0.0.1:$CUR_PORT;|server 127.0.0.1:$RB_PORT;|g' '$NGINX_CONF'
    ROLLBACK_BLUE_AFTER=\$(awk '/^[[:space:]]*server[[:space:]]+127\\.0\\.0\\.1:$BLUE_PORT;/{n++} END{print n+0}' '$NGINX_CONF')
    ROLLBACK_GREEN_AFTER=\$(awk '/^[[:space:]]*server[[:space:]]+127\\.0\\.0\\.1:$GREEN_PORT;/{n++} END{print n+0}' '$NGINX_CONF')
    case '$RB_COLOR' in
      blue)  [ \"\$ROLLBACK_BLUE_AFTER\" -eq 1 ] && [ \"\$ROLLBACK_GREEN_AFTER\" -eq 0 ] ;;
      green) [ \"\$ROLLBACK_GREEN_AFTER\" -eq 1 ] && [ \"\$ROLLBACK_BLUE_AFTER\" -eq 0 ] ;;
    esac || { restore_configs; echo 'Nginx upstream 替换未生效，已恢复配置' >&2; exit 1; }
    for CONF in '$PRODADM_CONF' '$CS_NGINX_CONF'; do
      [ -n \"\$CONF\" ] && [ -f \"\$CONF\" ] || continue
      grep -q 'http://127.0.0.1:$CUR_PORT;' \"\$CONF\" || continue
      sed -i 's|http://127.0.0.1:$CUR_PORT;|http://127.0.0.1:$RB_PORT;|g' \"\$CONF\"
      grep -q 'http://127.0.0.1:$RB_PORT;' \"\$CONF\" \
        && ! grep -q 'http://127.0.0.1:$CUR_PORT;' \"\$CONF\" \
        || { restore_configs; echo \"vhost 回滚端口替换失败: \$CONF\" >&2; exit 1; }
    done
    nginx -t || { restore_configs; exit 1; }
    nginx -s reload || {
      restore_configs
      nginx -t && nginx -s reload || true
      exit 1
    }
  " || fail "Nginx 回滚失败 / Nginx rollback failed"

  ok "已回滚至 $RB_COLOR (port=$RB_PORT) / Rolled back。⚠️ 数据库迁移未回滚，请人工确认 schema 与该版本兼容"
  exit 0
fi

# =============================================================================
# 1️⃣ 检测当前运行色 / Detect active color
# =============================================================================
log "检测 Nginx 活动 upstream... / Detecting active upstream..."
NGINX_PORT="$(probe_nginx_port)" \
  || fail "无法读取 Nginx upstream，拒绝猜测活动节点"
case "$NGINX_PORT" in
  "$BLUE_PORT") CURRENT_COLOR=blue; OLD_PORT=$BLUE_PORT ;;
  "$GREEN_PORT") CURRENT_COLOR=green; OLD_PORT=$GREEN_PORT ;;
  none) fail "首次部署缺少可验证的蓝绿 upstream；请先在 Nginx 预置蓝或绿端口" ;;
  conflict) fail "Nginx 配置包含多个 upstream 端口，拒绝猜测活动节点" ;;
  *)
    [[ "$NGINX_PORT" =~ ^[0-9]+$ ]] \
      || fail "Nginx upstream 端口非法: $NGINX_PORT"
    [ -n "$LEGACY_PORT" ] && [ "$NGINX_PORT" = "$LEGACY_PORT" ] \
      || fail "Nginx 指向配置外端口 $NGINX_PORT；请在 .env.deploy 设置匹配的 DEPLOY_LEGACY_PORT"
    CURRENT_COLOR=legacy
    OLD_PORT=$LEGACY_PORT
    ;;
esac

if ! ssh_exec "ss -tlnH \"sport = :$OLD_PORT\" 2>/dev/null | grep -q ."; then
  if OLD_DIR="$(find_stopped_release_for_port "$OLD_PORT")"; then
    safe_release_dir "$OLD_DIR" || fail "历史 release 目录不安全: $OLD_DIR"
    OLD_NODE_STOPPED=1
    if [ "$CURRENT_COLOR" = legacy ]; then
      log "检测到 legacy 已停机，保留 $OLD_DIR 作为恢复点并继续割接"
    else
      log "检测到活动槽停机，先恢复 Nginx 当前指向的 $CURRENT_COLOR 节点"
      recover_old_node_before_cutover \
        || fail "现有 $CURRENT_COLOR 节点恢复失败，拒绝继续发布"
      FAIL_RECOVERY_ATTEMPTED=0
    fi
  else
    [ "$CURRENT_COLOR" != legacy ] \
      || fail "Nginx 指向的 legacy 端口 $OLD_PORT 未监听，且无法定位可信旧 release 恢复点"
    CURRENT_COLOR=none
    OLD_PORT=""
  fi
fi

ACTIVE_DIR=""
if [ "$CURRENT_COLOR" != none ]; then
  if [ "$OLD_NODE_STOPPED" -eq 1 ] && [ -n "$OLD_DIR" ]; then
    ACTIVE_DIR="$OLD_DIR"
  else
    ACTIVE_DIR="$(find_running_release_for_port "$OLD_PORT")" \
      || fail "无法定位 Nginx 活动端口 $OLD_PORT 的 release 根目录"
  fi
  safe_release_dir "$ACTIVE_DIR" \
    || fail "活动 release 目录不符合安全模板: $ACTIVE_DIR"
fi

if [ "$CURRENT_COLOR" = "none" ] && [ "$SKIP_MIGRATE" -eq 1 ]; then
  fail "首次安装不能使用 --no-migrate；空库必须完成 bootstrap 迁移"
fi

# 选择对立色；首次部署（none）默认蓝
# Pick opposite color; first deploy defaults to blue
case "$CURRENT_COLOR" in
  blue)  TARGET_COLOR=green; APP_PORT=$GREEN_PORT; OLD_PORT=$BLUE_PORT; START_AUTO_MIGRATE=false ;;
  green) TARGET_COLOR=blue;  APP_PORT=$BLUE_PORT;  OLD_PORT=$GREEN_PORT; START_AUTO_MIGRATE=false ;;
  legacy) TARGET_COLOR=blue; APP_PORT=$BLUE_PORT; START_AUTO_MIGRATE=false ;;
  *)
    if [ "$NGINX_PORT" = "$GREEN_PORT" ]; then TARGET_COLOR=green; APP_PORT=$GREEN_PORT
    else TARGET_COLOR=blue; APP_PORT=$BLUE_PORT
    fi
    OLD_PORT=""
    START_AUTO_MIGRATE=true
    ;;
esac

ok "当前: $CURRENT_COLOR → 目标: $TARGET_COLOR (port=$APP_PORT) / Current: $CURRENT_COLOR → Target: $TARGET_COLOR"
cleanup_inactive_target

# =============================================================================
# 2️⃣ 安全确认目标目录 / Confirm target dir is safe to overwrite
# =============================================================================
if ssh_exec "[ -d '$RELEASE_DIR' ]"; then
  if [ "$ACTIVE_DIR" = "$RELEASE_DIR" ]; then
    ssh_exec "test \"\$(cat '$RELEASE_DIR/etc/source-head' 2>/dev/null)\" = '$SOURCE_HEAD'" \
      || fail "同版本活动 release 的 source HEAD 与本次候选不一致，拒绝幂等误判"
    wait_for_health "$OLD_PORT" "$VSN" \
      || fail "同版本 release 正在活动端口运行但健康或版本不符，拒绝覆盖"
    ok "目标 release 已在活动端口健康运行，重复部署直接成功 (port=$OLD_PORT, vsn=$VSN)"
    exit 0
  fi

  log "清理同版本上次失败的非活动 release: $RELEASE_DIR"
  ssh_exec "
    if [ -x '$RELEASE_DIR/bin/imboy' ]; then
      command -v timeout >/dev/null 2>&1 || exit 2
      timeout 10s '$RELEASE_DIR/bin/imboy' stop >/dev/null 2>&1 || true
    fi
  "
  stop_release_processes "$RELEASE_DIR" \
    || fail "上次失败 release 仍有残留进程，拒绝删除其运行目录"
  ssh_exec "rm -rf -- '$RELEASE_DIR'"
  ok "失败残留已安全清理，可重复发布"
fi

# =============================================================================
# 3️⃣ 拉代码 + 编译 release
# RELX_DEV_MODE=false 确保产物不含符号链接，RELX_INCLUDE_ERTS=true 内嵌运行时
# RELX_DEV_MODE=false: no symlinks in tarball; RELX_INCLUDE_ERTS=true: embed ERTS
# =============================================================================
if [ "$LOCAL_MODE" -eq 1 ]; then
  log "[--local] 同步本地源码到远端 $PROJECT_DIR ... / Syncing local source to remote..."
  # 内容比较避免本地生成器 touch 造成重复上传；改变的源码使用远端当前时间，
  # 避免旧时间戳（恢复文件/切换分支）让 make 漏编。
  rsync -az --checksum --no-times --delete \
    --exclude='.git' \
    --exclude='/ebin/' \
    --exclude='/test/' \
    --exclude='/.Codex/' \
    --exclude='/.codex/' \
    --exclude='/.agents/' \
    --exclude='_build/' \
    --exclude='.erlang.mk/' \
    --exclude='_rel/' \
    --exclude='deps/' \
    --exclude='log/' \
    --exclude='*.beam' \
    --exclude='*.d' \
    --exclude='config/sys.pro.config' \
    --exclude='config/sys.runtime.config' \
    --exclude='config/sys.dev.config' \
    --exclude='config/sys.local.config' \
    --exclude='.env.deploy*' \
    --exclude='docker/' \
    -e "ssh -p $SERVER_PORT -o ControlPath=$SSH_CTRL -o StrictHostKeyChecking=accept-new" \
    "$LOCAL_SRC_DIR/" \
    "$SERVER_USER@$SERVER_HOST:$PROJECT_DIR/"
  ok "本地源码已同步 / Local source synced"
  # 迁移走独立 staging 目录：DB 升级从 staging 读本地工作区的迁移文件（与源码
  # 同一快照），迁移成功后清理，失败保留供比对排查。目录放 $PROJECT_DIR 之外
  # （/tmp），避免下次部署 rsync --delete 同步源码时被误删、或被 make clean 碰到。
  if [ "$SKIP_MIGRATE" -eq 0 ]; then
    MIGRATE_STAGING="/tmp/imboy-migrate-staging-${NODE_NAME}"
    rsync -az --delete \
      -e "ssh -p $SERVER_PORT -o ControlPath=$SSH_CTRL -o StrictHostKeyChecking=accept-new" \
      "$LOCAL_SRC_DIR/priv/migrations/" \
      "$SERVER_USER@$SERVER_HOST:$MIGRATE_STAGING/" \
      || fail "迁移文件同步到 staging 失败 / Failed to sync migrations to staging"
    ok "迁移文件已同步到 staging / Migrations synced: $MIGRATE_STAGING"
  fi
else
  log "拉取代码... / Pulling code from git..."
  ssh_exec "
    set -e
    cd '$PROJECT_DIR'
    git fetch origin
    git checkout '$BRANCH'
    git reset --hard origin/'$BRANCH'
  "
  ok "代码已拉取 / Code pulled"
  REMOTE_HEAD="$(ssh_capture "cd '$PROJECT_DIR' && git rev-parse HEAD")" \
    || fail "无法读取远端 Git HEAD"
  [ "$REMOTE_HEAD" = "$SOURCE_HEAD" ] \
    || fail "远端 $BRANCH HEAD ($REMOTE_HEAD) 与本地候选 ($SOURCE_HEAD) 不一致：请先完成受控 push 或使用 -l"
fi

# 远端 VERSION 是 erlang.mk 生成 ebin/imboy.app vsn 的唯一来源，
# /healthz 自报该 vsn。远端落后（如本地 bump 未 push）时产物名对得上但
# 健康检查版本核对必败，这里提前拦截。
REMOTE_VSN="$(ssh_capture "head -n1 '$PROJECT_DIR/VERSION' 2>/dev/null | tr -d '[:space:]'")" \
  || fail "无法读取远端 VERSION"
[ "$REMOTE_VSN" = "$VSN" ] \
  || fail "远端仓库 VERSION ($REMOTE_VSN) 与目标版本 ($VSN) 不一致：本地提交是否已 push？(-l 模式请检查 rsync 排除项)"

log "编译 release... / Building release..."
ssh_exec "
  set -e
  cd '$PROJECT_DIR'
  # 销售版必须显式开启严格 E2EE、频道和付费频道入口；sys.pro.config
  # 是部署环境提供的忽略文件，校验器只输出策略布尔值，不输出任何密钥。
  test -f config/sys.pro.config || {
    echo '缺少 config/sys.pro.config：拒绝生成销售版 release' >&2
    exit 1
  }
  IMBOY_SALES_RELEASE=$SALES_RELEASE \
    escript scripts/validate_sales_release_config.escript config/sys.pro.config
  # relx 只认 config 里的 release 版本行（RELX_REL_VSN 实测不生效，版本双源），
  # 构建前把版本行强制对齐到 .env.deploy 指定的 VSN，避免产物名与解包名漂移。
  # git reset --hard 每次会还原此改动，幂等重写无害。
  sed -i 's/^{release, {imboy, \"[^\"]*\"}/{release, {imboy, \"$VSN\"}/' relx.config relxpro.config
  # 只在缓存不完整、版本/编译模式变化时清理应用，保留依赖与正常增量编译。
  bash scripts/lib/prepare_release_build.sh '$VSN'
  IMBOYENV=pro \
    RELX_DEV_MODE=false \
    RELX_INCLUDE_ERTS=true \
    make rel
  # 用服务器实际 sys.pro.config（含生产凭证）覆盖编译产物里的占位符 sys.config
  # Overlay generated sys.config with server's authoritative pro config (real credentials)
  REL_SYS_CONFIG='$PROJECT_DIR/_rel/imboy/releases/$VSN/sys.config'
  if [ -f '$PROJECT_DIR/config/sys.pro.config' ] && [ -f \"\$REL_SYS_CONFIG\" ]; then
    cp '$PROJECT_DIR/config/sys.pro.config' \"\$REL_SYS_CONFIG\"
    sed -i \"s/{http_port,[ ]*[0-9]\\+}/{http_port, $APP_PORT}/\" \"\$REL_SYS_CONFIG\"
  fi
"
ok "release 编译完成 / Release built"

# =============================================================================
# 4️⃣ 解包 + 生成 vm.args（节点身份 + BEAM 调优参数）
# Extract tarball + write vm.args (node identity + BEAM tuning flags)
# =============================================================================
log "解包 release + 写入 vm.args... / Extracting release and writing vm.args..."
# <<'VMARGS' 防止远端 shell 对内容二次展开；本地变量在外层双引号中已展开
# <<'VMARGS' prevents remote re-expansion; local vars are expanded by outer double-quotes
ssh_exec "
  set -e
  mkdir -p '$RELEASE_DIR'
  cd '$RELEASE_DIR' && tar -xzf '$RELEASE_TARBALL'
  REL_VSN_DIR=\$(find '$RELEASE_DIR/releases' -maxdepth 1 -mindepth 1 -type d | sort -V | tail -1)
  # 解包后覆盖 http_port（tarball 内含旧值，必须在这里改）
  sed -i \"s/{http_port,[ ]*[0-9]\\+}/{http_port, $APP_PORT}/\" \"\$REL_VSN_DIR/sys.config\"
  # 蓝绿 release 永久关闭 boot-time migrate；不依赖单次 daemon 命令的环境继承。
  if grep -q '{auto_migrate,' \"\$REL_VSN_DIR/sys.config\"; then
    sed -i 's/{auto_migrate,[ ]*true}/{auto_migrate, false}/' \"\$REL_VSN_DIR/sys.config\"
  else
    sed -i '/{imboy, \\[/a\\        {auto_migrate, false},' \"\$REL_VSN_DIR/sys.config\"
  fi
  grep -q '{auto_migrate,[ ]*false}' \"\$REL_VSN_DIR/sys.config\"
  install -d -m 0755 '$RELEASE_DIR/etc'
  printf '%s\n' '$SOURCE_HEAD' > '$RELEASE_DIR/etc/source-head'
  cat > \"\$REL_VSN_DIR/vm.args\" <<'VMARGS'
-name ${NODE_NAME}@${NODE_HOST}
-setcookie ${COOKIE}
-heart
-kernel inet_dist_use_interface '{127,0,0,1}'
-env ERL_EPMD_ADDRESS 127.0.0.1
+K true
+A 256
+S 4
+MSe true
+P 1048576
+Q 1048576
+sbwt none
+sbwtdcpu none
+sbwtdio none
+swt very_low
+stbt db
+zdbbl 81920
VMARGS
"
if [ -n "$PLUGIN_TRUSTED_PUBLIC_KEY_FILE" ]; then
  ssh_exec "install -d -m 0755 '$RELEASE_DIR/etc'"
  ssh_upload "$PLUGIN_TRUSTED_PUBLIC_KEY_FILE" "$PLUGIN_TRUSTED_PUBLIC_KEY_REMOTE"
  ssh_exec "[ \"\$(wc -c < '$PLUGIN_TRUSTED_PUBLIC_KEY_REMOTE')\" -eq 32 ] && chmod 0644 '$PLUGIN_TRUSTED_PUBLIC_KEY_REMOTE'"
  ok "插件签名可信公钥已安装到新 release"
fi
# 启动前 fail-fast：enterprise 档位 + 未安装公钥 = 必然 boot 崩溃循环
# （invalid_plugin_signature_config + heart 复活，且 40s 健康门超时后残留
# 阻塞后续发布）。宁可在此中止，也不留下一个注定起不来的槽位。
ssh_exec "
  SYS=\$(find '$RELEASE_DIR/releases' -maxdepth 2 -name sys.config | head -1)
  if [ -n \"\$SYS\" ] \\
     && grep -qE '\\{product_profile,' \"\$SYS\" \\
     && ! grep -qE '\\{product_profile,[ ]*community\\}' \"\$SYS\" \\
     && [ ! -f '$PLUGIN_TRUSTED_PUBLIC_KEY_REMOTE' ]; then
    echo 'sys.config 为非 community 档位但未安装插件可信公钥，启动必崩' >&2
    exit 1
  fi
" || fail "非 community 档位缺少插件可信公钥，拒绝启动新节点；请配置 DEPLOY_PLUGIN_TRUSTED_PUBLIC_KEY_FILE"
ok "release 已解包，vm.args 已写入 / Release extracted, vm.args written"

# =============================================================================
# 4.5️⃣ 启动前 Expand schema / Apply additive schema before node startup
#
# 新代码可能在启动或健康检查阶段就引用新增字段，因此 expand 必须早于新节点启动。
# 完整迁移仍由切流后的显式 db migrate 执行。
# =============================================================================
if [ "$CURRENT_COLOR" = "none" ]; then
  log "首次安装由 application boot 执行完整迁移，跳过针对既有 schema 的 expand"
else
  run_expand_migrations
fi

# =============================================================================
# 5️⃣ 启动新节点 + 轮询确认就绪 / Start new node + poll for readiness
# =============================================================================
# legacy → 蓝绿首次割接必须先停旧 writer；失败时 fail() 会恢复旧节点。
if [ "$CURRENT_COLOR" = legacy ]; then
  [ "$TSID_BOOTSTRAP_LEGACY_ACK" = "I-CONFIRM-OLD-WRITER-STOPPED" ] \
    || fail "legacy 首次割接要求在 .env.deploy 配置 DEPLOY_TSID_BOOTSTRAP_LEGACY_ACK"
  stop_old_node
fi

TSID_BOOTSTRAP_ENV=""
if [ -n "$TSID_STATE_DIR" ]; then
  case "$TARGET_COLOR" in
    blue) TSID_NODE_ID=$TSID_BLUE_NODE_ID ;;
    green) TSID_NODE_ID=$TSID_GREEN_NODE_ID ;;
  esac
  if [ "$TSID_BOOTSTRAP_FLOOR_UNIX_MS" = now ]; then
    TSID_BOOTSTRAP_FLOOR_UNIX_MS="$(ssh_capture "date +%s%3N")" \
      || fail "无法从服务器读取 TSID manual floor 时钟"
    [[ "$TSID_BOOTSTRAP_FLOOR_UNIX_MS" =~ ^[0-9]{13}$ ]] \
      || fail "服务器返回的 TSID manual floor 时钟非法"
  fi
  TSID_BOOTSTRAP_ENV="IMBOY_TSID_STATE_DIR='$TSID_STATE_DIR' IMBOY_TSID_NODE_ID='$TSID_NODE_ID'"
  [ -z "$TSID_BOOTSTRAP_MODE" ] \
    || TSID_BOOTSTRAP_ENV="$TSID_BOOTSTRAP_ENV IMBOY_TSID_BOOTSTRAP_MODE='$TSID_BOOTSTRAP_MODE'"
  [ -z "$TSID_BOOTSTRAP_FLOOR_UNIX_MS" ] \
    || TSID_BOOTSTRAP_ENV="$TSID_BOOTSTRAP_ENV IMBOY_TSID_BOOTSTRAP_FLOOR_UNIX_MS='$TSID_BOOTSTRAP_FLOOR_UNIX_MS'"
  [ -z "$TSID_BOOTSTRAP_LEGACY_ACK" ] \
    || TSID_BOOTSTRAP_ENV="$TSID_BOOTSTRAP_ENV IMBOY_TSID_BOOTSTRAP_LEGACY_ACK='$TSID_BOOTSTRAP_LEGACY_ACK'"
  [ -z "$TSID_BOOTSTRAP_REBIND_ACK" ] \
    || TSID_BOOTSTRAP_ENV="$TSID_BOOTSTRAP_ENV IMBOY_TSID_BOOTSTRAP_REBIND_ACK='$TSID_BOOTSTRAP_REBIND_ACK'"
fi
log "启动新节点 (port=$APP_PORT)... / Starting new node..."
CANDIDATE_STARTED=1
ssh_exec "cd '$RELEASE_DIR' && IMBOYENV=pro IMBOY_AUTO_MIGRATE='$START_AUTO_MIGRATE' HTTP_PORT='$APP_PORT' IMBOY_HTTP_PORT='$APP_PORT' IMBOY_E2EE_MODE='$E2EE_MODE' IMBOY_PLUGIN_TRUSTED_PUBLIC_KEY_FILES='$PLUGIN_TRUSTED_PUBLIC_KEY_REMOTE' $TSID_BOOTSTRAP_ENV ./bin/imboy daemon"

# 轮询取代原来的固定 sleep 5，在慢服务器上不会误报失败
# Polling replaces fixed sleep 5; won't false-fail on slow servers
# C-51：探 /healthz + 校验版本，而不是只看端口有没有被监听。
# 失败信息刻意点名"可能是残留进程"——这是实际最常见的原因，
# 直接写出来能省掉一轮排查。
wait_for_health "$APP_PORT" "$VSN" \
  || fail "新节点 40s 内未就绪或版本不符 / Node not ready or version mismatch within 40s (port=$APP_PORT, expect=$VSN)。
  常见原因：目标色端口上有上一次部署的残留进程。
  排查：ssh $SERVER_HOST \"ss -tlnp 'sport = :$APP_PORT'\" 并确认进程的 -root 目录"
ok "新节点已就绪且版本匹配 (port=$APP_PORT, vsn=$VSN) / New node ready, version verified"

if [ "$BOUNDARY_SCHEMA_REQUIRED" -eq 1 ] && [ -n "$OLD_PORT" ]; then
  [ "$(probe_boundary_schema)" = 1 ] \
    || fail "C2G boundary 最终 schema/backlog 校验失败，拒绝切流"
fi

# =============================================================================
# 6️⃣ 切换 Nginx upstream / Switch Nginx upstream
# 首次部署必须预置蓝/绿 upstream。无法证明切流完成时 fail-closed，禁止成功后再要求手工补配置。
# =============================================================================
if [ -n "$OLD_PORT" ]; then
  log "切换 Nginx: $OLD_PORT → $APP_PORT..."
  ssh_exec "
    : imboy-nginx-cutover
    set -eu
    HAD_ADMIN=0
    HAD_CS=0
    restore_configs() {
      cp '$NGINX_CONF'.bak '$NGINX_CONF'
      [ "\$HAD_ADMIN" -eq 0 ] || cp '$PRODADM_CONF'.bak '$PRODADM_CONF'
      [ "\$HAD_CS" -eq 0 ] || cp '$CS_NGINX_CONF'.bak '$CS_NGINX_CONF'
    }
    cp '$NGINX_CONF' '$NGINX_CONF'.bak
    if [ -f '$PRODADM_CONF' ]; then cp '$PRODADM_CONF' '$PRODADM_CONF'.bak; HAD_ADMIN=1; fi
    if [ -n '$CS_NGINX_CONF' ] && [ -f '$CS_NGINX_CONF' ]; then cp '$CS_NGINX_CONF' '$CS_NGINX_CONF'.bak; HAD_CS=1; fi
    sed -i 's|server 127.0.0.1:$OLD_PORT;|server 127.0.0.1:$APP_PORT;|g' '$NGINX_CONF'
    grep -q 'server 127.0.0.1:$APP_PORT;' '$NGINX_CONF' \
      && ! grep -q 'server 127.0.0.1:$OLD_PORT;' '$NGINX_CONF' \
      || { restore_configs; echo 'Nginx upstream 替换失败，已回滚 / replacement failed, rolled back' >&2; exit 1; }
    for CONF in '$PRODADM_CONF' '$CS_NGINX_CONF'; do
      [ -n \"\$CONF\" ] && [ -f \"\$CONF\" ] || continue
      grep -q 'http://127.0.0.1:$OLD_PORT;' \"\$CONF\" || continue
      sed -i 's|http://127.0.0.1:$OLD_PORT;|http://127.0.0.1:$APP_PORT;|g' \"\$CONF\"
      grep -q 'http://127.0.0.1:$APP_PORT;' \"\$CONF\" \
        && ! grep -q 'http://127.0.0.1:$OLD_PORT;' \"\$CONF\" \
        || { restore_configs; echo \"vhost upstream 替换失败: \$CONF\" >&2; exit 1; }
    done
    nginx -t || { restore_configs; exit 1; }
    nginx -s reload || {
      restore_configs
      nginx -t && nginx -s reload || true
      exit 1
    }
  "
  TRAFFIC_SWITCHED=1
  ok "Nginx 已切换至 $TARGET_COLOR / Nginx switched to $TARGET_COLOR"
elif [ "$CURRENT_COLOR" = none ]; then
  [ "$(probe_nginx_port)" = "$APP_PORT" ] \
    || fail "首次安装的 Nginx upstream 未指向目标端口 $APP_PORT"
  TRAFFIC_SWITCHED=1
  ok "首次安装沿用已预置的 $TARGET_COLOR upstream"
else
  fail "内部状态错误：缺少旧端口却进入切流阶段"
fi

# Nginx reload 只切换新连接；既有 WebSocket 仍停留在旧节点。完整迁移前必须
# 显式停止旧节点并确认端口关闭，不能把 reload 误当成 connection drain。
if [ "$SKIP_MIGRATE" -eq 0 ]; then
  stop_old_node
fi

# =============================================================================
# 7️⃣ 数据库迁移 / Run remaining DB migrations（切流并 drain 之后）
#
# 为什么完整迁移在 drain 之后：Nginx reload 只影响新连接，旧 WebSocket 会继续
# 承载业务；必须停止旧节点并确认端口关闭，才能执行可能含 contract 的完整迁移。
#
# 切流前的 run_expand_migrations 只执行显式批准的、旧代码兼容的增量 DDL，
# 解决新代码在切流瞬间依赖新列的问题；其余迁移仍须遵循 expand/contract 纪律：
#     - 新增列/表（expand）必须先于依赖它的新代码发布，或新代码能容忍其缺失
#     - 删除列/表（contract）只能在旧代码彻底下线后的**下一次**发布里做
#   本次调整解决的是"expand 迟于切流"与"contract 撞上旧节点"两个相反时序，
#   不是免除迁移评审。
#
# 迁移失败时**不自动重启旧节点**：此刻 schema 可能已部分应用，旧版本兼容性未知。
# 除首次安装和 C2G boundary bootstrap 外，新节点固定传
# IMBOY_AUTO_MIGRATE=false；否则 imboy_app:start/2 会在健康检查前先跑完整迁移，
# 使本节的切流后时序沦为重复执行而非真实门禁。
# =============================================================================
if [ "$SKIP_MIGRATE" -eq 1 ]; then
  log "跳过数据库迁移（--no-migrate）/ Skipping DB migrations"
elif [ "$CURRENT_COLOR" = "none" ]; then
  ok "首次安装已在健康检查前完成 bootstrap 迁移 / Bootstrap migrations completed during startup"
elif [ "$BOUNDARY_BOOTSTRAP" -eq 1 ]; then
  ok "C2G boundary 已在新节点接流量前完成并登记 / Boundary migrations completed before traffic"
else
  log "执行数据库迁移... / Running DB migrations..."
  # CTL_NODE 必须显式指定为本次刚启动的节点名，Makefile 默认值 imboy@127.0.0.1
  # 与 vm.args 里实际写入的 ${NODE_NAME}@${NODE_HOST} 不一致，不传会报
  # "cannot reach 'imboy@127.0.0.1'" 并中止部署（实测复现）。同理 cookie
  # 也必须显式传 IMBOY_CTL_COOKIE，否则 imboy_ctl 默认 cookie 与节点不一致，
  # 当 IMBOY_DEPLOY_COOKIE（如 .env.deploy 的旧值）不是当前值时连不上。
  # --local 模式迁移从 staging 读（与本次源码快照一致）；否则读 release 内 priv/migrations
  MIGRATE_CMD="db migrate"
  [ -n "$MIGRATE_STAGING" ] && MIGRATE_CMD="db migrate --dir $MIGRATE_STAGING"
  ssh_exec "cd '$PROJECT_DIR' && CTL_NODE='${NODE_NAME}@${NODE_HOST}' IMBOY_CTL_COOKIE='${COOKIE}' make ctl ARGS='$MIGRATE_CMD'" \
    || fail "数据库迁移失败 / DB migration failed。流量已切到新节点且 schema 可能部分应用。
  旧节点已停止；请先核对 schema_migrations_history 与兼容性，再决定是否人工恢复旧节点。
  $([ -n "$MIGRATE_STAGING" ] && echo "[--local] 迁移 staging 已保留供比对: $MIGRATE_STAGING")"
  # 迁移成功才清理 staging（-l 模式；失败时上面的 fail 已保留现场）
  if [ -n "$MIGRATE_STAGING" ]; then
    ssh_exec "rm -rf -- '$MIGRATE_STAGING'" \
      || log "警告：迁移 staging 清理失败（不影响部署，可手动 rm）: $MIGRATE_STAGING"
    ok "迁移 staging 已清理 / Migration staging cleaned"
  fi
  ok "数据库迁移完成 / DB migrations applied"
fi

if [ "$BOUNDARY_CUTOVER_PENDING" -eq 1 ]; then
  [ "$(probe_boundary_schema)" = 1 ] \
    || fail "C2G boundary 最终 schema/backlog 校验失败，不写 cutover marker"
  ssh_exec "umask 077 && : > '$BOUNDARY_CUTOVER_MARKER'" \
    || fail "C2G boundary cutover marker 写入失败"
  ok "C2G boundary cutover 已完成并持久标记"
fi

# =============================================================================
# 完成 / Done
# =============================================================================
echo
ok "蓝绿部署完成 / Blue-green deploy complete"
printf -- "----------------------------------------------\n"
printf "%-22s %s\n" "版本 / Version:"  "$VSN"
printf "%-22s %s\n" "节点 / Node:"     "${NODE_NAME}@${NODE_HOST}"
printf "%-22s %s\n" "环境 / Slot:"     "$TARGET_COLOR"
printf "%-22s %s\n" "端口 / Port:"     "$APP_PORT"
printf "%-22s %s\n" "目录 / Dir:"      "$RELEASE_DIR"
printf -- "----------------------------------------------\n"
