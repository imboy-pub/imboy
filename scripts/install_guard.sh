#!/usr/bin/env bash
# Read-only delivery guards. Never source .env or repair existing customer state.
set -euo pipefail
GUARD_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
GUARD_ENV="$GUARD_ROOT/deploy/.env"
GUARD_TEMPLATE="$GUARD_ROOT/deploy/.env.example"
guard_fail() { printf '[ERROR] %s\n' "$*" >&2; exit 1; }
declare -A guard_config=() guard_template_keys=()

# Compose control variables must not silently select another stack or profile.
for guard_key in COMPOSE_FILE COMPOSE_FILES COMPOSE_PROFILES COMPOSE_PROJECT_NAME; do
  if [ "${!guard_key+x}" = x ]; then
    guard_fail "继承环境包含 $guard_key；请 unset 后使用包内配置"
  fi
done

[ -r "$GUARD_TEMPLATE" ] || guard_fail '缺少可读的 deploy/.env.example'
while IFS= read -r guard_line || [ -n "$guard_line" ]; do
  if [[ "$guard_line" =~ ^[[:space:]]*\#?[[:space:]]*([A-Za-z_][A-Za-z0-9_]*)= ]]; then
    guard_template_keys["${BASH_REMATCH[1]}"]=1
  fi
done < "$GUARD_TEMPLATE"

guard_has_state=0
shopt -s nullglob dotglob
for guard_name in pg18 garage backend_priv certbot; do
  guard_path="$GUARD_ROOT/deploy/data/$guard_name"
  if [ -e "$guard_path" ] || [ -L "$guard_path" ]; then
    [ -d "$guard_path" ] && [ -r "$guard_path" ] && [ -x "$guard_path" ] \
      || guard_fail "数据路径 data/$guard_name 不可读取或不是目录，请先人工核实"
    guard_entries=("$guard_path"/*)
    [ "${#guard_entries[@]}" -eq 0 ] || guard_has_state=1
  fi
done

if [ -e "$GUARD_ENV" ] || [ -L "$GUARD_ENV" ]; then
  [ -f "$GUARD_ENV" ] && [ -r "$GUARD_ENV" ] || guard_fail 'deploy/.env 不是可读的普通文件'
  guard_lineno=0
  while IFS= read -r guard_line || [ -n "$guard_line" ]; do
    guard_lineno=$((guard_lineno + 1))
    [[ "$guard_line" != *$'\r'* ]] || guard_fail ".env 第 $guard_lineno 行包含 CRLF，请保存为 Unix LF"
    [[ "$guard_line" =~ ^[[:space:]]*$ || "$guard_line" =~ ^[[:space:]]*# ]] && continue
    [[ "$guard_line" =~ ^([A-Za-z_][A-Za-z0-9_]*)=(.*)$ ]] \
      || guard_fail ".env 第 $guard_lineno 行须为无引号 KEY=value"
    guard_key="${BASH_REMATCH[1]}"
    guard_value="${BASH_REMATCH[2]}"
    [ "${guard_config[$guard_key]+present}" != present ] || guard_fail ".env 重复配置键：$guard_key"
    case "$guard_value" in
      *[[:space:]]*|*\'*|*\"*|*\`*|*'$'*|*'\'*|*';'*|*'&'*|*'|'*|*'<'*|*'>'*|*'('*|*')'*|*'{'*|*'}'*|*'~'*)
        guard_fail ".env 的 $guard_key 须为无引号字面值，不含空白或 Shell 插值/控制符" ;;
    esac
    guard_config["$guard_key"]="$guard_value"
  done < "$GUARD_ENV"
fi

# The same controls must not re-enter through .env; monitoring is explicit CLI use.
for guard_key in COMPOSE_FILE COMPOSE_FILES COMPOSE_PROFILES COMPOSE_PROJECT_NAME; do
  [ "${guard_config[$guard_key]+present}" != present ] \
    || guard_fail ".env 内禁止配置 $guard_key；monitoring 请按手册显式运行 Compose"
done

if [ ! -f "$GUARD_ENV" ]; then
  [ "$guard_has_state" -eq 0 ] \
    || guard_fail '已有数据但 deploy/.env 缺失；请恢复原配置，禁止重新生成密钥'
  printf '%s\n' '[INFO] 尚无 deploy/.env；首次安装在 Docker 前置检查后只生成模板并退出。' \
    '[OK] 离线安装保护通过；客户配置尚未完成，未执行部署。'
  exit 0
fi

# Compare inherited settings only once a customer configuration exists.
# An unopened package can be checked before .env is created, even with host TZ.
# Include commented template overrides (such as BACKEND_IMAGE and PG_IMAGE).
for guard_key in "${!guard_template_keys[@]}"; do
  if [ "${!guard_key+x}" = x ]; then
    if [ "${guard_config[$guard_key]+present}" != present ] \
      || [ "${!guard_key}" != "${guard_config[$guard_key]}" ]; then
      guard_fail "继承环境中的 $guard_key 与 deploy/.env 不一致；请 unset 后重试"
    fi
  fi
done

[ "${guard_config[DATA_DIR]:-}" = ./data ] \
  || guard_fail '此交付包要求 DATA_DIR=./data；独立磁盘可挂载到 deploy/data'
[ "${guard_config[BACKEND_PORT]:-}" = 9800 ] \
  || guard_fail '此交付包要求 BACKEND_PORT=9800'
[ "${guard_config[UPTRACE_ENABLED]:-}" = false ] \
  || guard_fail '此交付包要求 UPTRACE_ENABLED=false；启用需专项交付'
[ "${guard_config[IMBOY_EB_DIRECT_ENABLED]:-}" = false ] \
  || guard_fail '此交付包要求 IMBOY_EB_DIRECT_ENABLED=false；启用需专项交付'

if [ "$guard_has_state" -eq 1 ]; then
  for guard_key in POSTGRES_PASSWORD JWT_KEY POSTGRE_AES_KEY ADM_COOKIE_SECRET \
    IMBOY_EDGE_INTERNAL_TOKEN GRAFANA_ADMIN_PASSWORD IMBOY_SOLIDIFIED_KEY \
    IMBOY_SOLIDIFIED_KEY_IV IMBOY_PASSWORD_SALT LIVEKIT_API_KEY LIVEKIT_API_SECRET \
    GARAGE_RPC_SECRET IMBOY_GARAGE_ACCESS_KEY IMBOY_GARAGE_SECRET_KEY \
    IMBOY_DIRECT_CLEANUP_PG_PASSWORD; do
    guard_value="${guard_config[$guard_key]:-}"
    case "${guard_value^^}" in
      ''|*CHANGE_ME*)
        guard_fail "已有数据但 $guard_key 缺失或仍为占位值；请恢复原凭据，不自动重新生成" ;;
    esac
  done
  [ -s "$GUARD_ROOT/deploy/data/backend_priv/keys/login_rsa_priv.pem" ] \
    || guard_fail '已有数据但 RSA 私钥缺失或为空；请恢复原私钥，不自动重新生成'
  printf '%s\n' '[OK] 已有状态保护通过；现有配置与私钥保持原样。'
else
  printf '%s\n' '[OK] 首次安装保护通过；内部密钥占位值交由原安装器初始化。'
fi
